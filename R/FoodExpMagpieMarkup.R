#' @title FoodExpMagpieMarkup
#' @description calculate markups
#' @param level reg or regglo
#' @param type "consumer" (marked up) or "producer" price output
#' @param gdx gdx file as input for mag base prices
#' @param afterShock TRUE or FALSE for kcal consumed
#' @param prodAggr TRUE for aggregating across alll products
#' @import dplyr tidyr
#' @importFrom magclass as.data.frame
#' @importFrom magpiesets findset
#' @importFrom gdx readGDX
#' @import brms
#' @export
#'
#' @return dataframe or magclass object of consumer food expenditures
#' @author David M Chen

FoodExpMagpieMarkup <- function(gdx, level = "reg", type = "consumer", prodAggr = FALSE, afterShock = FALSE) {


kfo <- findset("kfo")
kBH <- read.csv(system.file("extdata",mapping="mapMAgPIELEM.csv",
                              package = "MarkupsChen"))  %>%
  rename("BHName" = prod)

mapping <- readGDX(gdx = gdx, "i_to_iso")  %>%
           rename("iso3c" = "iso")


attr <- calcOutput("Attributes", aggregate = F)[,,"wm"] # t wm / t dm convert prices from dm to wm for markup
wm <- attr   %>% magclass::as.data.frame(rev = 2)  %>%
  rename("k" = "products", "wm" = ".value")  %>%
  select(k, wm)

nutr <- readGDX(gdx, "f15_nutrition_attributes")[,,"kcal"]  #mio kcal / t DM convert prices from kcal to dm

prpr <- FoodDemandModuleConsumerPrices(gdx) # $/kcal
prpr <- collapseNames((prpr / attr[,,"wm"][,,getItems(prpr, dim = 3)] * nutr[,getYears(prpr),getItems(prpr, dim = 3)] * 1e6)) # prpr in $/twmM
prpr <- time_interpolate(prpr, interpolated_year = c(2010:2017), integrate_interpolated_years = TRUE)
prpr <- add_columns(prpr, addnm = "Vegetables", dim = 3.1, fill = NA)
prpr[,,"Vegetables"] <- prpr[,,"others"]

### replace price for oils and sugars (which already processed) with weighted average price of
#primary products that go into the secondary products
ksd <- findset("ksd")

#cons <- demand(gdx)
#cons <- gdxAggregate(gdx = gdx, cons, weight = 'Intake', to = "iso")[,,"food"]
#cons <- collapseNames(cons)
#kfo <- c(findset("kfo"), "Vegetables")
#cons <- add_columns(collapseNames(cons), addnm = "Vegetables")
#load input vegetable data to get a the latest split

consKmag <- readFBSnew(level = "prim")

cons <- Kcal(gdx = gdx, level = "iso",
           calibrated = TRUE, after_shock = afterShock,
           products = "kfo", product_aggr = FALSE,
           per_capita = FALSE) * 365  # This is in MILLION KCAL!

cons <- collapseNames(cons)
kfo <- c(findset("kfo"), "Vegetables")
cons <- add_columns(collapseNames(cons), addnm = "Vegetables")
#load input vegetable data to get a the latest split

VegShr <- consKmag[,,"Vegetables"]/collapseNames((consKmag[,,"others"]+consKmag[,,"Vegetables"]))
VegShr <- toolCountryFill(VegShr, fill = 0.58) #mean value
othShr <- (1-VegShr)
cons[,,"Vegetables"] <- setYears(VegShr[,2019,], NULL) * cons[,,"others"]
cons[,,"others"] <- setYears(othShr[,2019,], NULL) * cons[,,"others"]


attr <- add_columns(attr, addnm = "Vegetables", dim = 3.2)
attr[,,"Vegetables"] <- attr[,,"others"]
#cons <- cons * attr[,,"wm"][,,getItems(cons, dim =3)] %>%
#  collapseNames()

proc_shr <- calcOutput("Processing_shares", aggregate = F)
proc_shr <- time_interpolate(proc_shr[getRegions(prpr),,], interpolated_year = getYears(prpr),
                             integrate_interpolated_years = T)
proc_shr <- proc_shr[,getYears(cons),]

cvn_fct <- calcOutput("Processing_conversion_factors", aggregate = F)
cvn_fct <- time_interpolate(cvn_fct, interpolated_year = getYears(prpr),
                            integrate_interpolated_years = T)[,getYears(prpr),]
cvn_fct <- cvn_fct[,getYears(cons),]

proc <- cons[,,intersect(ksd, getNames(cons, dim = 1))][,,"alcohol", inv = T]
proc_oils <- collapseNames((proc[,,"oils"] /
                              dimSums(cvn_fct[,,c("milling", "extracting")][,,"oils"],dim=3.1) *
                              proc_shr[,,"oils"] ))
proc_oils[is.na(proc_oils)] <- 0
proc_oils <- time_interpolate(proc_oils, interpolated_year = getYears(prpr),
                               integrate_interpolated_years = T)[,getYears(prpr),]


proc_sugar <- collapseNames((proc[,,"sugar"] /
                               dimSums(cvn_fct[,,"refining"][,,list("ItemCodeItem" = "sugar")],dim=3.1) *
                               proc_shr[,,list("ItemCodeItem" = "sugar")] ))
proc_sugar[is.na(proc_sugar)] <- 0
proc_sugar <- time_interpolate(proc_sugar, interpolated_year = getYears(prpr),
                               integrate_interpolated_years = T)[,getYears(prpr),]

citems <- intersect(getNames(proc_sugar, dim =1), getNames(prpr, dim =1))

sugmap <- data.frame(sugar = rep("sugar", length(citems)), pr = citems)
prpr_sug <- toolAggregate(prpr[,,citems], rel = sugmap,
                   from = "pr", to = "sugar",
                   weight = proc_sugar[,,citems],
                   dim = 3.1)

citems <- intersect(getNames(proc_oils, dim =1), getNames(prpr, dim = 1))
oilmap <- data.frame(oils = rep("oils", length(citems)), pr = citems)
prpr_oils <- toolAggregate(prpr[,,citems], rel = oilmap,
                          from = "pr", to = "oils",
                          weight = proc_oils[,,citems],
                          dim = 3.1)

prpr[,,"sugar"] <- prpr_sug
prpr[,,"oils"] <- prpr_oils

prpr<- prpr[,,kfo]


prpr <- prpr %>%
  collapseNames() %>%
    as.data.frame(rev = 2)  %>%
  rename("iso3c" = "iso", "year" = t, "k" = kfo, "value" = ".value") %>%
  inner_join(kBH)

##### get markup regression coefs #####


brmMarkupProdBF <-regressMarkups()

#remove alcohol
prpr <- filter(prpr, k != "alcohol")

#magCoefs <- kBH  %>%
#  rename("prod" = "BHName")  %>%
#    inner_join(coefs)

gdppc <- income(gdx, level = "iso")

gdppc <- gdppc %>%
  as.data.frame(rev = 2)  %>%
  rename("iso3c" = "iso", "year" = "t_all", "gdppc" = ".value") %>%
  select(iso3c, year, gdppc)

nutr <- as.data.frame(nutr, rev= 2)  %>%
  rename("year" = "t_all", "k" = kall, "kcal" = ".value") %>%
  select(year,k, kcal)

attr <- as.data.frame(attr, rev = 2)  %>%
      rename( "k" = "products", "wm" = ".value") %>%
      select(k, wm)


#split for memory
prpr1 <- prpr  %>% mutate(Cater = "Cater")   %>%
                 inner_join(gdppc)  %>%
                 mutate(lngdp = log(gdppc),
                         loggdp = log(gdppc, base = 10))  %>%
     select(iso3c, year, BHName, Cater, gdppc, lngdp, loggdp)  %>%
      distinct()

fit1 <- fitted(brmMarkupProdBF, prpr1)
prpr1 <- cbind(prpr1, fit1)

prpr2 <- prpr  %>% mutate(Cater = "noCater")   %>%
                 inner_join(gdppc)  %>%
                 mutate(lngdp = log(gdppc),
                         loggdp = log(gdppc, base = 10))  %>%
     select(iso3c, year, BHName, Cater, gdppc, lngdp, loggdp)  %>%
      distinct()
fit2 <- fitted(brmMarkupProdBF, prpr2)
prpr2 <- cbind(prpr2, fit2)

prpr_fits <- rbind(prpr1, prpr2)

prpr <- inner_join(prpr, prpr_fits)  %>%
               GDPuc::convertGDP(unit_in = "constant 2005 US$MER",
                   unit_out = "constant 2017 US$MER",
                   replace_NAs = "no_conversion")  %>%
        rename("prodPrice" = value)

markupPr <- prpr  %>%
                   mutate(consPrice = prodPrice + Estimate)  %>%
   select(iso3c, year, k, prodPrice, BHName,  Cater, gdppc, prodPrice, consPrice)  %>%
             pivot_wider(names_from = Cater, values_from = consPrice)    %>%
  rename( "caterPrice" = Cater, "noCaterPrice" = noCater)

markups <-  markupPr %>%
  pivot_longer(cols = c(prodPrice, noCaterPrice, caterPrice),
               names_to = "Price Type", values_to = "Price") %>%
  select(iso3c, year, k,  `Price Type`, Price)


cons <- cons[,,kfo] %>% #from FoodExpMagpieMarkup
  as.data.frame(rev=2)  %>%
  rename("foodD" = ".value", "iso3c" = "iso",
         "k" = "kfo", "year" = "t")

AFHshr <- regressFAFH()

magExp <- inner_join(cons,
                     markups)  %>%
  inner_join(AFHshr) %>%      ### from kcal_fafh
  pivot_wider(names_from =  c(`Price Type`),
              values_from = Price) %>%
  mutate(foodD = foodD * 1e6, #convert to kcal from Mio. kcal
         fahExp = foodD * (1-AFHshr) * (noCaterPrice),
         fafhExp = foodD * AFHshr * (caterPrice),
         farmAHexp = foodD *(1-AFHshr) * prodPrice,
         farmAFHexp = foodD *AFHshr * prodPrice,
         farmAHshr = farmAHexp/fahExp,
         farmAFHshr = farmAFHexp/fafhExp)%>%
    mutate(across(c(ends_with("Exp")),  ~ . / !!1e9 ),
         totalFoodExp = fahExp + fafhExp)  %>%  # get total food exp in billions
  select(iso3c, year, k, foodD, gdp, prodPrice, caterPrice, noCaterPrice, fahExp, fafhExp, totalFoodExp, farmAHexp, farmAFHexp, farmAHshr, farmAFHshr)

magExp[is.na(magExp)] <- 0

if(prodAggr) {
magExp<- magExp %>%
  group_by(iso3c, year) %>%
  summarise(fahExp = sum(fahExp),
            fafhExp = sum(fafhExp),
            farmAHexp = sum(farmAHexp),
            farmAFHexp = sum(farmAFHexp),
            totExp = sum(totalFoodExp)) %>%
  mutate(totfarmExp = farmAHexp + farmAFHexp,
         farmAHshr =  farmAHexp / fahExp,  # get farm shares
         farmAFHshr = farmAFHexp/fafhExp,
         farmShrTot = totfarmExp/totExp)
}

out <- magExp


return(out)
}

