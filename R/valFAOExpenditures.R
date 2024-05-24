#' @title valFAOExpenditures
#' @description uses regression markups to calculate consumer price markups
#' @import tidyr dplyr mrcommons
#' @importFrom magpiesets findset
#' @import madrat
#' @export
#'
#' @return dataframe of FAO FBS
#' @author David M Chen
#' @importFrom readxl read_xlsx
#' @importFrom ggrepel geom_text_repel

valFAOExpenditures <- function(plot = TRUE){
library(mrmarkup)
library(brms)
setConfig(forcecache=T)
kBH <- read.csv(system.file("extdata",mapping="mapMAgPIELEM.csv",
                                           package = "mrmarkup")) %>%
  rename("BHName" = prod)

h12 <- toolGetMapping("h12.csv", type = "regional") %>%
  rename("iso3c" = "CountryCode", "i" = "RegionCode") %>%
  select(iso3c, i)

FAOp <- readFAOPrices(level = "prim", fillPrices = TRUE)
#make 0 prices NA to fill
FAOp[FAOp == 0] <- NA

iso <- getItems(FAOp, dim = 1)
items <- getItems(FAOp, dim = 3)
avg <- new.magpie(cells_and_regions = iso,
                  years = NULL,
                  names = items)
for (i in iso ){
  for (t in items){
    avg[i,,t] <- dimSums(FAOp[i,,t], dim = 2, na.rm = T)/length(which(!is.na(FAOp[i,,t])))

    FAOp[i,,t] <- ifelse(is.na(FAOp[i,,t]), avg[i,,t], FAOp[i,,t])

  }
}

#then fill with regional average
for (i in getNames(FAOp)){
  FAOp[,,i] <- toolFillWithRegionAvg(FAOp[,,i])
}

#then fill with global FAOini price
#
pinit <- calcOutput("IniFoodPrice", aggregate = FALSE, datasource = "FAO")
attr <- calcOutput("Attributes", aggregate = FALSE)
pinit <- pinit/attr[,,"wm"][,,getItems(pinit, dim = 3)] %>%
  collapseNames()
#load("C:/PIK/ICPdata/pinitWM.Rda")
pinit <- add_columns(pinit, addnm = "Vegetables", dim = 3.1, fill = 0)
FAOp <- FAOp[,,"remaining", invert = T]
citems <- intersect(getNames(pinit), getNames(FAOp))

for (i in citems)  {
  FAOp[,,i] <- ifelse(is.na(FAOp[,,i]), pinit[,,i], FAOp[,,i])
}

prpr <- FAOp %>%  as.data.frame(rev = 2) %>%
  rename( "iso3c"= ISO, "year" = Year, "k" = ItemCodeItem, "value" = ".value") %>%
  inner_join(h12)   %>%
  inner_join(kBH) %>%
  rename("magPrice" = "value")

#remove alcohol
prpr <- filter(prpr, k != "alcohol")


##### get markup regression coefs #####


#coefs <- coefs  %>%   
#regressMarkups() %>%
 # rename("BHName" = "prod")

#magCoefs <- kBH  %>%
 # inner_join(coefs)


#
gdppcppp <- calcOutput("GDPpc", aggregate = F, average2020 = FALSE)
#load("C:/PIK/ICPdata/gdppcppp.Rda")

gdppc <- time_interpolate(gdppcppp[,,"gdppc_SSP2"], interpolated_year = c(2010:2030),
                          integrate_interpolated_year = TRUE) %>%
  as.data.frame(rev = 2)  %>%
  rename("gdppc" = ".value")  %>%
  select(iso3c, year, gdppc)

#attr <- calcOutput("Attributes", aggregate = F)[,,"wm"] #convert markup to dm for magpie
#wm <- attr   %>% as.data.frame(rev = 2)  %>%
#  rename("k" = "products", "wm" = ".value")  %>%
#  select(k, wm)

prpr <- filter(prpr, year > 2001)

#split for predicting
pred <- prpr %>%
        select(iso3c, year, BHName) %>%
        distinct()


markupPrC <- inner_join(pred, gdppc)  %>%
               mutate(Cater = "Cater") %>%
          mutate(loggdp = log(gdppc, base = 10),
                 lngdp = log(gdppc), 
                 gdp= gdppc)
markupPrNC <- inner_join(pred, gdppc)  %>%
              mutate(Cater = "noCater")  %>%
          mutate(loggdp = log(gdppc, base = 10),
                 lngdp = log(gdppc), 
                 gdp = gdppc)


load("/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/brmRev1OutConsTighterHigherFilterpop1outliPopWSplit4.Rda")


brmMarkupProdBF <- brmRev1OutConsTighterHigherFilterpop1outliPopWSplit4


#brmMarkupProdBF <- brmMarkupihs

#split into 4 to save RAM
#reverse asinh
markupPrC1 <- cbind(filter(markupPrC, year < 2010), 
                     fitted(brmMarkupProdBF ,filter(markupPrC, year < 2010)))
markupPrC2 <- cbind(filter(markupPrC, year > 2010), 
                     fitted(brmMarkupProdBF ,filter(markupPrC, year > 2010)))
markupPrC <- rbind(markupPrC1, markupPrC2)
rm(markupPrC1, markupPrC2)
gc()

markupPrNC1 <- cbind(filter(markupPrNC, year < 2010), 
                     fitted(brmMarkupProdBF ,filter(markupPrNC, year < 2010)))
markupPrNC2 <- cbind(filter(markupPrNC, year > 2010), 
                     fitted(brmMarkupProdBF ,filter(markupPrNC, year > 2010)))
markupPrNC <- rbind(markupPrNC1, markupPrNC2)
rm(markupPrNC1, markupPrNC2)
gc()
markupPr <- rbind(markupPrC, markupPrNC)  %>%
            inner_join(prpr) %>%
              rename("markup" = Estimate)  %>% 
           mutate(consPrice = magPrice + markup,
                   consPrice025 = magPrice + `Q2.5`,
                   consPrice975 = magPrice + `Q97.5` ) %>%
  select(!c(markup, `Est.Error`, `Q2.5`, `Q97.5`)) %>%
  pivot_longer(names_to = "Prices", cols = c(consPrice, 
                                         consPrice025, consPrice975))  %>% 
  pivot_wider(names_from = c(Cater, Prices), values_from = value) %>% 
  pivot_longer(cols = c(magPrice,Cater_consPrice, Cater_consPrice025, Cater_consPrice975,
                       noCater_consPrice, noCater_consPrice025, noCater_consPrice975),
             names_to = "Price Type", values_to = "Price")


  #pivot_longer(cols = c(magPrice, noCater, cater),
  #            names_to = "Price Type", values_to = "Price")


#  #rename("k" = kcr)  %>%
 # inner_join(magCoefs)  %>%
  ## inner_join(wm) %>%
  #mutate(markup = a*(b^log(gdppc, base = 10)),
  #       # markupCater = markupCater * wm,  #convert to dry matter
  #       consPrice = magPrice + markup) %>%
  #select(!c(a,b, markup)) %>%
  #pivot_wider(names_from = cater, values_from = consPrice) %>%
  #pivot_longer(cols = c(magPrice, noCater, cater),
   #            names_to = "Price Type", values_to = "Price")



#use FAO consumption

consKmag <- readFBSnew(level = "prim")
cons <- consKmag %>%
  as.data.frame(rev=2)  %>%
  rename("foodD" = ".value", "iso3c" = "Area",
         "k" = "prod", "year" = "Year")


plotp <- markupPr %>%
        inner_join(cons) %>%
        group_by(iso3c, year, BHName, `Price Type`) %>%
         summarise(`Average Price` = weighted.mean(Price, w = foodD)/1000)

plotp1 <- separate(plotp, `Price Type`, into = c("Price Type", "ybd"))
plotp1[which(plotp1[,"ybd"] == "consPrice"), "ybd"] <- "y"
plotp1[which(plotp1[,"ybd"] == "consPrice025"), "ybd"] <- "ylo"
plotp1[which(plotp1[,"ybd"] == "consPrice975"), "ybd"] <- "yhi"

plotp1[which(is.na(plotp1[,"ybd"])), "ybd"] <- "y"

plotp1 <- plotp1  %>% pivot_wider(names_from = "ybd", values_from = "Average Price")  %>% 
   mutate(ylo = case_when(
                        is.na(ylo) ~ y,
                        !is.na(ylo) ~ ylo),
          yhi = case_when(
                        is.na(yhi) ~ y,
                         !is.na(yhi) ~ yhi))
                    
plotp1[which(plotp1[,"Price Type"] == "Cater"), "Price Type"] <- "Food-Away-From-Home"
plotp1[which(plotp1[,"Price Type"] == "magPrice"), "Price Type"] <- "Producer"
plotp1[which(plotp1[,"Price Type"] == "noCater"), "Price Type"] <- "Food-At-Home"

plotp1$`Price Type` <- factor(plotp1$`Price Type`, levels = c("Food-Away-From-Home", "Food-At-Home","Producer"))


prices <- ggplot(filter(plotp1, iso3c %in% c("IND", "USA"))) + 
 geom_ribbon(aes(x= year , ymin = ylo, ymax = yhi, fill = `Price Type`), alpha=0.2) +
  geom_line(aes(x = year, y = y, color = `Price Type`), lwd = 1.4) +
  facet_grid( BHName ~ iso3c ,   scales = "free") +
  ggtitle(paste("Food Prices")) +
  labs(x = "Year", y = "USD$17/kg") +
  theme_bw(base_size = 18) +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = ~ axisTicks(., log = FALSE))
prices

ggsave(prices, height = 16, width = 12, 
   filename = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/plots/Prices11.png")

pricesF <- ggplot(filter(plotp1, iso3c %in% c("IND", "USA"))) + 
 geom_ribbon(aes(x= year , ymin = ylo, ymax = yhi, fill = `Price Type`), alpha=0.2) +
  geom_line(aes(x = year, y = y, color = `Price Type`), lwd = 1.4) +
  facet_wrap( BHName ~ iso3c, nrow = 7, scales = "free" ,
              labeller = label_wrap_gen(multi_line=FALSE)) +
  ggtitle(paste("Food Prices")) +
  labs(x = "Year", y = "USD$17/kg") +
  theme_bw(base_size = 24) +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = ~ axisTicks(., log = FALSE))
pricesF


ggsave(pricesF, height = 24, width = 12, filename = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/plots/final/Fig21.png")
ggsave(pricesF, height = 24, width = 12, filename = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/plots/final/Fig21.pdf")

t <- plotp1  %>% select(!c(ylo, yhi))  %>% 
  pivot_wider(names_from = "Price Type", values_from = "y")  %>% 
  mutate(AFHratio = Producer/`Food-Away-From-Home`, AHratio = Producer/`Food-At-Home`)  
filter(t,  year == "2019", iso3c %in% c("IND", "USA"))  %>% arrange(desc(AFHratio))


AFH <- regressFAFH(weight = FALSE, plot = TRUE)  %>% 
select(!gdp)
 
magExp <-  inner_join(cons, markupPr)  %>% 
  inner_join(AFH) %>%      ### from kcal_fafh
  pivot_wider(names_from = `Price Type`,
              values_from = Price) %>% 
  mutate(fahExp = foodD * (1-AFHshr) * (noCater_consPrice),   #FAH
       fahExpL = foodD * (1-AFHshr) * (noCater_consPrice025),
       fahExpH = foodD * (1-AFHshr) * (noCater_consPrice975),
         fafhExp = foodD * AFHshr * (Cater_consPrice),   # FAFH 
        fafhExpL = foodD * (AFHshr) * (Cater_consPrice025),
       fafhExpH = foodD * (AFHshr) * (Cater_consPrice975),
         farmAHexp = foodD *(1-AFHshr) * magPrice, #farm Exp
         farmAFHexp = foodD *AFHshr * magPrice,
         farmAHshr = farmAHexp/fahExp,   #farm share of AH
         farmAHshrL = farmAHexp/fahExpL,
         farmAHshrH = farmAHexp/fahExpH,
         farmAFHshr = farmAFHexp/fafhExp, #farm share of AFH
        farmAFHshrL = farmAFHexp/fafhExpL,
         farmAFHshrH = farmAFHexp/fafhExpH) %>%
  select(iso3c, year, k, foodD, gdppc, fahExp, fahExpL, fahExpH,
                                       fafhExp,  fafhExpL, fafhExpH,
                farmAHexp, farmAFHexp,
                                farmAHshr, farmAHshrL, farmAHshrH,
                                farmAFHshr, farmAFHshrL, farmAFHshrH )


magExpMeanK <- magExp %>%
  group_by(iso3c, year) %>%
  summarise(fahExp = sum(fahExp),
              fahExpL = sum(fahExpL),
              fahExpH = sum(fahExpH),
            fafhExp = sum(fafhExp),
           fafhExpL = sum(fafhExpL),
          fafhExpH = sum(fafhExpH),

           farmAHexp = sum(farmAHexp),
            farmAFHexp = sum(farmAFHexp)) %>%
  mutate(totExp = fahExp + fafhExp,
        totExpL = fahExpL + fafhExpL,
        totExpH = fahExpH + fafhExpH,
         totfarmExp = farmAHexp + farmAFHexp,
         farmShrAH =  farmAHexp / fahExp,  # get farm shares
          farmShrAHL =  farmAHexp / fahExpL, 
 farmShrAHH =  farmAHexp / fahExpH, 
         farmShrAFH = farmAFHexp/fafhExp,
     farmShrAFHL = farmAFHexp/fafhExpL,
          farmShrAFHH = farmAFHexp/fafhExpH,
         farmShrTot = totfarmExp/totExp, 
        farmShrTotL = totfarmExp/totExpL, 
         farmShrTotH = totfarmExp/totExpH) %>%
  mutate(across(c(ends_with(c("Exp", "ExpL", "ExpH"))),  ~ . / !!1e9 ),
         totalFoodExp = fahExp + fafhExp, 
         totalFoodExpL = fahExpL + fafhExpH,
         totalFoodExpH = fahExpL + fafhExpH,
         ) # get total food exp in billions




usda <- NULL
 for (i in as.character(c(2017:2021))) { 
t <- read_xlsx("/p/projects/magpie/users/davidch/ICPdata_cluster/data-on-expenditures-on-food-and-alcoholic-beverages-in-selected-countries.xlsx",
           skip = 5, sheet = i)  %>%
            select(1, 5)  %>% 
            rename( "co" = "...1", "expfahusda"=  "...5")    %>% 
             mutate(year = as.numeric(i))
usda <- rbind(usda, t)
 }
 
usda$iso3c <-  toolCountry2isocode(usda$co) 

# 2021 doesn't exist in wdi conversion, fill those with 2020 
usda <- usda  %>%  filter(!is.na(iso3c))  %>% 
 rename("value" = "expfahusda")  %>% 
 mutate(actualyear = year, 
         year = case_when(
                         year == 2021 ~ 2020,
                        year < 2021 ~ year))  %>% 
GDPuc::convertGDP(unit_in = "current US$MER", 
                   unit_out = "constant 2017 US$MER")   %>% 
select(!year)  %>% 
rename("year" = "actualyear", "expfahusda" = "value")


# make map of farm share 
pop <- calcOutput("Population", aggregate = FALSE)[,,"pop_SSP2"]
pop <- time_interpolate(pop, interpolated_year = unique(magExpMeanK$year), integrate_interpolated_year = TRUE)

mag <- as.magpie(magExpMeanK[,c("iso3c", "year", "farmShrTot")], tidy = TRUE)

magfah <- magExpMeanK  %>% 
          select("iso3c", "year", "fahExp", "fahExpL", "fahExpH")  %>% 
          pivot_longer(c(fahExp, fahExpL, fahExpH), names_to = "bds")  %>% 
          as.magpie(tidy = TRUE)
magfah <- magfah*1000/pop[getRegions(magfah),getYears(magfah),]
magfah <- as.data.frame(magfah, rev = 2)  %>% 
  rename("fah" = ".value")  %>% 
  select(!variable)

usda <- mutate(usda, bds = "fahExp")
usdacomp <- magfah   %>%  full_join(usda)   %>% 
filter(iso3c %in% c(unique(usda$iso3c)))    %>% 
select(!co)  %>% 
rename("Own Calculation" = "fah", "USDA" = "expfahusda")  %>%  
pivot_longer(col = c(`Own Calculation`, USDA), names_to = "FAHExpenditure")   %>%   
pivot_wider(names_from = "bds", values_from = "value")

usdacomp <- filter(usdacomp, !iso3c %in% c("VEN","TWN", "BHR", "QAT", "SGP") )

usdacompP <- ggplot(usdacomp, aes(x = year)) +
   geom_ribbon(aes( ymin = fahExpL, ymax = fahExpH, fill = FAHExpenditure), alpha=0.2) +
geom_line( aes(y = fahExp, color = FAHExpenditure), size = 2 ) +
facet_wrap(~iso3c, nrow = 19, scales = "free") +
theme_bw(base_size = 28) +
  ggtitle("Food-At-Home expenditures per person") +
theme(axis.text=element_text(size=18))+
#        plot.title=element_text(size=24)) +
scale_fill_manual(values = c("#c54a4a",  "#1f4edb"))+
scale_color_manual(values = c("#c54a4a", "#1f4edb"))+
 ylab("USD$17 per person per year")  + 
  expand_limits(y=0)

usdacompP
ggsave(usdacompP, height =  40, width = 24, 
file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/plots/FigS21split.png")
ggsave(usdacompP, height =  40, width = 24, 
file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/plots/FigS21split.pdf")


countries <- c("CHN","IND", "USA", "IDN","PAK","NGA", "BRA", "BGD", "RUS", "MEX", "DEU", "FRA")

usdacompPSub <- ggplot(filter(usdacomp, iso3c %in% countries),
                     aes(x = year)) +
 geom_ribbon(aes( ymin = fahExpL, ymax = fahExpH, fill = FAHExpenditure), alpha=0.2) +
geom_line( aes(y = fahExp, color = FAHExpenditure), lwd = 1.5 ) +
facet_wrap(~iso3c) +
theme_bw(base_size = 30) +
  ggtitle("Food-At-Home expenditures per person") +
theme(axis.text.x=element_text(size=18), 
 legend.title=element_blank())+
#        plot.title=element_text(size=24)) +
scale_fill_manual(values = c("#c54a4a",  "#1f4edb"))+
scale_color_manual(values = c("#c54a4a", "#1f4edb"))+
 ylab("USD$17 per person per year")  + 
   expand_limits(y=0)

usdacompPSub
ggsave(usdacompPSub, height =  12, width = 18, 
file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/plots/Fig3split.png")
ggsave(usdacompPSub, height =  12, width = 18, 
file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/plots/Fig3split.pdf")

library(rworldmap)
library(luplot)

pdf("/p/projects/magpie/users/davidch/ICPdata_cluster/plots/final/Fig4a.pdf", width= 10, height = 7)
map <- plotcountrymap(mag[,2019, ],  numCats = 5,
            catMethod = c(0, 0.2, 0.4, 0.6, 0.8, 1), colourPalette = c('#f1eef6','#bdc9e1','#74a9cf','#2b8cbe','#045a8d'),
            mapTitle = "a) Farm share of Food Dollar 2019 \n FAH + FAFH Food Expenditures", 
            addLegend = FALSE,
            borderCol = "black")
 do.call(addMapLegend, c(map, 
                       legendLabels = "all"))
 dev.off()


#write.csv(magExpMeanKvalid, file="noweightReg_GDPconv.csv")
# 

### FAO value shares instead of Yi 

fao <- readSource("FAO_online", "ValueShares", convert = FALSE)
#china is XCN
chn <- new.magpie(cells_and_regions = "CHN", years = getYears(fao), names = getNames(fao), fill = 0)
chn["CHN",,] <- fao["XCN",,]
fao <- mbind(chn, fao)
fao <- fao["XCN",,invert = T]

# remove totals
fao <- fao[,,"22126|Primary Factors Total", invert = TRUE]
fao <- fao[,,"22117|Total Industries", invert = TRUE]
# remove relative values, total values, keep 2015USD
fao <- collapseNames(fao[,,"Value_US$_2015_prices_(millions)"])


#load gdp and pop
gdp <- calcOutput("GDPpc", aggregate = FALSE)
gdp <- collapseNames(gdppcppp[,,"gdppc_SSP2"]) %>%
  time_interpolate(interpolated_year = getYears(fao), integrate_interpolated_years = TRUE) %>%
  as.data.frame(rev=3) %>% 
  rename("gdp" = ".value", "region" = "iso3c")
pop <- calcOutput("Population", aggregate = FALSE)
pop <- collapseNames(pop[,,"pop_SSP2"]) %>%
   time_interpolate(interpolated_year = getYears(fao), integrate_interpolated_years = TRUE) %>%
  as.data.frame(rev=3)  %>% 
  rename("pop" = ".value", "region" = "iso3c" )


# sum across factors as not so interested there for now
fao <- dimSums(fao, dim = 3.3) %>%
     as.data.frame(rev = 3) %>% 
  rename("value" = ".value",
         "Ind" = "data1",
         "Food" = "data") %>%
  inner_join(pop) %>% 
  inner_join(gdp) 

fafh <- filter(fao, Food ==  "22098|Food and Accomodation Away From Home (FAAFH)") %>%
  filter(!is.na(value))

totfafh2 <- fafh %>% group_by(region, year, Food) %>%
  filter(Ind != "22096|Accommodation and food service activities") %>%
  summarise(total = sum(value))

fshrafh2 <- fafh %>%
  inner_join(totfafh2) %>%
  pivot_wider(names_from = Ind, values_from = value) %>%
  mutate(farmShr = `22097|Agriculture, Forestry and Fishing`/ total )

ggplot(fshrafh2, aes(x=year, y = farmShr)) +
  geom_line() +
  geom_point()+
  facet_wrap(~region) +
  ggtitle("Farm share of FTAH (without accomodation)")
# 
  fshrafh2 <- fshrafh2  %>% 
            rename( "iso3c" = "region", "FAOFarmShr" = "farmShr")  %>% 
            select(iso3c, year, FAOFarmShr)


fah <- filter(fao, Food ==  "22120|Food At Home (FAH)") %>%
  filter(!is.na(value))

totfah <- fah %>% group_by(region, year, Food) %>%
  filter(Ind != "22096|Accommodation and food service activities") %>%
  summarise(total = sum(value))

fah <- fah %>%
  inner_join(totfah) %>%
  pivot_wider(names_from = Ind, values_from = value) %>%
  mutate(farmShr = `22097|Agriculture, Forestry and Fishing`/ total )

ggplot(fah, aes(x=year, y = farmShr)) +
  geom_line() +
  geom_point()+
  facet_wrap(~region) +
  ggtitle("Farm share of FTAH (without accomodation)")

  fah <- fah  %>% 
            rename( "iso3c" = "region", "FAOFarmShr" = "farmShr")  %>% 
            select(iso3c, year, FAOFarmShr)



compFAO4 <-  select(magExpMeanK, iso3c, year, farmShrAH) %>%
  full_join(fshrafh2) %>%
  pivot_longer(cols = c(farmShrAH, FAOFarmShr),
               names_to = "source", values_to = "farmAHShr")  


compFAO4L <-  select(magExpMeanK, iso3c, year, farmShrAHL) %>%
  full_join( fshrafh2) %>%
   rename("farmShrAH" = "farmShrAHL")  %>% 
  pivot_longer(cols = c(farmShrAH, FAOFarmShr),
               names_to = "source", values_to = "farmAHShrL")

compFAO4H <-  select(magExpMeanK, iso3c, year, farmShrAHH) %>%
  full_join( fshrafh2) %>%
         rename( "farmShrAH" = "farmShrAHH")  %>% 
  pivot_longer(cols = c(farmShrAH, FAOFarmShr),
               names_to = "source", values_to = "farmAHShrH")

compFAO4 <- inner_join(compFAO4, compFAO4L)  %>% 
          inner_join(compFAO4H)  %>% 
          filter(iso3c %in% unique(fshrafh2$iso3c),
                   !iso3c %in% c("BRN", "NPL", "SGP"))
                            




compFAO4fah <-  select(magExpMeanK, iso3c, year, farmShrAH) %>%
  full_join(fah) %>%
  pivot_longer(cols = c(farmShrAH, FAOFarmShr),
               names_to = "source", values_to = "farmAHShr")  

compFAO4Lfah <-  select(magExpMeanK, iso3c, year, farmShrAHL) %>%
  full_join( fah) %>%
   rename("farmShrAH" = "farmShrAHL")  %>% 
  pivot_longer(cols = c(farmShrAH, FAOFarmShr),
               names_to = "source", values_to = "farmAHShrL")

compFAO4Hfah <-  select(magExpMeanK, iso3c, year, farmShrAHH) %>%
  full_join( fah) %>%
         rename( "farmShrAH" = "farmShrAHH")  %>% 
  pivot_longer(cols = c(farmShrAH, FAOFarmShr),
               names_to = "source", values_to = "farmAHShrH")

compFAO4fah <- inner_join(compFAO4fah, compFAO4Lfah)  %>% 
          inner_join(compFAO4Hfah)  %>% 
             filter(iso3c %in% unique(fah$iso3c))


compFAO4  %>% filter(iso3c == "DEU")


library(scales)
fig4bSI <- ggplot(compFAO4) +
   geom_ribbon(aes(x = year , ymin = farmAHShrL, ymax = farmAHShrH, fill = source), alpha=0.2) +
  geom_line( aes(x = year, y = farmAHShr, colour = source), size = 1.5)+
  facet_wrap(~iso3c) +
  #  ylim(c(0.15, 0.45)) +
  theme_bw(base_size = 25) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_manual(labels = c("FAO 2022", "Own Calculation" ),
                       values = c( "#1f4edb", "#c54a4a"))+
        scale_color_manual(labels = c("FAO 2022", "Own Calculation" ),
                       values = c( "#1f4edb", "#c54a4a"))+

  ggtitle("Farm Share of FAH Expenditures \n  vs FAO FAAFH - accomodation")
  fig4bSI
ggsave(fig4bSI, height =  17, width = 23, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/final/figS3.png")
ggsave(fig4bSI, height =  17, width = 23, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/final/fig4S3.pdf")

countries
fig4b <- ggplot(filter(compFAO4, iso3c %in% countries)) +
  geom_ribbon(aes(x = year , ymin = farmAHShrL, ymax = farmAHShrH, fill = source), alpha=0.2) +
  geom_line( aes(x = year, y = farmAHShr, colour = source), size = 1.5)+
  facet_wrap(~iso3c) +
  #  ylim(c(0.15, 0.45)) +
  theme_bw(base_size = 25) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_manual(labels = c("FAO 2022", "Own Calculation" ),
                       values = c( "#1f4edb", "#c54a4a"))+
        scale_color_manual(labels = c("FAO 2022", "Own Calculation" ),
                       values = c( "#1f4edb", "#c54a4a"))+

  ggtitle("b) Farm Share of Food-At-Home Expenditures")

fig4b

library(scales)
b <- ggplot(compFAO4fah) +
  geom_point(aes(x = year, y = farmAHShr, colour = source), size = 1.5) +
   geom_ribbon(aes(x = year , ymin = farmAHShrL, ymax = farmAHShrH, fill = source), alpha=0.2) +
  geom_line( aes(x = year, y = farmAHShr, colour = source), size = 1.5)+
  facet_wrap(~iso3c) +
  #  ylim(c(0.15, 0.45)) +
  theme_bw(base_size = 25) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_manual(labels = c("FAO 2022", "Own Calculation" ),
                       values = c( "#1f4edb", "#c54a4a"))+
        scale_color_manual(labels = c("FAO 2022", "Own Calculation" ),
                       values = c( "#1f4edb", "#c54a4a"))+

  ggtitle("b) Farm Share of Food-At-Home Expenditures")
b
countries
aftc <- c("AUS", "BEL", "BRA", "CAN", "CHL","COL","KOR","MEX","SVK", "USA")
aft <- ggplot(filter(compFAO4, iso3c %in% aftc)) +
   geom_ribbon(aes(x = year , ymin = farmAHShrL, ymax = farmAHShrH, fill = source), alpha=0.2) +
  geom_line( aes(x = year, y = farmAHShr, colour = source), size = 1.5)+
  facet_wrap(~iso3c) +
  #  ylim(c(0.15, 0.45)) +
  theme_bw(base_size = 25) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_manual(labels = c("FAO 2022", "Own Calculation" ),
                       values = c( "#1f4edb", "#c54a4a"))+
        scale_color_manual(labels = c("FAO 2022", "Own Calculation" ),
                       values = c( "#1f4edb", "#c54a4a"))+

  ggtitle("b) Farm Share of Food-At-Home Expenditures")



fah2 <- compFAO4fah  %>% filter(source == "FAOFarmShr", iso3c == "USA")  %>% 
mutate(source = paste0(source, "fah"))   


compFAO42 <- rbind(compFAO4, fah2)

fig4b <- ggplot(filter(compFAO42, iso3c %in% countries)) +
   geom_ribbon(aes(x = year , ymin = farmAHShrH, ymax = farmAHShrL, fill = source), alpha=0.2) +
  geom_line( aes(x = year, y = farmAHShr, colour = source), size = 1.5)+
  geom_point(aes(x=year, y = farmAHShr, colour = source))+
  facet_wrap(~iso3c) +
  #  ylim(c(0.15, 0.45)) +
  theme_bw(base_size = 30) +
  theme(axis.text.x=element_text(size=20))+
  labs(color="Data source", fill = "Data source")  +
  scale_x_continuous(breaks = c(2005,2010,2015,2019)) +
  scale_fill_manual(labels = c("FAO 2022 FAAFH", "FAO 2022 FAH",  "Own Calculation FAH" ),
                       values = c( "#1f4edb","#0d9a0d", "#c54a4a"))+
        scale_color_manual(labels = c("FAO 2022 FAAFH","FAO 2022 FAH",  "Own Calculation FAH" ),
                       values = c( "#1f4edb", "#0d9a0d", "#c54a4a"))+
lims(y = c(0,0.69)) +

  ggtitle("b) Farm Share of Food Expenditures")

fig4b
ggsave(fig4b, height =  15, width = 22, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/final/fig4b.png")
ggsave(fig4b, height =  15, width = 22, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/final/fig4b.pdf")


## farm shares by product
### global price plot
gdppc <- calcOutput("GDPpc", aggregate = FALSE)[,,"gdppc_SSP2"]  %>% 
        as.data.frame(rev = 2)  %>% 
        rename("gdppc" = ".value")
iG <- gdppc %>% filter(year == 2020) %>%
  mutate(incomeG =  case_when(
    gdppc <= 1006 ~ "LIC",
    gdppc > 1006 & gdppc <= 3956 ~ "LMIC",
    gdppc > 3956 & gdppc <= 12235 ~ "UMIC",
    gdppc > 12235 ~ "HIC")) %>%
  select(iso3c, incomeG)
iG$incomeG <- factor(iG$incomeG, levels = c("HIC", "UMIC","LMIC", "LIC"))


magExpP <- magExp  %>% 
          inner_join(kBH)  %>% 
           inner_join( iG)  %>% 
           group_by(year, BHName, incomeG)  %>% 
      summarise(fahExp = sum(fahExp),
               fafhExp = sum(fafhExp),
               fahExpL = sum(fahExpL),
               fafhExpL = sum(fafhExpL),
               fahExpH = sum(fahExpH),
               fafhExpH = sum(fafhExpH),
                farmAHexp = sum(farmAHexp),
               farmAFHexp = sum(farmAFHexp),
               )  %>% 
     mutate(totExp = fahExp + fafhExp,
            totExpL = fahExpL + fafhExpL,
            totExpH = fahExpH + fafhExpH,
            totfarmExp = farmAHexp + farmAFHexp,
            farmShrTot = totfarmExp/totExp, 
            farmShrTotL = totfarmExp/totExpL, 
            farmShrTotH = totfarmExp/totExpH)  %>% 
    select(year, BHName, incomeG, farmShrTotL, farmShrTot, farmShrTotH)  

figS4 <- ggplot(magExpP, aes(x=year)) +
  geom_line(aes(y= farmShrTot), color = "#135fa5", size = 1.5) +
  geom_ribbon(aes(ymin = farmShrTotL, ymax = farmShrTotH), fill = "#0084ff", alpha = 0.5) +
  facet_grid(incomeG ~ BHName) +
  theme_bw(base_size = 30) + 
    scale_x_continuous(breaks = c(2005,2010,2015)) +
  ylab("Farm Share, Total Expenditures") + 
  ggtitle("Farm Shares by Product across countries aggregated by income level")

figS4
ggsave(figS4, height =  18, width = 24, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/final/figS5.png")
ggsave(figS4, height =  18, width = 24, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/final/figS5.pdf")

filter(magExpP, year == 2019)




head(compFAO4)
compyi3  %>% filter(source == "USDAfarmShrTot") 

yi4 <-  read_xlsx(system.file("extdata",mapping="YiSourceFig4.xlsx",
                                    package = "mrmarkup"), skip = 1) %>%
  pivot_longer(cols = c(2:last_col()), names_to = "year", values_to = "YifarmAHshr") %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year)) %>%
  group_by(Country, year) %>%
  summarise(YifarmAHshr = mean(YifarmAHshr, na.rm =T)) %>%
  ungroup()
yi4$iso3c <- toolCountry2isocode(yi4$Country, mapping = c("Korea, Rep." = "KOR") )

compyi4 <-  select(magExpMeanK, iso3c, year, farmShrAH) %>%
  inner_join( select(yi4, iso3c, year, YifarmAHshr)) %>%
  pivot_longer(cols = c(farmShrAH, YifarmAHshr),
               names_to = "source", values_to = "farmAHShr")


compyi4L <-  select(magExpMeanK, iso3c, year, farmShrAHL) %>%
  inner_join( select(yi4, iso3c, year, YifarmAHshr)) %>%
   rename("farmShrAH" = "farmShrAHL")  %>% 
  pivot_longer(cols = c(farmShrAH, YifarmAHshr),
               names_to = "source", values_to = "farmAHShrL")

compyi4H <-  select(magExpMeanK, iso3c, year, farmShrAHH) %>%
  inner_join( select(yi4, iso3c, year, YifarmAHshr)) %>%
         rename( "farmShrAH" = "farmShrAHH")  %>% 
  pivot_longer(cols = c(farmShrAH, YifarmAHshr),
               names_to = "source", values_to = "farmAHShrH")

compyi4 <- inner_join(compyi4, compyi4L)  %>% 
          inner_join(compyi4H)

library(scales)
a <- ggplot(compyi4) +
   geom_ribbon(aes(x = year , ymin = farmAHShrL, ymax = farmAHShrH, fill = source), alpha=0.2) +
  geom_line( aes(x = year, y = farmAHShr, colour = source), size = 1.5)+
  facet_wrap(~iso3c) +
  #  ylim(c(0.15, 0.45)) +
  theme_bw(base_size = 25) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_manual(labels = c("Own Calculation", "Yi et al. 2021" ),
                       values = c("#c54a4a",  "#1f4edb"))+
        scale_color_manual(labels = c("Own Calculation", "Yi et al. 2021" ),
                       values = c("#c54a4a", "#1f4edb"))+
  
  ggtitle("b) Farm Share of Food-At-Home Expenditures")
a
ggsave(a, height =  12, width = 19, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/FigS4.png")
ggsave(a, height =  12, width = 19, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/FigS4.pdf")


yi3 <-  read_xlsx(system.file("extdata",mapping="YiSourceFig3.xlsx",
                              package = "mrmarkup"), skip = 4) %>%
  rename("year" = Year, "USDAfarmShrTot" = `Farm value share of expenditures`,
         "YifarmShrTot" = `New farm share series`,
  ) %>%
  select(year, USDAfarmShrTot, YifarmShrTot) %>%
  filter(!is.na(YifarmShrTot))
yi3$USDAfarmShrTot[is.na(yi3$USDAfarmShrTot)]  <- 0

yi3 <- yi3 %>%
  mutate(year = as.numeric(year),
         USDAfarmShrTot = as.numeric(USDAfarmShrTot),
         YifarmShrTot = YifarmShrTot/100,
         USDAfarmShrTot = USDAfarmShrTot/100)

yi3[which(yi3$USDAfarmShrTot==0),"USDAfarmShrTot"] <- NA

compyi3 <- filter(magExpMeanK, iso3c == "USA") %>%
  select(year, farmShrTot) %>%
  right_join(yi3) %>%
  pivot_longer(cols = c(farmShrTot, USDAfarmShrTot, YifarmShrTot),
               names_to = "source", values_to = "farmShr")


compyi3L <- filter(magExpMeanK, iso3c == "USA") %>%
  select(year, farmShrTotL) %>%
  right_join(yi3) %>%
  rename( "farmShrTot" = "farmShrTotL")  %>% 
pivot_longer(cols = c(farmShrTot, USDAfarmShrTot, YifarmShrTot),
               names_to = "source", values_to = "farmShrL")


compyi3H <- filter(magExpMeanK, iso3c == "USA") %>%
  select(year, farmShrTotH) %>%
  right_join(yi3) %>% 
    rename( "farmShrTot" = "farmShrTotH")  %>% 
  pivot_longer(cols = c(farmShrTot, USDAfarmShrTot, YifarmShrTot),
               names_to = "source", values_to = "farmShrH")

compyi3 <- inner_join(compyi3, compyi3L)  %>% 
          inner_join(compyi3H)

b <- ggplot(compyi3) +
   geom_ribbon(aes(x= year , ymin = farmShrL, ymax = farmShrH, fill = source), alpha=0.2) +
  geom_line( aes(x = year, y = farmShr, colour = source), size = 2)+
  #ylim(c(0.10, 0.30)) +
  theme_bw(base_size = 30) +
    scale_fill_manual(labels = c("Own Calculation", "USDA", "Yi et al. 2021" ),
                       values = c("#c54a4a", "#68bb7b", "#1f4edb"))+
        scale_color_manual(labels = c("Own Calculation", "USDA", "Yi et al. 2021" ),
                       values = c("#c54a4a", "#68bb7b", "#1f4edb"))+
  ggtitle("Farm share of US food expenditures")
b



ggsave(b, height =  12, width = 19, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/final/FigS4.png")
ggsave(b, height =  12, width = 19, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/final/FigS4.pdf")



  library(gridExtra)
out <- grid.arrange(b,a)
ggsave(out, height =  24, width = 19, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/Fig4YiVal.png")
ggsave(out, height =  24, width = 19, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/Fig4YiVal.pdf")

}

