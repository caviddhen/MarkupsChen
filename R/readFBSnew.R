#' @title readFBSnew
#' @description read gets FAO food expenditures
#' @param level k prim or LEM
#' @return either return the regular FBS, or the proc_sugar and proc_oil datasheets
#' (important for weighting other functions)
#' @import tidyr dplyr madrat
#' @importFrom magpiesets findset
#' @import mrland
#' @export
#'
#' @return dataframe of FAO FBS
#' @author David M Chen

readFBSnew <- function(level = "k", return = "FBS"){

### csv is FAOSTAT Food Balance sheets Food  and Processing demand
fb <-  read.csv(system.file("extdata",mapping="FAOSTAT_FB.csv",
                            package = "MarkupsChen"))  %>%
  select(Area, Element, Item.Code, Item, Year, Value)   %>%
  #filter(Year %in% c(2011, 2017))  %>%
  unite(col = "prod", c(Item.Code, Item), sep = "|")  %>%
  relocate(Year, .after = Area)  %>%
  relocate(Element, .after = prod)  %>%
  mutate(Value = Value * 1000) # tons

 fb$Area <- toolCountry2isocode(fb$Area, mapping = c("T�rkiye" = "TUR",
                                                     "Türkiye" = "TUR",
                                                     "Côte d'Ivoire" = "CIV"))

fb <- filter(fb, !is.na(Area))



hr1 <-as.magpie(fb, spatial = 1, temporal = 2, tidy = T)
hr1[is.na(hr1)] <- 0

LEMmapping <- read.csv(system.file("extdata",mapping="newConsLEMmapping.csv",
                     package = "MarkupsChen"))

kmapping <- read.csv(system.file("extdata",mapping="newFAOitems_online_DRAFT.csv",
                     package = "MarkupsChen"))

kpr <- c(findset("kpr"), "Vegetables")
ksd <- findset("ksd")
kli <- findset("kli")

####get primary product demand of processed goods by aggregating processed
   #items to k then calculating the primary product requirement from them ##

#do cream and butter conversion factors manually
hr1[,,"2740|Butter, Ghee"] <- hr1[,,"2740|Butter, Ghee"] * 25
hr1[,,"2743|Cream"] <- hr1[,,"2743|Cream"] * 10
#add processed milk demand also to food for cheese
hr1[,,c("2848|Milk - Excluding Butter","2743|Cream")][,,"Food"] <- hr1[,,c("2848|Milk - Excluding Butter", "2743|Cream")][,,"Food"] +
  hr1[,,c("2848|Milk - Excluding Butter", "2743|Cream")][,,"Processing"]

hrk <-  toolAggregate(collapseNames(hr1[,,"Food"]), rel = kmapping,
                      from = "newFoodBalanceItem",
                      to = "k_ICP", partrel = T, dim = 3)

proc_shr <- calcOutput("Processing_shares", aggregate = F)
#load("C:/PIK/ICPdata/proc_shr.Rda")
proc_shr <- time_interpolate(proc_shr[getRegions(hrk),,], interpolated_year = getYears(hrk), integrate_interpolated_years = T)[,getYears(hrk),]
cvn_fct <- calcOutput("Processing_conversion_factors", aggregate = F)
#load("C:/PIK/ICPdata/cvn_fct.Rda")
cvn_fct <- time_interpolate(cvn_fct, interpolated_year = getYears(hrk), integrate_interpolated_years = T)[,getYears(hrk),]

food <- hrk[,,c(kli,intersect(kpr, getNames(hrk)))]
food <- add_dimension(food, dim = 3.2, nm = "food")

proc <- hrk[,,intersect(ksd, getNames(hrk))][,,"alcohol", inv = T]
proc_oils <- collapseNames(proc[,,"oils"] /
                             cvn_fct[,,c("milling", "extracting")][,,"oils"]*
                             proc_shr[,,"oils"] )
proc_oils[is.na(proc_oils)] <- 0
proc_oils[is.infinite(proc_oils)] <- 0

proc_oils <- dimSums(proc_oils, dim = 3.1, na.rm=T)

proc_oils <- add_dimension(proc_oils, dim = 3.2, nm = "processed")

proc_sugar <- collapseNames((proc[,,"sugar"] /
                               cvn_fct[,,"refining"][,,list("ItemCodeItem" = "sugar")] *
                               proc_shr[,,list("ItemCodeItem" = "sugar")] ))
proc_sugar[is.na(proc_sugar)] <- 0
proc_sugar[is.infinite(proc_sugar)]
proc_sugar <- add_dimension(proc_sugar, dim = 3.2, nm = "processed")

proc <- mbind(proc_oils, proc_sugar)

consK <- mbind(food,
               proc)

# #add processed demand in Mt dry matter primary commodity
# consKmag <- collapseNames(consK[,,"food"])
#
# consKmag[,,"sugar"] <- dimSums(proc_sugar, dim = 3)
#
# oils <- dimSums(proc_oils, dim = 3)
# getNames(oils) <- "oils"
# consKmag <- mbind(consKmag, oils)


if (level == "LEM"){

magMapping <-  read.csv(system.file("extdata",mapping="mapMAgPIELEM.csv",
                                      package = "MarkupsChen"))

#remove sugar
consK <- consK[,,"sugar", inv = T]
consWf <- toolAggregate(collapseNames(consK[,,"food"]), rel = magMapping, from = "k", to = "prod",
                        dim = 3.1, partrel = TRUE)
consWp <- dimSums(consK[,,"processed"], dim = 3)
getNames(consWp) <- "Processed"
consW <- mbind(consWf, consWp)
out <- consW

} else if (level == "prim"){

#add processed demand as sum of primary demand
consKmag <- collapseNames(consK[,,"food"])

consKmag[,,"sugar"] <-dimSums(proc_sugar, dim = 3)

oils <- dimSums(proc_oils, dim = 3)
getNames(oils) <- "oils"
consKmag <- mbind(consKmag, oils)
out <- consKmag
} else if (level == "k") {

out <- consK
}


if(return == "FBS"){
 return(out) } else if (return == "proc"){
  return(list("proc_sugar" = proc_sugar, "proc_oils" = proc_oils))
}


}


