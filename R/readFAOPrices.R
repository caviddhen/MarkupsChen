#' @title readFAOPrices
#' @description read gets FAO food expenditures
#' @param fillPrices true tries to fill prices with 5 year average around the missing value
#' @param level k or prim
#' @import tidyr dplyr mrcommons madrat
#' @importFrom magpiesets findset
#' @export
#'
#' @return dataframe of FAO expenditures
#' @author David M Chen

readFAOPrices <- function(fillPrices = T, level = "k") {

FAOmap <-  read.csv(system.file("extdata",mapping="FAOitems_online.csv",
                            package = "MarkupsChen"))


prpr <- readSource("FAO_online", "PricesProducerAnnual",
                   convert = TRUE)

#convert from 2005 constant US$ to 2017 US$ MER based on Consumer PRice INdex
prpr <- prpr * 1.23  # convert with CPI

#production weight from ProductionItem to FoodBalance Item
crop <- collapseNames(readSource("FAO_online", "Crop", convert = TRUE)[,,"production"])
live <- collapseNames(readSource("FAO_online", "LivePrim", convert = TRUE)[,,"production"])
weight <- mbind(crop, live)

#make sure matching years and items
citems <- intersect(getNames(prpr), getNames(weight))
cyears <- intersect(getYears(prpr), getYears(weight))
cregions <- intersect(getRegions(prpr), getRegions(weight))

weight <- weight[cregions,cyears, citems][,,"1166|Meat nes", inv = TRUE]
prpr <- prpr[cregions,cyears,citems][,,"1166|Meat nes", inv = TRUE]

FAOmap1 <- FAOmap[which(FAOmap$ProductionItem %in% getNames(weight)),]
prpr_fbs <- toolAggregate(prpr, rel = FAOmap1, weight = weight,
                          from = "ProductionItem", to = "FoodBalanceItem", partrel = FALSE,
                          dim = 3.1, wdim = 3.1 )

#map to magpie
kmapping <- read.csv(system.file("extdata",mapping="newFAOitems_online_DRAFT.csv",
                     package = "MarkupsChen"))

#consumption weight from FoodBalanceItem to Large Exp Grouping, but some values dropped, oilpalm gets dropped
hr <- calcOutput("FAOharmonized", aggregate = FALSE)
#load("C:/PIK/ICPdata/hr.Rda")

hr <- dimSums(hr[,,c("food", "processed")], dim = 3.2, na.rm = T)

citems <- intersect(getNames(prpr_fbs), getNames(hr, dim = 1))
pryears <-  intersect(getYears(prpr_fbs), getYears(hr))

rm <- c(setdiff(citems, kmapping[which(kmapping$FoodBalanceItem %in% citems),"FoodBalanceItem"]),
        "2633|Cocoa Beans and products",  "2630|Coffee and products", "2635|Tea (including mate)")

prprk<- toolAggregate(prpr_fbs[,,citems][,,rm, inv = T],  rel = kmapping,
                      from = "FoodBalanceItem", to = "k_ICP",
                      weight = setYears(hr[,2013,citems][,,rm, inv = T],NULL), dim = 3, wdim = 3,  #weight is 2013 consumption as FAOharmonized doesn't have all years
                      partrel = TRUE )

if (fillPrices) {

prprk[prprk==0] <- NA

#try to fill some missing prices with the 5 year average, any exists

for(i in getRegions(prprk)){
  for(j in getNames(prprk)){
    prprk[i,2011,j] <- ifelse(is.na(prprk[i,2011,j]),
                              mean(prprk[i,c(2009,2010,2012,2013),j], na.rm=T),
                              prprk[i,2011,j])
  }}


for(i in getRegions(prprk)){
  for(j in getNames(prprk)){
    prprk[i,2017,j] <- ifelse(is.na(prprk[i,2017,j]),
                              mean(prprk[i,c(2015,2016,2018,2019),j], na.rm=T),
                              prprk[i,2017,j])
  }}

prprk[is.na(prprk)] <- 0
}


FAOp <- prprk

if (level == "prim"){

  proc <- readFBSnew(return = "proc")
  proc_sugar <- proc[["proc_sugar"]]
  proc_oils <- proc[["proc_oils"]]

  prpr_sug <- new.magpie(cells_and_regions = getRegions(proc_sugar), years = getYears(proc_sugar),
                         fill=NA, names = "sugar")

  proc_sugar <- collapseNames(proc_sugar)
  citems <- intersect(getNames(proc_sugar), getNames(prprk))

  for(i in getRegions(proc_sugar)) {
    for (yr in getYears(proc_sugar)){
      prpr_sug[i,yr,] <- weighted.mean(prprk[i,yr,citems],  w =  collapseNames(proc_sugar[i,yr,citems]))
    }}


  prpr_oils <- new.magpie(cells_and_regions = getRegions(proc_oils), years = getYears(proc_oils),
                          fill=NA, names = "oils")

  proc_oils <- collapseNames(proc_oils)
  citems <- intersect(getNames(proc_oils), getNames(prprk))

  for(i in getRegions(proc_oils)) {
    for (yr in getYears(proc_oils)){
      prpr_oils[i,yr,] <- weighted.mean(prprk[i,yr,citems],  w =  collapseNames(proc_oils[i,yr,citems]))
    }}

  prpr_oils["USA",,]

  #extend same price into the past for sugar and oils

  diffyears <- setdiff(getYears(FAOp), getYears(prpr_oils))
  procMag <- mbind(prpr_sug, prpr_oils)
  fillpast <- new.magpie(cells_and_regions = getRegions(procMag),
                         years = diffyears,
                         names = getNames(procMag),
                         fill = NA)
  fillpast[,,"sugar"] <- setYears(procMag[,min(getYears(procMag, as.integer = T)),"sugar"],NULL)
  fillpast[,,"oils"]  <- setYears(procMag[,min(getYears(procMag, as.integer = T)),"oils"],NULL)
  procMag <- mbind(fillpast, procMag)

  FAOpmag <- FAOp[getRegions(prpr_oils),, ][,,"sugar", inv =T]
  FAOp <- mbind(FAOpmag, procMag)

}

return(FAOp)

}

