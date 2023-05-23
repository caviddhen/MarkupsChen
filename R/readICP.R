#' @title readICP
#' @description read ICP data
#' @import dplyr tidyr rlang
#' @export
#'
#' @return dataframe of ICP expenditures
#' @author David M Chen
#' @importFrom readxlsb read_xlsb

readICP <- function() {

  exp <- readSource("ICP2017raw", convert = FALSE)



  #mapping of k to BH aggregate category to even broader cat
  kBHmap <- read.csv(system.file("extdata",mapping="MappingKBH.csv",
               package = "MarkupsChen"))

  colnames(exp)[1:5] <- exp[1, c(1:5)]
  colnames(exp)[6] <- "ClassificationCode"

  exp <- exp %>%
    filter(!row_number() == 1) %>%
    pivot_longer(cols = 7:ncol(exp), names_to = "iso3c", values_to = "LCU") %>%
    rename("BHCode" = `Item Code`, "BHName" = `Item Name`) %>%
    select(BHCode, BHName, iso3c, ClassificationCode, LCU)

  exchanges <- readICPexchangeRates()

  exp <- inner_join(exp, exchanges) %>%
    mutate(expPPP = (LCU/pppEX)/1e9, expMER = (LCU/merEX)/1e9) #in billions

  expProdAgg <- filter(exp, ClassificationCode == "e_Class",
                       !str_detect(BHName, "Spirits|Wine|Beer|Tobacco|Narcotics|Coffee|cheese and eggs")) %>%
    group_by(BHName, iso3c) %>%
    select(iso3c, BHName, expPPP, expMER) %>%
    mutate(year = 2017) %>%
    select(iso3c, BHName, expMER, year) #only MER for now

  #split milk and eggs for better matching with FAO where countries only report one or the other

  expMilk <- filter(exp, BHName %in% c("Fresh milk","Preserved milk and other milk products",
                                       "Cheese and curd")) %>%
    group_by(iso3c) %>%
    summarise(expMER = sum(expMER, na.rm=T)) %>%
    mutate(year = 2017, BHName = "Milk products")


  expEggs <- filter(exp, BHName =="Eggs and egg-based products" ) %>%
    group_by(iso3c) %>%
    summarise(expMER = sum(expMER, na.rm=T)) %>%
    mutate(year = 2017, BHName = "Eggs")

  expProdAgg <- rbind(expProdAgg, expMilk, expEggs)

  ############# ADD 2011 values, downloaded from ICP website###################
  ################################

  #assum same ratios of hotel to catering and split of milk cheese eggs for 2011 as in 2017

  ### get ratio of resto to hotel
  hrat <- filter(exp, BHName %in% c("RESTAURANTS AND HOTELS", "CATERING SERVICES")) %>%
    select(BHName, iso3c, expMER) %>%
    pivot_wider(names_from = BHName, values_from = expMER) %>%
    mutate(hrat = `CATERING SERVICES`/`RESTAURANTS AND HOTELS`)

  eggrat <- filter(exp, BHName %in% c("Milk, cheese and eggs", "Eggs and egg-based products")) %>%
    select(BHName, iso3c, expMER) %>%
    pivot_wider(names_from = BHName, values_from = expMER) %>%
    mutate(eggrat  = `Eggs and egg-based products`/`Milk, cheese and eggs`)


  exp2 <- read.csv(system.file("extdata",mapping="c4391ee3-4ebc-4ab0-a94b-bd47c059040e_Data.csv",
                        package = "MarkupsChen"))

  exp21 <- exp2 %>%
    filter(!is.na(Series.Code)) %>%
    select(Country.Code, Series.Name, X2011..YR2011.) %>%
    rename("iso3c" = Country.Code, "expMER" = X2011..YR2011.) %>%
    separate(Series.Name, into = c("BHCode", "BHName"), sep = ":") %>%
    select(!BHCode) %>%
    filter(BHName %in% c( "Bread and cereals", "Meat", "Fish and seafood", "Milk, cheese and eggs",
                          "Oils and fats", "Fruit", "Vegetables", "Sugar, jam, honey, chocolate and confectionery",
                          "Food products n.e.c. (Class)", "RESTAURANTS AND HOTELS")) %>%
    filter(expMER != "..") %>%
    mutate(expMER = as.numeric(expMER))

  cater <- filter(exp21, BHName == "RESTAURANTS AND HOTELS") %>%
    inner_join(hrat) %>%
    mutate("Catering services (Class)" = expMER * hrat) %>%
    select(iso3c,"Catering services (Class)")  %>%
    mutate(BHName = "Catering services (Class)") %>%
    rename("expMER" = "Catering services (Class)") %>%
    filter(!is.na(expMER))

  eggMilk <- filter(exp21, BHName == "Milk, cheese and eggs") %>%
    inner_join(eggrat) %>%
    mutate( Eggs = expMER * eggrat,
            "Milk products" = expMER * (1-eggrat)) %>%
    select(iso3c,"Eggs", "Milk products")  %>%
    pivot_longer(cols = c("Eggs", "Milk products"), names_to = "BHName", values_to = "expMER") %>%
    filter(!is.na(expMER))

  exp21 <- rbind(exp21, cater ) %>%
           rbind(eggMilk) %>%
    filter(!BHName %in% c("RESTAURANTS AND HOTELS", "Milk, cheese and eggs")) %>%
    mutate(year = 2011)

 #currency converts
  ################

  exp21 <- exp21  %>%
    rename("value" = expMER) %>%
    convertGDP(unit_in = "constant 2011 US$MER",
               unit_out = "constant 2017 US$MER",
               replace_NAs = "no_conversion") %>%
    rename("expMER" = value)

  expProdAgg <- rbind(expProdAgg, exp21)

 ########## Redistribute food nec category to all products proportionally, but ignore catering ####
  expProdAggNEC <- toolRedist(expProdAgg, redist = "Food products n.e.c. (Class)") %>%
                             rename("expMERnoCatering" = "expMER")

  expProdAggCatering <-
    toolRedist(expProdAgg, redist = c( "Food products n.e.c. (Class)", "Catering services (Class)")) %>%
    rename("expMERwCatering" = "expMER")

expProdAgg <- inner_join(expProdAggNEC, expProdAggCatering)

###merge sugars and fats into processed##
  expProdAggf <- filter(expProdAgg, BHName %in% c("Sugar, jam, honey, chocolate and confectionery",
                                                   "Oils and fats",
                                                   "Mineral waters, soft drinks, fruit and vegetable juices (Class)")) %>%
    group_by(iso3c, year) %>%
    summarise(across(c(expMERnoCatering,expMERwCatering), sum)) %>%  #expPPP
    mutate(BHName = "Processed")

  expProdAgg <- rbind(expProdAgg, expProdAggf) %>%
    filter(!BHName %in% c( "Sugar, jam, honey, chocolate and confectionery", "Oils and fats",
                            "Mineral waters, soft drinks, fruit and vegetable juices (Class)"))

#####remove where no reporting
 expProdAgg <- filter(expProdAgg, expMERnoCatering !=0 | expMERwCatering != 0 )

return(expProdAgg)
 }


