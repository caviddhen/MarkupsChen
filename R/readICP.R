#' @title readICP
#' @description read original ICP data (under confidentiality agreement)
#' @import dplyr tidyr rlang
#' @export
#'
#' @return dataframe of ICP expenditures
#' @author David M Chen
#' @importFrom readxlsb read_xlsb

readICP <- function() {

  exp <- read_xlsb(system.file("extdata",mapping="ICP-Researcher-Data_Global_2017_Chen_0810-2022.xlsb",
                               package = "mrmarkup"), sheet = "EXP", skip = 3)

  #mapping of k to BH aggregate category to even broader cat
  kBHmap <- read.csv(system.file("extdata",mapping="MappingKBH.csv",
               package = "mrmarkup"))

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

exp[is.na(exp)] <- 0


  expProdAgg <- filter(exp, str_detect(BHName, "Rice|Other cereals|Bread$|Other bakery|Pasta|Beef and veal|Pork|Lamb|Poultry|Other meats|Fish and seafood|milk|Cheese|Eggs|Oils|Fruit|Vegetables|Sugar, jam|Catering services \\(Class\\)|Food products n\\.e\\.c\\. \\(Class\\)")) %>%
    group_by(BHName, iso3c) %>%
    select(iso3c, BHName, expPPP, expMER) %>%
    mutate(year = 2017) %>%
    select(iso3c, BHName, expMER, year) #only MER for now

  kBHagg <- read.csv(system.file("extdata",mapping="MappingBHAgg.csv",
               package = "mrmarkup"))

#group to my grouping
expProdAgg <- expProdAgg  %>% 
              inner_join(kBHagg)  %>% 
              group_by(iso3c, year, Bhagg)  %>% 
              summarise(expMER = sum(expMER))  %>% 
              rename(BHName = Bhagg)

## redistribute meat preps
meatPrep <- filter(expProdAgg, BHName %in% c("Beef and veal", "Lamb, mutton and goat", "Pork", "Poultry",
                                             "Other meats and meat preparations"))  %>% 
            toolRedist(redist = "Other meats and meat preparations")

expProdAgg <- filter(expProdAgg, !BHName %in% c("Beef and veal", "Lamb, mutton and goat", "Pork", "Poultry",
                                             "Other meats and meat preparations")) 
expProdAgg <- rbind(expProdAgg, meatPrep)

 
  ############# ADD 2011 values###################
  ################################

  ### get ratio of resto to hotel
  hrat <- filter(exp, BHName %in% c("RESTAURANTS AND HOTELS", "CATERING SERVICES")) %>%
    select(BHName, iso3c, expMER) %>%
    pivot_wider(names_from = BHName, values_from = expMER) %>%
    mutate(hrat = `CATERING SERVICES`/`RESTAURANTS AND HOTELS`)

  meatrat <- filter(expProdAgg, BHName %in% c("Beef and veal", "Lamb, mutton and goat", "Pork", "Poultry")) %>%
    select(year, BHName, iso3c, expMER) %>%
    pivot_wider(names_from = BHName, values_from = expMER) %>%
        mutate(Meat = `Beef and veal` + `Lamb, mutton and goat`  +   Pork + Poultry)  %>% 
          mutate_at(vars(-c(iso3c, Meat, year)), funs(. / Meat))

eggrat <- filter(exp, BHName %in% c("Milk, cheese and eggs", "Eggs and egg-based products")) %>%
    select(BHName, iso3c, expMER) %>%
    pivot_wider(names_from = BHName, values_from = expMER) %>%
    mutate(eggrat  = `Eggs and egg-based products`/`Milk, cheese and eggs`)

breadrat <- filter(exp, BHName %in% c("Bread and cereals", "Rice")) %>%
    select(BHName, iso3c, expMER) %>%
    pivot_wider(names_from = BHName, values_from = expMER) %>%
    mutate(breadrat  = Rice/`Bread and cereals`)



  exp2 <- read.csv(system.file("extdata",mapping="c4391ee3-4ebc-4ab0-a94b-bd47c059040e_Data.csv",
                        package = "mrmarkup"))

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

meats <- filter(exp21, BHName == "Meat")  %>% 
            inner_join(select(meatrat, -Meat))  %>% 
            mutate_at(vars(-c(iso3c, BHName, year, expMER)), funs(expMER * .))  %>% 
            select(iso3c, 5:last_col())  %>% 
              pivot_longer(cols = c(2:last_col()), names_to = "BHName", values_to = "expMER")  %>% 
              filter(!is.na(expMER))
breads <- filter(exp21, BHName == "Bread and cereals")  %>% 
            inner_join(select(breadrat, -c(`Bread and cereals`, "Rice")))  %>% 
            mutate(Rice = expMER * breadrat, 
                   "Bread and cereals" = expMER * (1-breadrat))  %>% 
            select(iso3c, "Bread and cereals", "Rice")  %>% 
              pivot_longer(cols = c( "Bread and cereals", "Rice"), names_to = "BHName", values_to = "expMER")  %>% 
              filter(!is.na(expMER))

  exp21 <- filter(exp21, !BHName %in% c("RESTAURANTS AND HOTELS", "Milk, cheese and eggs", "Meat", "Bread and cereals")) %>%
           rbind(cater ) %>%
           rbind(eggMilk) %>%
           rbind(meats)  %>% 
           rbind(breads)  %>% 
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


