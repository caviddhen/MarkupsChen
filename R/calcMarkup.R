#' @title calcMarkup
#' @description calculate markups
#' @param plot plot markups
#' @import dplyr tidyr
#' @importFrom magclass as.data.frame
#' @export
#'
#' @return dataframe of ICP expenditures
#' @author David M Chen

calcMarkup <- function(plot = FALSE) {

  expProdAgg <- readICP() #ICP expenditures
  expLEC <- calcFAOExpenditures() #FAO expenditures

###new
expFAO <- mutate(expLEC, value = value/1e3,
                 FAOexpAFH = FAOexpAFH/1e3,
                 FAOexpAH = FAOexpAH/1e3) %>%  #in billions
  rename( "BHName" = prod, "FAOexpMER" = value)

#### run this FOR WITH CATERING DIVIDE BY TOTAL Mt Consumption and total expenditure

FAOmarkup <- inner_join(expFAO, expProdAgg) %>%
  mutate(FAOmarkupNoCater = (expMERnoCatering - FAOexpAH),
         FAOmarkupwCater = (expMERwCatering - FAOexpMER)) %>%
  filter(!(is.na(FAOmarkupNoCater) | is.infinite(FAOmarkupNoCater) | FAOexpMER ==0),
         !(is.na(FAOmarkupwCater) | is.infinite(FAOmarkupwCater) | FAOexpMER ==0)) ### remove where 0 expenditure

consW <- readFBSnew(level = "LEM")

### split expenditures AH and AFH ###

pop <- calcOutput("Population", aggregate = F)[,,"pop_SSP2"]
pop <- time_interpolate(pop[,,"pop_SSP2"], interpolated_year <- c(2010:2020), integrate_interpolated_years = TRUE)
pop <- magclass::as.data.frame(collapseNames(pop), rev = 2) %>%
  rename("pop" = .value) %>%
  select(iso3c, year, pop)

AFHpred <- regressFAFH(weight = FALSE, plot = TRUE, predict = "SSP2", threshold = 0.85, calibrate = TRUE)

consW <- magclass::as.data.frame(consW, rev = 2) %>%
  rename("iso3c" = Area, "BHName" = prod, "year" = Year, "Consumption(Mt)" = ".value") %>%
  mutate("Consumption(Mt)" = `Consumption(Mt)`/1e6) %>%
  inner_join(AFHpred) %>%
  mutate(consAFH = `Consumption(Mt)` * AFHshr,
         consAH = `Consumption(Mt)`*(1-AFHshr)) %>% #add regressFAFH coefficients
  select(iso3c, year, BHName, `Consumption(Mt)`, AFHshr, consAFH, consAH)

FAOmarkup1 <- inner_join(FAOmarkup, consW) %>%
  mutate(FAOmarkupNoCater_perT = ((FAOmarkupNoCater/consAH))*1000,
         FAOmarkupwCater_perT = ((FAOmarkupwCater/`Consumption(Mt)`))*1000) %>%  #markup in billions, cons in milions
  inner_join(pop) %>%
  mutate(cons_pc = `Consumption(Mt)` /pop *1000) %>%  #kilograms consumption per capita
  filter(cons_pc > 3) #filter out less than 3 kg per year eating

### see if any expenditures are very low on ICP side
FAOmarkup2 <- FAOmarkup1 %>% mutate(icpExpPc = expMERwCatering/pop * 1000,
                                    ExpPerKG = icpExpPc/cons_pc,
                                    incomeG =  case_when(
                                      gdp <= 1006 ~ "LIC",
                                      gdp > 1006 & gdp <= 3956 ~ "LMIC",
                                      gdp > 3956 & gdp <= 12235 ~ "UMIC",
                                      gdp > 12235 ~ "HIC"))  %>%

  arrange(ExpPerKG) %>%
  group_by(incomeG, BHName)

checkAvg <- FAOmarkup2 %>%
  group_by(incomeG,BHName) %>%
  summarise(avg = mean(ExpPerKG))

rm_20th <- checkAvg %>% mutate(rm= avg*0.1)

FAOmarkup3 <- inner_join(FAOmarkup2,rm_20th) %>%
  mutate(FAOmarkupNoCater_perT  =
           case_when(ExpPerKG > rm ~ FAOmarkupNoCater_perT),
         FAOmarkupwCater_perT  =
           case_when(ExpPerKG > rm ~ FAOmarkupwCater_perT )) %>%
  filter(!is.na(FAOmarkupNoCater_perT),!is.na(FAOmarkupwCater_perT))

if(plot){
ggplot(FAOmarkup3, aes(x=log(gdp, base=10), y = FAOmarkupNoCater_perT)) +
  geom_point(aes(size = pop))+
  #stat_smooth(method =  "lm") +
  facet_wrap(~BHName, scales = "free", nrow=2) +
  geom_line(y = 0) +
  geom_text_repel(aes(label = iso3c))  # facet_wrap(~Bhagg, scales = "free")

ggplot(FAOmarkup3, aes(x= log(gdp, base=10), y = FAOmarkupwCater_perT)) +
  geom_point(aes(size = pop))+
  #stat_smooth(method =  "lm") +
  facet_wrap(~BHName, scales = "free", nrow=2) +
  geom_line(y = 0) +
  ggrepel::geom_text_repel(aes(label = iso3c), max.overlaps = 15)  # facet_wrap(~Bhagg, scales = "free")
 }
return(FAOmarkup3)

}


