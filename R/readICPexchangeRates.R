#' @title readICPexchangeRates
#' @description read ICP exchange rates
#' @import dplyr tidyr readxlsb GDPuc mrcommons
#' @export
#'
#' @return dataframe of ICP expenditures
#' @author David M Chen
#' @importFrom readxlsb read_xlsb

readICPexchangeRates <- function() {

#LCU/pppEX = ppp

ppp <-  read_xlsb(system.file("extdata",mapping="ICP-Researcher-Data_Global_2017_Chen_0810-2022.xlsb",
                              package = "mrmarkup"), sheet = "PPP", skip = 3)

colnames(ppp)[1:6] <- ppp[1, c(1:6)]

ppp <- ppp %>%
  filter(!row_number() == 1) %>%
  pivot_longer(cols = 7:ncol(ppp), names_to = "iso3c", values_to = "pppEX") %>%
  rename("BHCode" = `Item Code`, "BHName" = `Item Name`, "year" = Year) %>%
  select(year, BHCode, BHName, iso3c, pppEX) %>%
  mutate(year = as.integer(year))


mer <- read_xlsb(system.file("extdata",mapping="ICP-Researcher-Data_Global_2017_Chen_0810-2022.xlsb",
                             package = "mrmarkup"), sheet = "EXR", skip = 4)

mer <- mer %>%
  pivot_longer(cols = 8:ncol(mer), names_to = "year", values_to = "merEX") %>%
  rename("iso3c" = Country.Code) %>%
  mutate(year = as.numeric(gsub("X", "", year))) %>%
  select(year, iso3c, merEX)

exchanges <- inner_join(ppp, mer)
return(exchanges)
}

