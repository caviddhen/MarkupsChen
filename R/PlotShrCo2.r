
library(mrmarkup)
library(brms)
library(magpie4)
POLgdx <-  "/p/projects/magpie/users/davidch/magpie_versions/marketingMargins/magpie/output/Margins_SSP2-POL_2024-04-23_12.52.33/fulldata.gdx"

setConfig(forcecache=T)

POL <- FoodExpMagpieMarkup(gdx = POLgdx, level = "iso", type = "consumer", prodAggr = TRUE, afterShock = TRUE,
                                povmodel = FALSE, validYi = FALSE)

gdppc <- calcOutput("GDPpc", aggregate = FALSE)[,,"gdppc_SSP2"]  %>% 
        as.data.frame(rev = 2)  %>% 
        rename("gdppc" = ".value")
iG <- gdppc %>% filter(year == 2020) %>%
  mutate(incomeG =  case_when(
    gdppc <= 1006 ~ "LIR",
    gdppc > 1006 & gdppc <= 3956 ~ "LMIR",
    gdppc > 3956 & gdppc <= 12235 ~ "UMIR",
    gdppc > 12235 ~ "HIR")) %>%
  select(iso3c, incomeG)
iG$incomeG <- factor(iG$incomeG, levels = c("HIC", "UMIC","LMIC", "LIC"))

POL <- POL  %>% inner_join(iG)  %>% 
    group_by(incomeG, year)  %>% 
    summarise(farmShrTot = weighted.mean(farmShrTot, w=totExp))


ctax <- PriceGHG(POLgdx)[,,"co2_c"][,,"peatland"] %>% collapseNames()  %>% 
      as.data.frame(rev = 2)  %>% 
      rename("cPrice" = ".value", "iso3c" = "i", "year" = "t_all")


gdppc <- calcOutput("GDPpc", aggregate = TRUE)[,,"gdppc_SSP2"]  %>% 
        as.data.frame(rev = 2)  %>% 
        rename("gdppc" = ".value")
iGA <- gdppc %>% filter(year == 2020) %>%
  mutate(incomeG =  case_when(
    gdppc <= 1006 ~ "LIC",
    gdppc > 1006 & gdppc <= 3956 ~ "LMIC",
    gdppc > 3956 & gdppc <= 12235 ~ "UMIC",
    gdppc > 12235 ~ "HIC")) %>%
  select(iso3c, incomeG)
iGA$incomeG <- factor(iGA$incomeG, levels = c("HIC", "UMIC","LMIC", "LIC"))

ctax <- inner_join(ctax, iGA)  %>% 
     group_by(year, incomeG)  %>% 
      summarise(cPriceM = mean(cPrice))

out <- inner_join(POL, ctax)  
a <- ggplot(out, aes(y = farmShrTot, x = cPriceM)) +
     geom_line(size = 2) + 
    facet_grid(~incomeG) +
    theme_bw(base_size = 20) +
    ylab("Farm Share (%)") +
    xlab("Carbon Price ($/ton)")


ggsave(a, height =  10, width = 20,
 file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/chceck.pdf")
