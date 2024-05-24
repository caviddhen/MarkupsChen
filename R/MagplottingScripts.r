MagplottingScripts <- function(BAUgdx, POLgdx){

setwd("/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/plots/")
library(mrmarkup)
library(brms)
POLgdx <-  "/p/projects/magpie/users/davidch/magpie_versions/marketingMargins/magpie/output/Margins_SSP2-POL_2024-04-23_12.52.33/fulldata.gdx"
BAUgdx <-  "/p/projects/magpie/users/davidch/magpie_versions/marketingMargins/magpie/output/Margins_SSP2-BAU_2024-04-23_12.49.35/fulldata.gdx"

setConfig(forcecache=T)
BAUm <- FoodExpMagpieMarkup(gdx = BAUgdx, level = "iso", type = "consumer", prodAggr = FALSE, afterShock = TRUE,
                                povmodel = FALSE, validYi = FALSE)

BAUm <- BAUm  %>% mutate(scen = "BAU")
                               
POL <- FoodExpMagpieMarkup(gdx = POLgdx, level = "iso", type = "consumer", prodAggr = FALSE, afterShock = TRUE,
                                povmodel = FALSE, validYi = FALSE)

POLm <- POL  %>% mutate(scen = "POL")

markups <- rbind(BAUm, POLm)  %>% 
          select(iso3c, year, scen, k, Uncertainty, prodPrice, caterPrice, noCaterPrice) %>% 
         pivot_longer(c(prodPrice, caterPrice, noCaterPrice), names_to = "Price Type")  %>% 
         rename("Price" = "value")  %>% 
         mutate(Price = Price/1000)

def2020 <- filter(markups,year == 2020) %>%
             rename( "pr20" = "Price"  ) %>%
         select(iso3c, scen, k, Uncertainty, pr20, `Price Type`)

markups <- inner_join(markups, def2020) %>%
         mutate(PriceIndex = Price/pr20) %>%
       select(!pr20)


markups$`Price Type` <- factor(markups$`Price Type`,
                               levels = c("prodPrice", "caterPrice", "noCaterPrice"))


# reg <- "EUR"
#
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

markups$`Price Type` <- factor(markups$`Price Type`,
                                  levels = c("caterPrice", "noCaterPrice", "prodPrice"))

kBH <- read.csv(system.file("extdata",mapping="mapMAgPIELEM.csv",
                              package = "mrmarkup"))  %>%
  rename("BHName" = prod)

                                  
kBH$BHName <- factor(kBH$BHName,
                               levels = c("Bread and cereals", "Rice", "Beef and veal", "Pork" ,
                                       "Poultry",  "Lamb, mutton and goat",  "Milk products", "Eggs",
                                          "Vegetables", "Fruit", "Processed", "Fish and seafood"))
kBH <- mutate(kBH,
              t = case_when(
                BHName %in% c("Bread and cereals", "Rice",
                               "Vegetables", "Fruit",
                                "Processed") ~ "Plant-Based",
                BHName %in% c( "Beef and veal", "Pork" ,
                                       "Poultry",  "Lamb, mutton and goat",
                                        "Milk products", "Eggs") ~
                  "Livestock Products"
              ))

consb <- Kcal(gdx = BAUgdx, level = "iso",
           calibrated = TRUE, after_shock = TRUE, 
           products = "kfo", product_aggr = FALSE,
           per_capita = FALSE) * 365  # This is in MILLION KCAL! 
consb <- add_dimension(consb, dim = 3.2, add = "scen", nm = "BAU")

consp <-  Kcal(gdx = POLgdx, level = "iso",
           calibrated = TRUE, after_shock = TRUE, 
           products = "kfo", product_aggr = FALSE,
           per_capita = FALSE) * 365  # This is in MILLION KCAL! 
consp <- add_dimension(consp, dim = 3.2, add = "scen", nm = "POL")

cons <- mbind(consb, consp)

cons <- cons %>% #from FoodExpMagpieMarkup
  as.data.frame(rev=2)  %>%
  rename("foodD" = ".value", "iso3c" = "iso",
         "k" = "kfo", "year" = "t")


markupsGlo <- inner_join(markups, iG) %>%
   inner_join(cons) %>%
   inner_join(kBH) %>%
   group_by(year, incomeG, BHName, Uncertainty, `Price Type`, scen) %>%
   summarise(Price = weighted.mean(Price, w = foodD))  %>% 
    pivot_wider(names_from = Uncertainty, values_from = Price)


markupsGloProd <- inner_join(markups, iG) %>%
  inner_join(cons) %>%
  inner_join(kBH) %>%
  group_by(year, Uncertainty, `Price Type`, BHName, scen) %>%
  summarise(Price = weighted.mean(Price, w = foodD))%>%  
  pivot_wider(names_from = Uncertainty, values_from = Price)

markupsGloG <- inner_join(markups, iG) %>%
  inner_join(cons) %>%
  inner_join(kBH) %>%
  group_by(year,Uncertainty, `Price Type`, scen) %>%
  summarise(Price = weighted.mean(Price, w = foodD)) %>%
  mutate(t = "Total") %>% 
   pivot_wider(names_from = Uncertainty, values_from = Price)

markupsGloIG <- inner_join(markups, iG) %>%
  inner_join(cons) %>%
  inner_join(kBH) %>%
  group_by(year, Uncertainty, incomeG, `Price Type`, scen) %>%
  summarise(Price = weighted.mean(Price, w = foodD)) %>%
  mutate(t = "Total") %>% 
   pivot_wider(names_from = Uncertainty, values_from = Price)

markupsGlo3 <- inner_join(markups, iG) %>%
  inner_join(cons) %>%
  inner_join(kBH) %>%
  group_by(year, Uncertainty, `Price Type`, t, scen) %>%
  summarise(Price = weighted.mean(Price, w = foodD))%>%
    pivot_wider(names_from = Uncertainty, values_from = Price)

markupsGlo3 <- rbind(markupsGlo3, markupsGloG) %>%
             mutate(t = factor(t, levels = c("Plant-Based","Livestock Products",
                                              "Total")))

## BAU plot for all 3 aggregations

ggplot(filter(markupsGlo,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year))+
  geom_ribbon(aes(ymin = q25, ymax = q95, fill = `Price Type`), alpha = 0.2) + 
  geom_line(aes(y = Est, color = `Price Type`), lwd = 1.4)+
  facet_wrap(incomeG~ BHName, scales = "free", nrow = 4) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD05/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                values = c("#1E5B3E", "#348C62",  "#54D598")) +
  scale_fill_manual( guide = "none",
                 values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 10)

ggplot(filter(markupsGloProd,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, color = `Price Type`))+
    geom_ribbon(aes(ymin = q25, ymax = q95, fill = `Price Type`), alpha = 0.2) + 
  geom_line(aes(y = Est), lwd = 1.4)+
 facet_wrap(~ BHName, scales = "fixed", nrow = 2) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                    values = c("#1E5B3E", "#348C62",  "#54D598")) +
   scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598")) +

  theme_bw(base_size = 14)

ggplot(filter(markupsGloIG,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
      aes(x = year, color = `Price Type`))+
    geom_ribbon(aes(ymin = q25, ymax = q95, fill = `Price Type`), alpha = 0.2) + 
  geom_line(aes(y = Est), lwd = 1.4)+
  facet_wrap(~incomeG, scales = "fixed", nrow = 1) +
  ggtitle(paste("BAU")) +
  ylab("Price $USD05/kg")+
    scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                    values = c("#1E5B3E", "#348C62",  "#54D598")) +
   scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 16)


ggplot(filter(markupsGlo3,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, color = `Price Type`))+
    geom_ribbon(aes(ymin = q25, ymax = q95, fill = `Price Type`), alpha = 0.2) + 
  geom_line(aes(y = Est), lwd = 1.4)+
  facet_wrap(~ t, scales = "fixed", nrow = 1) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD/kg")+
     scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                    values = c("#1E5B3E", "#348C62",  "#54D598")) +
   scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 14)


ggplot(filter(markupsGloG,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
         aes(x = year, color = `Price Type`))+
    geom_ribbon(aes(ymin = q25, ymax = q95, fill = `Price Type`), alpha = 0.2) + 
  geom_line(aes(y = Est), lwd = 1.4)+
  #facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD/kg")+
 scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                    values = c("#1E5B3E", "#348C62",  "#54D598")) +
   scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 18)



## POL plot for all 3 aggregations

ggplot(filter(markupsGlo,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
      aes(x = year, color = `Price Type`))+
    geom_ribbon(aes(ymin = q25, ymax = q95, fill = `Price Type`), alpha = 0.2) + 
  geom_line(aes(y = Est), lwd = 1.4)+
  facet_wrap(incomeG~ BHName, scales = "fixed", nrow = 4) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                    values = c("#1E5B3E", "#348C62",  "#54D598")) +
   scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 10)


ggplot(filter(markupsGloIG,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
        aes(x = year, color = `Price Type`))+
    geom_ribbon(aes(ymin = q25, ymax = q95, fill = `Price Type`), alpha = 0.2) + 
  geom_line(aes(y = Est), lwd = 1.4)+
  facet_wrap(~incomeG, scales = "fixed", nrow = 4) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                    values = c("#1E5B3E", "#348C62",  "#54D598")) +
   scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 10)

ggplot(filter(markupsGloProd,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
        aes(x = year, color = `Price Type`))+
    geom_ribbon(aes(ymin = q25, ymax = q95, fill = `Price Type`), alpha = 0.2) + 
  geom_line(aes(y = Est), lwd = 1.4)+
  facet_wrap(~ BHName, scales = "fixed", nrow = 2) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                    values = c("#1E5B3E", "#348C62",  "#54D598")) +
   scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 14)


ggplot(filter(markupsGloG,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
        aes(x = year, color = `Price Type`))+
    geom_ribbon(aes(ymin = q25, ymax = q95, fill = `Price Type`), alpha = 0.2) + 
  geom_line(aes(y = Est), lwd = 1.4)+
  #facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/kg")+
 scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                    values = c("#1E5B3E", "#348C62",  "#54D598")) +
   scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 18)

### facet Scen and BAU GLO prices
a <- ggplot(filter(markupsGloG, year %in% seq(2020, 2050, 5)),
       aes(x = year, color = `Price Type`))+
    geom_ribbon(aes(ymin = q25, ymax = q95, fill = `Price Type`), alpha = 0.2) + 
  geom_line(aes(y = Est), lwd = 1.4)+
  facet_wrap(~ scen, scales = "fixed", nrow = 1) +
  ggtitle(paste("a) Global Average Food Prices \n     Baseline (BAU) and Climate Mitigation (POL) Scenarios")) +
  ylab("Price $USD/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                    values = c("#1E5B3E", "#348C62",  "#54D598")) +
   scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598"))+
  theme_bw(base_size = 20)

library(gridExtra)
filter(markupsGloG, year %in% c(2020, 2050))  %>% 
pivot_wider(names_from = year, values_from = Price)  %>% 
mutate(ch = `2050`/`2020`)

### make relative to BAU

markupsRatioGlo <- markupsGlo %>%
         pivot_wider(names_from = scen, values_from = c(Est, q25, q95)) %>%
         mutate(ratio = Est_POL/Est_BAU, q95 = q25_POL/q25_BAU, q25 = q95_POL/q95_BAU  )

markupsRatioGloProd <- markupsGloProd %>%
  pivot_wider(names_from = scen, values_from = c(Est, q25, q95)) %>%
         mutate(ratio = Est_POL/Est_BAU, q95 = q25_POL/q25_BAU, q25 = q95_POL/q95_BAU  )


markupsRatioGloG <- markupsGloG %>%
  pivot_wider(names_from = scen, values_from = c(Est, q25, q95)) %>%
         mutate(ratio = Est_POL/Est_BAU, q95 = q25_POL/q25_BAU, q25 = q95_POL/q95_BAU  )


markupsRatioGloIG <- markupsGloIG %>%
  pivot_wider(names_from = scen,values_from = c(Est, q25, q95)) %>%
         mutate(ratio = Est_POL/Est_BAU, q95 = q25_POL/q25_BAU, q25 = q95_POL/q95_BAU  )

markupsRatioGlo3 <- markupsGlo3 %>%
  pivot_wider(names_from = scen, values_from = c(Est, q25, q95)) %>%
         mutate(ratio = Est_POL/Est_BAU, q95 = q25_POL/q25_BAU, q25 = q95_POL/q95_BAU  )



### plots comparing BAU and POL
ggplot(filter(markupsRatioGlo, year %in% seq(2020,2050,5)),
       aes(x = year, color = `Price Type`))+
  geom_ribbon(aes(ymin = q25, ymax = q95), alpha = 0.2) + 
  geom_line(aes(y = ratio), lwd = 1.4)+
  facet_wrap(incomeG ~ BHName, scales = "free", nrow = 4) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                    values = c("#1E5B3E", "#348C62",  "#54D598")) +
   scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598"))+
  theme_bw(base_size = 20 )

ggplot(filter(markupsRatioGloProd, year %in% seq(2020,2050,5)),
       aes(x = year, color = `Price Type`))+
  geom_ribbon(aes(ymin = q25, ymax = q95), alpha = 0.2) + 
  geom_line(aes(y = ratio), lwd = 1.4)+
  facet_wrap( ~ BHName, scales = "free", nrow =2) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(labels =  c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c( "#54D598", "#1E5B3E", "#348C62"),
                     guide = guide_legend(reverse = TRUE) )+
     scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598"))+
  theme_bw(base_size = 20)

b <- ggplot(filter(markupsRatioGloIG, year %in% seq(2020,2050,5)),
      aes(x = year, color = `Price Type`))+
  geom_ribbon(aes(ymin = q25, ymax = q95), alpha = 0.2) + 
  geom_line(aes(y = ratio), lwd = 1.4)+
  facet_wrap( ~ incomeG, scales = "free", nrow = 1) +
  ggtitle(paste("b) POL to BAU Price Ratio by Income Group")) +
  scale_color_manual(labels =  c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c( "#54D598", "#1E5B3E", "#348C62"),
                     guide = guide_legend(reverse = TRUE) )+
     scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598"))+
  theme_bw(base_size = 20)

b
filter(markupsRatioGloIG, year %in% c(2020, 2050), 
incomeG %in% c("HIC","LIC")) 




c <- ggplot(filter(markupsRatioGlo3, year %in% seq(2020,2050,5)),
        aes(x = year, color = `Price Type`))+
  geom_ribbon(aes(ymin = q25, ymax = q95), alpha = 0.2) + 
  geom_line(aes(y = ratio), lwd = 1.4)+
  facet_wrap( ~ t, scales = "free", nrow = 1) +
  ggtitle(paste("c) POL to BAU Price Ratio by Product ")) +
  scale_color_manual(labels =  c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c( "#54D598", "#1E5B3E", "#348C62"),
                     guide = guide_legend(reverse = TRUE) )+
     scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598"))+
  theme_bw(base_size = 20)
c
library(gridExtra)
out1 <- grid.arrange(a,b,c)
out1
ggsave(out1, file = "./Fig5.pdf", height = 18, width = 15)
ggsave(out1, file = "./Fig5.png", height = 18, width = 15)

ggplot(filter(markupsRatioGloG, year %in% seq(2020,2050,5)),
        aes(x = year, color = `Price Type`))+
  geom_ribbon(aes(ymin = q25, ymax = q95), alpha = 0.2) + 
  geom_line(aes(y = ratio), lwd = 1.4)+
  #facet_wrap( ~ BHName, scales = "free", nrow = 4) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(labels =  c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c( "#54D598", "#1E5B3E", "#348C62"),
                     guide = guide_legend(reverse = TRUE) )+
     scale_fill_manual( guide = "none",
                      values = c("#1E5B3E", "#348C62",  "#54D598"))+
    theme_bw(base_size = 11)


### Plots comparing ratio of livestock to plant, relative to 2020 value for scenario and price
PLratio <- markups %>% 
  inner_join(iG) %>% 
  inner_join(cons) %>%
  inner_join(kBH) %>%
  group_by(year, incomeG, `Price Type`, t, scen, Uncertainty) %>%
  summarise(Price = weighted.mean(Price, w = foodD))  %>% 
  pivot_wider(names_from = t, values_from = Price) 

PLratio1 <- select(PLratio, !`Livestock Products`)  %>% 
             mutate(Uncertainty = case_when(
                                 Uncertainty == "Est" ~ "Est",
                                Uncertainty == "q25" ~ "q95",
                                Uncertainty == "q95" ~ "q25"))

PLratio <- select(PLratio, !`Plant-Based`)  %>% 
           inner_join(PLratio1)  %>% 
  mutate(LivestockPlantRatio = `Livestock Products` / `Plant-Based`)  

PLr2020 <- filter(PLratio, year == 2020)  %>% 
           rename("LivestockPlantRatio2020" = LivestockPlantRatio)  %>% 
           ungroup() %>% 
           select(!c(year,  `Livestock Products`, `Plant-Based`))

PLratio <- inner_join(PLratio, PLr2020)  %>% 
          mutate(relLivestockPlantRatio = LivestockPlantRatio / LivestockPlantRatio2020)
  
  #bar plot 2020 and 2050 BAU and POL
#PLratio    %>% 
#  select(!c(`Livestock Products`, `Plant-Based`, LivestockPlantRatio2020))  %>% 
#  pivot_wider(names_from = `scen`, values_from = LivestockPlantRatio)  %>% 
#  mutate(ratio = POL/BAU)  %>% 
#  pivot_longer(c(RatioLPRatioCater, RatioLPRatioNoCater), names_to = "ratioType", values_to = "ratioPrices")

## bar plot of the bau vs pol ratios grouped by price type 
#price substitution is also diluted by this effect
#meat becomes more attractive from the substitution effect due to its lower markup, in line with literature on ease of meat preparation

PLratiobar <- filter(PLratio, year %in% c (2020, 2050))  %>% 
             unite("yearscen", year, scen)  %>% 
             filter(yearscen != "2020_POL")  %>% 
             select(yearscen, incomeG, `Price Type`, Uncertainty, LivestockPlantRatio)  %>% 
             pivot_wider(names_from = Uncertainty, values_from = LivestockPlantRatio)

PLratiobarp <- ggplot(PLratiobar,
       aes(x = yearscen , group = `Price Type`, fill = `Price Type`))+
  geom_bar(aes(y = Est), position = "dodge", stat = "identity", width = 0.8)+
  geom_errorbar(aes(ymin=q25, ymax=q95), width=.2,
                 position=position_dodge(.9))  + 
  facet_wrap( ~incomeG, scales = "free") +
  ggtitle(paste("Price Ratio of Livestock to Plant-Based Products")) + 
  scale_fill_manual(labels=c('FAFH', 'FAH', 'Producer'),
                    values = c(  "#1E5B3E","#348C62", "#54D598")) +
  xlab("Year and Scenario") + 
  ylab("Ratio") +
  theme_bw(base_size = 29)


PLratiobar %>% select(yearscen, LivestockPlantRatio)  %>% 
 pivot_wider(names_from = yearscen, values_from = LivestockPlantRatio)  %>% 
 mutate(`2050_BAU` = `2050_BAU`/`2020_BAU`,
     `2050_POL` = `2050_POL`/`2020_BAU` )

PLratiobar  %>% filter(incomeG == "HIC")


ggsave(PLratiobarp, file = "./Fig6.pdf", height = 12, width = 16)
ggsave(PLratiobarp, file = "./Fig6.png", height = 12, width = 16)



#use FAO consumption

### plots comparing BAU and POL
ggplot(filter(markups, iso3c == iso,
              k %in% prods,
              scen %in% c("BAU", "POL"),
              `Price Type` == "noCaterPrice"),
       aes(x = year, y = Price, color = scen))+
  geom_line(lwd = 1.4)+
  facet_wrap(~k, scales = "free") +
  ggtitle(paste(iso, " Consumer FAH Prices")) +
  scale_color_manual(labels = c("BAU", "POL"),
                     values = c( "#5AD2D8", "#995AD8"),
                     guide = guide_legend(reverse = TRUE) ) +
  theme_bw(base_size = 18)

### plots comparing BAU and POL
ggplot(filter(markups, iso3c == iso,
              k %in% prods,
              scen %in% c("BAU", "POL"),
              `Price Type` == "CaterPrice"),
       aes(x = year, y = Price, color = scen))+
  geom_line(lwd = 1.4)+
  facet_wrap(~k, scales = "free") +
  ggtitle(paste(iso, " Consumer FAFH Prices")) +
  scale_color_manual(labels = c("BAU", "POL"),
                     values = c( "#5AD2D8", "#995AD8"),
                     guide = guide_legend(reverse = TRUE) ) +
  theme_bw(base_size = 18)
}


