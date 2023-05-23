#' @title regressFAFH
#' @description calculate markups
#' @import dplyr tidyr
#' @importFrom magclass as.data.frame
#' @export
#' @param weight weighted by population regression
#' @param plot plot the line
#' @param predict SSP income scenario along which to predict
#' @param threshold upper threshold
#' @param calibrate calibrate lines for existing datapoints
#'
#' @return regression values for food away from home
#' @author David M Chen

regressFAFH <- function(weight = TRUE, plot = TRUE, predict = "SSP2", threshold = 0.85, calibrate = TRUE) {

# get consumption share by calore of food outside of home
orf <-  read.csv(system.file("extdata",mapping = "Orfanos2009mean.csv",
                                package = "MarkupsChen"), skip = 1)

orf$year <- 2000
wel <-  read.csv(system.file("extdata",mapping = "WellardColeT3Means.csv",
                             package = "MarkupsChen"))
colnames(wel)[1] <- "country"
wel$year <- 0
wel[which(wel$country=="USA"),"year"] <- 2005

wel[which(wel$country=="Korea"),"year"] <- 2005
wel[which(wel$country=="Brazil"),"year"] <- 2010
wel[which(wel$country=="Australia"),"year"] <- 1995

kcal_fafh <- rbind(orf, wel)
ind <- data.frame("country" = "India", "kcal_shr" = 0.05, "year" = 2010 ) #Law et al. 2018
chn <-  data.frame("country" = "China", "kcal_shr" = 0.26, "year" = 2015 ) #Xue et al. 2021

kcal_fafh <- rbind(kcal_fafh, ind, chn)
kcal_fafh$iso3c <- toolCountry2isocode(kcal_fafh$country, mapping = c("The Netherlands" = "NLD"))

gdppcppp <- calcOutput("GDPpc", aggregate=F)
#gdppcppp <- readRDS("C:/PIK/ICPdata/gdppcppp.rds")
gdppc_iso <- magclass::as.data.frame(collapseNames(gdppcppp[,,"gdppc_SSP2"]), rev = 2) %>%
  rename("gdp" = .value) %>%
  select(iso3c, year, gdp) %>%
  mutate(year = as.numeric(as.character(year)))

catShr <- inner_join(kcal_fafh, gdppc_iso)

pop_iso <- calcOutput("Population", aggregate = FALSE)
#load("C:/PIK/ICPdata/pop_iso.Rda")
pop_iso <- magclass::as.data.frame(pop_iso[,,"pop_SSP2"]) %>%
  rename("pop" = Value, "iso3c" = Region, "year" = Year) %>%
  select(iso3c, year, pop) %>%
  mutate(year = as.numeric(as.character(year)))

catShr <- inner_join(catShr, pop_iso)

if (weight) {
 lm_m <- function(df){
         lm(kcal_shr  ~ gdp,
         weight=pop,
         data = df)}
 } else {
  lm_m <- function(df){
          lm(kcal_shr  ~ gdp,
          data = df) }
}

 efrons_pseudo_r2<-function(model){
  pred <- predict(model)
  n <- length(pred)
  res <- resid(model)
  w <- weights(model)
  if (is.null(w)) w <- rep(1, n)
  rss <- sum(w * res ^ 2)
  resp <- pred + res
  center <- weighted.mean(resp, w)
  r.df <- summary(model)$df[2]
  int.df <- 1
  tss <- sum(w * (resp - center)^2)
  r.sq <- 1 - rss/tss
  adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
  out <- list(pseudo.R.squared = r.sq,
              adj.R.squared = adj.r.sq)
  return(out)
  }


catlm <- lm_m(catShr)

catShr$predlm <- predict(catlm)

#t <- data.frame(gdp = 126004.6 )
#predict(catlm, newdata = t)

#efrons_pseudo_r2(catlm)

if(plot){
s5 <- ggplot(catShr, aes(x=gdp, y = kcal_shr)) +
  geom_point(aes(size = pop)) +
  #geom_line(aes(y = predMM)) +
  geom_line(aes(y = predlm)) +
  ggtitle("Share of total calories consumed Away-From-Home") +
  ylab("share") + xlab("GDP per capita")+
  theme_bw(base_size = 20)+
  theme(legend.position = NULL)+
  guides(size =FALSE)+
  ggrepel::geom_text_repel(aes(label = iso3c), max.overlaps = 15)  # facet_wrap(~Bhagg, scales = "free")
s5
ggsave(s5, height =  8, width = 12, file = "./figS5.png")
ggsave(s5, height =  8, width = 12, file = "./figS5.pdf")

}


gdppcppp <- time_interpolate(gdppcppp, interpolated_year <- c(2010:2020), integrate_interpolated_years = TRUE)

gdpPred <- magclass::as.data.frame(collapseNames(gdppcppp[,,paste0("gdppc_", predict)]), rev = 2) %>%
         rename("gdp" = .value) %>%
        select(iso3c, year, gdp) %>%
        mutate(year = as.numeric(as.character(year)))
gdpPred$pred <- predict(catlm, newdata = gdpPred)


if (calibrate) {
  calib <- data.frame(iso3c = catShr$iso3c, year = catShr$year, calib = 0)
  for(i in catShr$iso3c){
  calib[which(calib$iso3c == i),"calib"] <-  catShr[which(catShr$iso3c == i),"kcal_shr"] -
                                                 catShr[which(catShr$iso3c == i),"predlm"]
  gdpPred[which(gdpPred$iso3c == i), "pred"] <-  gdpPred[which(gdpPred$iso3c == i), "pred"] +
                                                   calib[which(calib$iso3c == i),"calib"]
}
}

gdpPred[which(gdpPred$pred > threshold),"pred"] <- threshold

colnames(gdpPred)[colnames(gdpPred)=="pred"] <- "AFHshr"

return(gdpPred)

}

