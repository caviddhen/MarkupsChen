#' @title regressMarkups
#' @description regress markups
#' @param plot plot the regression lines
#' @import tidyverse
#' @importFrom minpack.lm nlsLM
#' @import Metrics
#' @import ggplot2
#' @import ggrepel
#' @import brms
#' @export
#'
#' @return dataframe of ICP expenditures
#' @author David M Chen

regressMarkups <- function(plot = TRUE){

library(brms)
library(mrmarkup)
setConfig(forcecache=T)

FAOmarkuppT <- calcMarkup(plot = TRUE)

## make weight same scale as 
FAOmarkuppT <- FAOmarkuppT  %>% group_by(BHName)  %>% 
             add_count()  %>% 
             group_by(year, BHName)  %>% 
             mutate(totPop = sum(pop),
             weight = pop/totPop * n)
#######  with  catering@#!!!!!!
## below is no catering

FAOmarkuppT$FAOmarkup_perT <- FAOmarkuppT$FAOmarkupwCater_perT

FAOmarkuppT2 <- mutate(FAOmarkuppT,
                       loggdp = log(gdp, base = 10))
# #Remove HUN fruit and ISL overall?
# FAOmarkuppT2 <- filter(FAOmarkuppT2,
#                        !(BHName == "Fruit" & iso3c == "HUN"))
# FAOmarkuppT2 <- filter(FAOmarkuppT2,
#                        !(iso3c == "ISL"))
# FAOmarkuppT2 <- filter(FAOmarkuppT2,
#                        !(iso3c == "LUX"))
# FAOmarkuppT2 <- filter(FAOmarkuppT2, FAOmarkup_perT < 10000)

#linear model 
lm_m <- function(df, y){
  lm(FAOmarkup_perT ~ loggdp, weight=weight,
     data = df)
}

#exp model  simple
exp_ms <- function(df){
  minpack.lm::nlsLM(FAOmarkup_perT ~ a*(b^loggdp),
                    weight = weight,
                    alg = "plinear",
                    data = df,
                    start = list(a = 0.005, b = 10 ))
}
exp_ms <- possibly(exp_ms, otherwise = "Error, possibly singular")

#exp model
exp_m <- function(df){
  nls(FAOmarkup_perT ~ a*(b^loggdp) + c,
      weight = weight,
      data = df,
      start = list(a = 0.07, b = 10, c=0))
}
exp_m <- possibly(exp_m, otherwise = "Error, possibly singular")

options(mc.cores = 4)

exp_sbr <-function(df){ brm(
  bf(FAOmarkup_perT|weights(weight)~  a*(b^loggdp),
     a ~ 1, b ~ 1,
     nl = TRUE),
  data = df, family = gaussian(),
  prior = c( prior(normal(1, 500), nlpar = "a"),
             prior(normal(10, 80), nlpar = "b")),
  control = list(adapt_delta = 0.9 ))
}


R2_func <- function(mod){
  RSS.p <- sum(residuals(mod)^2)  # Residual sum of squares
  (TSS <- sum((augment(mod)$FAOmarkup_perT - mean(augment(mod)$FAOmarkup_perT))^2))  # Total sum of squares
  R2 <- 1 - (RSS.p/TSS)
  return(R2)}
R2_func <- possibly(R2_func, otherwise = "no model output")

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


rmse_func <- function(df, model) {
  actual <- df$FAOmarkup_perT
  predicted <- predict(model)
  return(rmse(actual = actual, predicted = predicted))
}
rmse_func <- possibly(rmse_func, otherwise = "Error, no model")



markupCater  <- FAOmarkuppT   %>% 
           mutate(loggdp = log(gdp, base = 10),
               lngdp = log(gdp),
               scalegdp = scale(gdp)) %>% 
           select(iso3c, gdp, year, BHName, FAOmarkupNoCater_perT, FAOmarkupwCater_perT,
                    loggdp, lngdp, scalegdp,  pop)  %>% 
           rename("noCater" = FAOmarkupNoCater_perT, "Cater" = FAOmarkupwCater_perT)  %>% 
           pivot_longer(cols = c(noCater, Cater), names_to = "Cater") %>% 
           ungroup()  %>% 
           add_count()  %>% 
            mutate(totPop = sum(pop),
            weight = pop/totPop * n) 

  markupPrior  <- c(
    set_prior("normal(0.1, 0.05)", nlpar = "a", lb = 0),
    set_prior("normal(1, 1)", nlpar = "b", lb = 0),
    set_prior("normal(0.1, 0.05)", class="sd", nlpar="a"),
    set_prior("normal(0.1, 0.05)", class="sd", nlpar="b"),
    set_prior("student_t(3, 300, 800)", class = "sigma", lb = 0))

  brmMarkupProdBF <- brm(
    bf(value|weights(weight)~  a*(b^lngdp),
       a ~  (1|BHName|Cater)  , b ~  (1|BHName),
       nl = TRUE), init = 0,
    data = markupCater, family = student(), prior = markupPrior,
    control = list(adapt_delta = 0.99, max_treedepth= 12))



post <- pp_check(brmMarkupProdBF, ndraws = 50) + xlim(-5000, 15000)
ggsave(post, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/FinalPlots/FigS7b.pdf", height = 12, width = 16)
ggsave(post, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/FinalPlots/FigS7b.png", height = 12, width = 16)




fitted <- fitted(brmMarkupProdBF, markupCater)
pred <- predict(brmMarkupProdBF, markupCater) 
colnames(pred) <- paste0("pred", colnames(pred))
z <- cbind(markupCater, fitted)

z <- cbind(z, pred)


y <- mutate(z,          label = case_when(
                        weight > 3 ~ iso3c),
                      BHName = factor(BHName, levels = c("Bread and cereals", "Rice", 
                                                      "Fruit", "Vegetables",
                                                         "Beef and veal", "Poultry", "Pork", "Lamb, mutton and goat",
                                                         "Milk products", "Eggs", "Processed")))

library(scales)

supp.labs <- c("Food-Away-From-Home", "Food-At-Home")
names(supp.labs) <- c("Cater", "noCater")


levels(y$BHName) <- c(levels(y$BHName), "Lamb, mutton \n and goat")
levels(y$BHName)[match("Lamb, mutton and goat",levels(y$BHName))] <- "Lamb, mutton \n and goat"

levels(y$BHName) <- c("Bread and cereals", "Rice", 
                                                      "Fruit", "Vegetables",
                                                         "Beef and veal", "Poultry", "Pork", "Lamb, mutton \n and goat",
                                                         "Milk products", "Eggs", "Processed")


brmsplot <- ggplot(filter(y, pop > 1, value < 20000), aes(y=Estimate, x = gdp)) +
#  geom_ribbon(aes(ymin = `predQ2.5`, ymax = `predQ97.5`), fill =  "#c5e1cc", alpha = 0.5 ) +
  geom_ribbon(aes(ymin = `Q2.5`, ymax = `Q97.5`), fill = "#ADDEB9") + 
    geom_point(aes(y=value, size = weight)) +
  geom_line(aes(size = 1), size =1, color = "darkgreen") +
  ggtitle("Consumer Price Markups by Price and Consumption") +
  geom_hline(yintercept=0, linetype="dashed") +
  labs( y = "Markup (USD$17 / tWM)", x = expression("GDPpc"))  +
facet_grid(rows = vars(Cater), cols = vars(BHName),
           scales = "fixed", 
          labeller = labeller( Cater = supp.labs )) +
  ggrepel::geom_text_repel(aes(label = label), max.overlaps = 30) +
scale_x_log10(labels = label_log(digits= 2)) +
ylim(-3000, 18000) +
   theme_bw(base_size = 26) + 
  theme(legend.position = "none") # facet_wrap(~Bhagg, scales = "free")


brmsplot
  
ggsave(brmsplot, height =  15, width = 30,
 file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/FinalPlots/Fig1U.pdf")
ggsave(brmsplot, height =  15, width = 30, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/FinalPlots/Fig1U.png")



brmsplotnolog <- ggplot(y, aes(y=Estimate, x = gdp)) +
 # geom_ribbon(aes(ymin = `predQ2.5`, ymax = `predQ97.5`), fill =  "#c5e1cc", alpha = 0.5 ) +
  geom_ribbon(aes(ymin = `Q2.5`, ymax = `Q97.5`), fill = "#ADDEB9") + 
    geom_point(aes(y=value, size = weight)) +
  geom_line(aes(size = 1), size =1, color = "darkgreen") +
  ggtitle("Consumer Price Markups by Price and Consumption") +
  geom_hline(yintercept=0, linetype="dashed") +
  labs( y = "Markup (USD$17 / tWM)", x = expression("GDPpc"))  +
facet_grid(rows = vars(Cater), cols = vars(BHName),
           scales = "fixed", 
          labeller = labeller( Cater = supp.labs )) +
  ggrepel::geom_text_repel(aes(label = label), max.overlaps = 30) +
#scale_x_log10(labels = label_log(digits= 2)) +
 #scale_x_log10(breaks = 500 * 2^seq(0, 9, by = 1),
 #               labels = label_dollar(scale_cut = cut_short_scale())) +
ylim(-3000, 18000) +
   theme_bw(base_size = 26) + 
  theme(legend.position = "none") # facet_wrap(~Bhagg, scales = "free")


brmsplotnolog
  
ggsave(brmsplotnolog, height =  15, width = 30,
 file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/FinalPlots/FigS1U.pdf")
ggsave(brmsplotnolog, height =  15, width = 30, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/FinalPlots/FigS1U.png")


coef(brmMarkupProdBeveragesLAST)

y1 <- filter(y, iso3c %in% c("USA", "CHN", "NGA", "IND","IDN","DEU","FRA","RUS"))

lineartest <- ggplot(y1, aes(y=Estimate, x= gdp, color = BHName)) +
 # geom_ribbon(aes(ymin = `predQ2.5`, ymax = `predQ97.5`), fill =  "#c5e1cc", alpha = 0.5 ) +
  geom_ribbon(aes(ymin = `Q2.5`, ymax = `Q97.5`), fill = "#ADDEB9") + 
    geom_point(aes(y=value, size = weight)) +
  geom_line(aes(size = 1)) +
  ggtitle("Consumer Price Markups by Price and Consumption") +
  geom_hline(yintercept=0, linetype="dashed") +
  labs( y = "Markup (USD$05 / tWM)", x = expression("logGDPpc"))  +
facet_grid(rows = vars(Cater),
           scales = "free_y") +
  ggrepel::geom_text_repel(aes(y = value, label = iso3c), max.overlaps = 30) +
  theme_bw(base_size = 24)  # facet_wrap(~Bhagg, scales = "free")
lineartest




looR2 <- loo_R2(brmMarkupProdBF)
br2 <- bayes_R2(brmMarkupProdBF)


 coef(brmMarkupProdBF)$BHName[,,"b_Intercept"] %>% as.data.frame() %>% 
mutate(Estimate = round(log(Estimate), 3), "Q2.5" = round(log(Q2.5),3), "Q97.5" = round(log(Q97.5), 3))  %>% 
select(!Est.Error)  %>% write.csv(file = "/p/projects/magpie/users/davidch/ICPdata_cluster/Rev1/FinalPlots/IncomeElasticities.csv")
}