#' @title regressMarkups
#' @description regress markups
#' @param plot plot the regression lines
#' @import brms rstan scales ggplot2 ggrepel Metrics tidyverse
#' @export
#'
#' @return regression model object
#' @author David M Chen

regressMarkups <- function(plot = TRUE){

  FAOmarkuppT <- calcMarkup(plot = FALSE)

  ## make weight same scale as data - otherwise brms sees as "more" observations
  FAOmarkuppT <- FAOmarkuppT  %>%
    group_by(BHName)  %>%
    add_count()  %>%
    group_by(year, BHName)  %>%
    mutate(totPop = sum(pop),
           weight = pop/totPop * n)

  FAOmarkuppT$FAOmarkup_perT <- FAOmarkuppT$FAOmarkupwCater_perT

  FAOmarkuppT2 <- mutate(FAOmarkuppT,
                         loggdp = log(gdp, base = 10),
                         lngdp = log(gdp))
   markupCater  <- FAOmarkuppT   %>%
    mutate(loggdp = log(gdp, base = 10),
           lngdp  = log(gdp),
           scalegdp = scale(gdp),
           ihsNoCater = log(FAOmarkupNoCater_perT + sqrt(FAOmarkupNoCater_perT ^ 2 + 1)),
           ihsCater = log(FAOmarkupwCater_perT + sqrt(FAOmarkupwCater_perT ^ 2 + 1))) %>%
    select(iso3c, year, BHName,
           FAOmarkupNoCater_perT, FAOmarkupwCater_perT,
           # change here for ihs transform!!
           #ihsNoCater, ihsCater ,
           gdp, lngdp, loggdp, scalegdp, pop)  %>%
    rename(
      # "noCater" = ihsNoCater, "Cater" = ihsCater
      "noCater" = FAOmarkupNoCater_perT, "Cater" = FAOmarkupwCater_perT
    )  %>%
    pivot_longer(cols = c(noCater, Cater), names_to = "Cater") %>%
    ungroup()  %>%
    add_count()  %>%
    mutate(totPop = sum(pop),
           weight = pop/totPop * n)

  markupCater$yearf  <- as.factor(markupCater$year)


  markupPrior  <- c(
    set_prior("normal(0.1, 0.05)", nlpar = "a", lb = 0),
    set_prior("normal(1, 1)", nlpar = "b", lb = 0),
    set_prior("normal(0.1, 0.05)", class="sd", nlpar="a"),
    set_prior("normal(0.1, 0.05)", class="sd", nlpar="b"),
    set_prior("student_t(3, 300, 800)", class = "sigma", lb = 0))

  brmModel <- brm(
    bf(value|weights(weight)~  a*(b^lngdp),
       a ~  (1|BHName|Cater)  , b ~  (1|BHName),
       nl = TRUE), init = 0,
    data = markupCater, family = student(), prior = markupPrior,
    control = list(adapt_delta = 0.99, max_treedepth= 12))

return(brmModel)
  }
