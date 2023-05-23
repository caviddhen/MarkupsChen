#' @title toolRedist
#' @description redistirbute other foods and catering categories
#' @import tidyr dplyr mrcommons
#' @importFrom magpiesets findset
#' @export
#'
#' @return redistributed dataset
#' @author David M Chen

toolRedist <- function(x, redist, output) {

  ratios <- x %>%
    filter(!BHName %in%  redist) %>%
    group_by(iso3c, year) %>%
    summarise(sumMER = sum(expMER, na.rm=TRUE)) %>%
    inner_join(x) %>%
    filter(!BHName %in%  redist) %>%
    mutate(ratioMER = expMER/sumMER) %>%
    select(iso3c, year, BHName, ratioMER) %>%
    pivot_wider(names_from = BHName, values_from = ratioMER)

  colnames(ratios)[3:ncol(ratios)] <- paste0("ratioMER_",colnames(ratios)[3:ncol(ratios)] )

  redistAmt <- filter(x, BHName %in% redist ) %>%
    inner_join(ratios) %>%
    mutate(across(starts_with("ratioMER"), ~ .x * expMER, .names = "Add_{.col}")) %>%
    select(c(iso3c,year, BHName, starts_with("Add"))) %>%
    group_by(iso3c, year) %>%
    summarise(across(starts_with("Add_"), sum, na.rm= T)) %>%
    rename_with(~ str_remove(pattern = "^.*?(_)", .x)) %>%
    pivot_longer(cols = c(3:last_col()), values_to = "exp"  ,
                 names_to = c("curr", "BHName"), names_sep = "_") %>%
    pivot_wider(names_from = "curr", values_from = "exp") %>%
    rename("expMER" = ratioMER)

  out <- bind_rows(x, redistAmt) %>%
    group_by(iso3c, year, BHName) %>%
    summarise(across(starts_with("exp"), sum, na.rm = TRUE)) %>%
    filter(!BHName %in% redist)

return(out)

}
