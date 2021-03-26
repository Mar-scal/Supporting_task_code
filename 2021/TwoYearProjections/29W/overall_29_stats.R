overall_29_stats <- function(object, strata.labs=mod.res$labels) {
  
  object <- map_df(1:length(object), function(i) map_df(1:length(object[[i]]), function(x) rbind(object[[i]][[x]]))) %>%
    dplyr::group_by(strata) %>%
    dplyr::mutate(row = 1:n())%>%
    dplyr::ungroup()
  
  total_bm <- object %>%
    dplyr::group_by(strata) %>%
    dplyr::mutate(row = 1:n()) %>%
    tidyr::pivot_wider(id_cols = c(row, proj, year, mu, modelyear), names_from = strata, values_from = Biomass) %>%
    dplyr::select(row, strata.labs) %>%
    dplyr::mutate(Total=rowSums(.)-row) %>%
    tidyr::pivot_longer(cols = c(strata.labs, Total)) %>%
    dplyr::rename(Biomass=value,
                  strata=name)
  
  total_catch <- object %>%
    dplyr::group_by(strata) %>%
    dplyr::mutate(row = 1:n()) %>%
    tidyr::pivot_wider(id_cols = c(row, proj, year, mu, modelyear), names_from = strata, values_from = catch) %>%
    dplyr::select(row, strata.labs) %>%
    dplyr::mutate(Total=rowSums(.)-row) %>%
    tidyr::pivot_longer(cols = c(strata.labs, Total)) %>%
    dplyr::rename(catch=value,
                  strata=name)
  
  object <- left_join(total_bm, total_catch, by = c("row", "strata")) %>%
    left_join(dplyr::select(object, row, strata, Biomass, catch, B.change, pB0_increase), by = c("row", "strata", "Biomass", "catch")) %>%
    left_join(unique(dplyr::select(object, row, year, mu, proj, modelyear)), by=c("row"))
  
  return(object)
}