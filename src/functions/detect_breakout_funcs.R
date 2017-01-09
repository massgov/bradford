# functions for breakout detection
makeSummaryDf <- function(df, global = T) {
  # Takes a data frame of pre-grouped formstack responses and summarises them into proportion responding yes 
  # with standard errors. Can summarise at the timestamp (global) level.
  dat = df %>%
    dplyr::summarise(n_affirmative = sum(info_found == "Yes", na.rm = T),
                     n_negative = sum(info_found == "No", na.rm = T),
                     n_total_responses = n()) %>%
    dplyr::filter(n_total_responses > 50) %>%
    dplyr::mutate(prop_affirmative = n_affirmative / n_total_responses,
                  prop_affirmative_se = sqrt(prop_affirmative 
                                             * (1 - prop_affirmative) 
                                             / n_total_responses))
    if (global) {  # if global is true we group only on timestamp 
      dat %>%
        dplyr::ungroup() %>%
        dplyr::group_by(timestamp) %>%
        dplyr::summarise(prop_affirmative_se =  sum(prop_affirmative_se * n_total_responses) / sum(n_total_responses),  # pooled  
                         prop_affirmative = sum(prop_affirmative * n_total_responses) / sum(n_total_responses))
    } else {
      return(dat)
    }
}
