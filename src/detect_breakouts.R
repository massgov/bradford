library(magrittr)

#source functions
source("src/funcs.R")

new.cols <- c("submit_time", "info_found", "problem_desc", "site", "content_author",
              "child_content_author", "referrer", "ip_addr", "id", "long", "lat", 
              "browser", "os")

#### IMPORT DATA ####
formstack.master <- readr::read_csv('/Users/Connor/Documents/GitHub/GoogleAnalyticsSegmentation/data/formstack/formstack_master.csv') %>%
  dplyr::rename_(.dots = setNames(object = paste0("`", names(.), "`"), nm = new.cols)) %>%
  purrr::map_if(is.character, stringr::str_trim) %>%
  data.frame() %>%
  dplyr::filter(ip_addr != "^(146\\.243\\.\\d{1,3}|170\\.63\\.\\d{1,3}|170\\.154\\.\\d{1,3}|65\\.217\\.255\\.\\d{1,3}|4.36.198.102|65.118.148.102|204.166.193.130|204.130.104.10)",
                info_found %in% c("Yes", "No"),
                referrer != "http://<!--") %>%
  dplyr::mutate(info_found = droplevels(info_found),
                referrer = droplevels(referrer),
                time_of_day = createTimeBucket(lubridate::hour(submit_time)))

rm(new.cols)

#### SUMMARISE TIMESLICES ####
global.summary.monthly <- formstack.master %>%
  dplyr::mutate(month_year = lubridate::ymd(lubridate::floor_date(submit_time, unit = "month"))) %>%
  dplyr::group_by(site, month_year) %>%
  dplyr::summarise(n_affirmative = sum(info_found == "Yes", na.rm = T),
                   n_negative = sum(info_found == "No", na.rm = T),
                   n_total_responses = n()) %>%
  dplyr::filter(n_total_responses > 50) %>%
  dplyr::mutate(prop_affirmative = n_affirmative / n_total_responses,
                prop_affirmative_se = sqrt(prop_affirmative 
                                           * (1 - prop_affirmative) 
                                           / n_total_responses)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(month_year) %>%
  dplyr::summarise(grand_mean_se =  sum(prop_affirmative_se * n_total_responses) / sum(n_total_responses),
                   grand_mean = sum(prop_affirmative * n_total_responses) / sum(n_total_responses))

global.summary.weekly <- formstack.master %>%
  dplyr::mutate(timestamp = lubridate::ymd(lubridate::floor_date(submit_time, unit = "week"))) %>%
  dplyr::group_by(site, timestamp) %>%
  dplyr::summarise(n_affirmative = sum(info_found == "Yes", na.rm = T),
                   n_negative = sum(info_found == "No", na.rm = T),
                   n_total_responses = n()) %>%
  dplyr::filter(n_total_responses > 50) %>%
  dplyr::mutate(prop_affirmative = n_affirmative / n_total_responses,
                prop_affirmative_se = sqrt(prop_affirmative 
                                           * (1 - prop_affirmative) 
                                           / n_total_responses)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(timestamp) %>%
  dplyr::summarise(grand_mean_se =  sum(prop_affirmative_se * n_total_responses) 
                   / sum(n_total_responses),
                   grand_mean = sum(prop_affirmative * n_total_responses) / sum(n_total_responses))

global.summary.daily <- formstack.master %>%
  dplyr::mutate(date = lubridate::floor_date(submit_time, unit = "day")) %>%
  dplyr::group_by(site, date) %>%
  dplyr::summarise(n_affirmative = sum(info_found == "Yes", na.rm = T),
                   n_negative = sum(info_found == "No", na.rm = T),
                   n_total_responses = n()) %>%
  dplyr::filter(n_total_responses > 50) %>%
  dplyr::mutate(prop_affirmative = n_affirmative / n_total_responses,
                prop_affirmative_se = sqrt(prop_affirmative 
                                           * (1 - prop_affirmative) 
                                           / n_total_responses)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(grand_mean_se =  sum(prop_affirmative_se * n_total_responses) / sum(n_total_responses),
                   grand_mean = sum(prop_affirmative * n_total_responses) / sum(n_total_responses)) 

# create a list where each slot is a data frame specific to a dept summarised by month 
site.summary.monthly <- formstack.master %>%
  dplyr::mutate(month_year = lubridate::ymd(lubridate::floor_date(submit_time, unit = "month"))) %>%
  dplyr::group_by(site, month_year) %>%
  dplyr::summarise(n_affirmative = sum(info_found == "Yes", na.rm = T),
                   n_negative = sum(info_found == "No", na.rm = T),
                   n_total_responses = n()) %>%
  dplyr::filter(n_total_responses > 50) %>%
  dplyr::mutate(prop_affirmative = n_affirmative / n_total_responses,
                prop_affirmative_se = sqrt(prop_affirmative 
                                           * (1 - prop_affirmative) 
                                           / n_total_responses))

site.summary.weekly <- formstack.master %>%
  dplyr::mutate(timestamp = lubridate::ymd(lubridate::floor_date(submit_time, unit = "week"))) %>%
  dplyr::group_by(site, timestamp) %>%
  dplyr::summarise(n_affirmative = sum(info_found == "Yes", na.rm = T),
                   n_negative = sum(info_found == "No", na.rm = T),
                   n_total_responses = n()) %>%
  dplyr::filter(n_total_responses > 50) %>%
  dplyr::mutate(prop_affirmative = n_affirmative / n_total_responses,
                prop_affirmative_se = sqrt(prop_affirmative 
                                           * (1 - prop_affirmative) 
                                           / n_total_responses))

#### DETECT BREAKOUTS ####
global.breakouts.monthly <- global.summary.monthly %>%
  dplyr::select(grand_mean, month_year) %>%
  dplyr::rename(count = grand_mean,
                timestamp = month_year) %>%
  dplyr::mutate(timestamp = as.POSIXct(timestamp)) %>%
  BreakoutDetection::breakout(min.size = 5, method = "multi", plot = T)

global.breakouts.weekly <- global.summary.weekly %>%
  dplyr::select(grand_mean, timestamp) %>%
  dplyr::rename(count = grand_mean) %>%
  dplyr::mutate(timestamp = as.POSIXct(timestamp)) %>%
  BreakoutDetection::breakout(min.size = 6, method = "multi", plot = T)

global.breakouts.daily <- global.summary.daily %>%
  dplyr::select(grand_mean, date) %>%
  dplyr::rename(count = grand_mean,
                timestamp = date) %>%
  BreakoutDetection::breakout(min.size = 3, method = "amoc", plot = T)

site.breakouts.monthly <- site.summary.monthly %>% 
  dplyr::select(prop_affirmative, month_year, site) %>%
  dplyr::rename(count = prop_affirmative,
                timestamp = month_year) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(site = droplevels(site),
                timestamp = as.POSIXct(timestamp)) %>%
  split(.$site) %>%
  lapply(function(x) x[c("count", "timestamp")]) %>%
  purrr::map(function(x) BreakoutDetection::breakout(Z = x, min.size = 3, method = "multi", plot = T))

site.breakouts.weekly <- site.summary.weekly %>% 
  dplyr::select(prop_affirmative, timestamp, site) %>%
  dplyr::rename(count = prop_affirmative) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(site = droplevels(site),
                timestamp = as.POSIXct(timestamp)) %>%
  split(.$site) %>%
  lapply(function(x) x[c("count", "timestamp")]) %>%
  purrr::map(function(x) BreakoutDetection::breakout(Z = x, min.size = 4, method = "amoc", plot = T))
  
 
  
