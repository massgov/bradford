library(magrittr)

#source functions
source("src/funcs.R")
source("src/functions/detect_breakout_funcs.R")
source("src/creds.R")

new.cols <- c("submit_time", "info_found", "problem_desc", "site", "content_author",
              "child_content_author", "referrer", "ip_addr", "id", "long", "lat",
              "browser", "os")

#### IMPORT DATA ####
formstack.master <- readr::read_csv("data/formstack/formstack_master.csv") %>%
  dplyr::rename_(.dots = setNames(object = paste0("`", names(.), "`"), nm = new.cols)) %>%
  purrr::map_if(is.character, stringr::str_trim) %>%
  data.frame() %>%
  dplyr::filter(ip_addr != ip.range,
                info_found %in% c("Yes", "No"),
                referrer != "http://<!--") %>%
  dplyr::mutate(info_found = droplevels(info_found),
                referrer = droplevels(referrer),
                time_of_day = createTimeBucket(lubridate::hour(submit_time)))

rm(new.cols)

#### SUMMARISE TIMESLICES ####
global.summary.monthly <- formstack.master %>%
  dplyr::mutate(timestamp = lubridate::ymd(lubridate::floor_date(submit_time, unit = "month"))) %>%
  dplyr::group_by(site, timestamp) %>%
  makeSummaryDf(global = T)

global.summary.weekly <- formstack.master %>%
  dplyr::mutate(timestamp = lubridate::ymd(lubridate::floor_date(submit_time, unit = "week"))) %>%
  dplyr::group_by(site, timestamp) %>%
  makeSummaryDf(global = T)

# create a list where each slot is a data frame specific to a dept summarised by month
site.summary.monthly <- formstack.master %>%
  dplyr::mutate(timestamp = lubridate::ymd(lubridate::floor_date(submit_time, unit = "month"))) %>%
  dplyr::group_by(site, timestamp) %>%
  makeSummaryDf(global = F)

site.summary.weekly <- formstack.master %>%
  dplyr::mutate(timestamp = lubridate::ymd(lubridate::floor_date(submit_time, unit = "week"))) %>%
  dplyr::group_by(site, timestamp) %>%
  makeSummaryDf(global = F)

# create a list where each slot is a data frame specific to an endpoint summarised by month
referrer.summary.monthly <- formstack.master %>%
  dplyr::mutate(timestamp = lubridate::ymd(lubridate::floor_date(submit_time, unit = "month"))) %>%
  dplyr::group_by(referrer, timestamp) %>%
  makeSummaryDf(global = F)

#### DETECT BREAKOUTS ####
global.breakouts.monthly <- global.summary.monthly %>%
  dplyr::select(prop_affirmative, timestamp) %>%
  dplyr::rename(count = prop_affirmative,
                timestamp = timestamp) %>%
  dplyr::mutate(timestamp = as.POSIXct(timestamp)) %>%
  BreakoutDetection::breakout(min.size = 3, method = "multi", plot = T)

global.breakouts.weekly <- global.summary.weekly %>%
  dplyr::select(prop_affirmative, timestamp) %>%
  dplyr::rename(count = prop_affirmative) %>%
  dplyr::mutate(timestamp = as.POSIXct(timestamp)) %>%
  BreakoutDetection::breakout(min.size = 12, method = "multi", plot = T)

site.breakouts.monthly <- site.summary.monthly %>%
  dplyr::select(prop_affirmative, timestamp, site) %>%
  dplyr::rename(count = prop_affirmative,
                timestamp = timestamp) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(site = droplevels(site),
                timestamp = as.POSIXct(timestamp)) %>%
  split(.$site) %>%
  purrr::map(function(x) x[c("count", "timestamp")]) %>%
  purrr::map(function(x) BreakoutDetection::breakout(Z = x, min.size = 3, method = "multi", plot = T))

site.breakouts.weekly <- site.summary.weekly %>%
  dplyr::select(prop_affirmative, timestamp, site) %>%
  dplyr::rename(count = prop_affirmative) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(site = droplevels(site),
                timestamp = as.POSIXct(timestamp)) %>%
  split(.$site) %>%
  purrr::map(function(x) x[c("count", "timestamp")]) %>%
  purrr::map(function(x) BreakoutDetection::breakout(Z = x, min.size = 12, method = "multi", plot = T))

# referrers only monthly for now to avoid inconsistent sample intervals
referrer.breakouts.monthly <- referrer.summary.monthly %>%
  dplyr::select(prop_affirmative, timestamp, referrer) %>%
  dplyr::rename(count = prop_affirmative,
                timestamp = timestamp) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(referrer = droplevels(referrer),
                timestamp = as.POSIXct(timestamp)) %>%
  split(.$referrer) %>%
  purrr::map(function(x) x[c("count", "timestamp")]) %>%
  purrr::map(function(x) BreakoutDetection::breakout(Z = x, min.size = 3, method = "multi", plot = T))

rm(formstack.master)

#### SAVE DATA ####
purrr::map(ls(), function(x) saveRDS(get(x), file = paste0("data/", x, ".RDS")))
