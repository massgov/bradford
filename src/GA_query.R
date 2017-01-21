library(magrittr)
library(RGoogleAnalytics)

# TODO:
#   1. Deal with offset in hit timestamp. Split with stringr::col_split() then add/subtract offset.
#   2. There are a handful of sessions which have all NA's aside from the sessionID, why? This may
#       be caused by an exit before the DOM fully loads (bounce). Or it may be the result of having
#       client side js turned off or some other blocker?  << Probably not becuase the sessionID is still collected

#### QUERIES ####
ga.metrics.events <- c("ga:uniqueEvents")

ga.dims.events <- c("ga:pagePath", "ga:dimension1", "ga:dimension4", "ga:eventCategory",
                    "ga:eventAction")

ga.dims.sessions <- c("ga:pagePath", "ga:dimension1", "ga:dimension2", "ga:dimension3",
                      "ga:dimension4")

ga.metrics.sessions <- c("ga:timeOnPage")

ga.dims.user <- c("ga:dimension1", "ga:medium", "ga:deviceCategory", "ga:operatingSystem",
                  "ga:browser", "ga:source")

ga.metrics.user <- c("ga:sessionDuration")

start.date <- "2016-12-14"
end.date <- as.character(lubridate::ymd(Sys.Date()))

# authorize with GA
load("data/GA_token/pilot_token")
RGoogleAnalytics::ValidateToken(token)

#### IMPORT DATA ####
ga.master.events <- RGoogleAnalytics::Init(start.date = start.date,
                                           end.date = end.date,
                                           dimensions = ga.dims.events,
                                           metrics = ga.metrics.events,
                                           max.results = 99999,
                                           table.id = 'ga:132193522') %>%
  RGoogleAnalytics::QueryBuilder() %>%
  RGoogleAnalytics::GetReportData(token = token, split_daywise = F, paginate_query = T)

ga.master.sessions <- RGoogleAnalytics::Init(start.date = start.date,
                                             end.date = end.date,
                                             dimensions = ga.dims.sessions,
                                             metrics = ga.metrics.sessions,
                                             max.results = 99999,
                                             table.id = 'ga:132193522') %>%
  RGoogleAnalytics::QueryBuilder() %>%
  RGoogleAnalytics::GetReportData(token = token, split_daywise = F, paginate_query = F)

ga.master.user <- RGoogleAnalytics::Init(start.date = start.date,
                                         end.date = end.date,
                                         dimensions = ga.dims.user,
                                         metrics = ga.metrics.user,
                                         max.results = 99999,
                                         table.id = 'ga:132193522') %>%
  RGoogleAnalytics::QueryBuilder() %>%
  RGoogleAnalytics::GetReportData(token = token, split_daywise = F, paginate_query = F)

#### MUNGING ####
ga.conversion <- ga.master.events %>%
  dplyr::group_by(dimension1) %>%
  dplyr::summarise(conversion = factor(ifelse("Conversion" %in% eventCategory == T, "Conversion", "No Conversion")))

ga.master.conversion <- ga.master.sessions %>%
  dplyr::group_by(dimension1) %>%  # group by session ID
  dplyr::mutate(hit_time_utc = lubridate::ymd_hms(dimension2)) %>%
  dplyr::filter(hit_time_utc == max(hit_time_utc)) %>%
  dplyr::full_join(ga.master.user, by = "dimension1") %>%
  dplyr::full_join(ga.conversion, by = "dimension1") %>%
  dplyr::rename(sessionID = dimension1,  # naming which does not comply with style
                hitTimestamp = dimension2,
                nodeID = dimension3,
                clientID = dimension4) %>%
  dplyr::ungroup() %>%
  .[complete.cases(.),]  # remove records with NA

# create a representation of variable length page traversal paths via md5 hashing
ga.session.paths <- ga.master.sessions %>%
  dplyr::select(pagePath, dimension1, dimension2) %>%
  dplyr::group_by(dimension1) %>%
  dplyr::mutate(hitTimeUTC = lubridate::ymd_hms(dimension2)) %>%
  dplyr::arrange(hitTimeUTC) %>%  # place in order
  dplyr::ungroup() %>%
  dplyr::select(pagePath, dimension1)

ga.path.hashes <- ga.session.paths %>%
  split(.$dimension1) %>%
  purrr::map(function(x) digest::digest(paste0(x$pagePath, collapse = ""),  # collapse the full path together
                                        algo = "md5", serialize = F)) %>%
  unlist() %>%
  dplyr::data_frame(dimension1 = names(.), pathHash = .)

ga.path.hashes.top20 <- ga.path.hashes %>%
  dplyr::group_by(pathHash) %>%
  dplyr::count() %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::slice(1:20)


ga.session.hashes <- ga.session.paths %>%
  dplyr::inner_join(ga.path.hashes, by = "dimension1") %>%
  dplyr::select(pagePath, pathHash, dimension1) %>%
  dplyr::group_by(pathHash) %>%
  unique() %>%
  split(.$pathHash) %>%
  purrr::map(function(x) dplyr::select(x[1], pagePath))  # only take out the page path, leave hash as slot name

ga.session.hashes.top20 <- ga.session.hashes[names(ga.session.hashes) %in% ga.path.hashes.top20$pathHash == T]

# join hashes to conversion df
ga.master.conversion <- ga.master.conversion %>%
  dplyr::inner_join(ga.path.hashes, by = c("sessionID" = "dimension1"))

#### SAVE DATA ####
saveRDS(ga.master.conversion, "~/Documents/GitHub/bradford/dashboard/data/ga_master_conversions.RDS")
saveRDS(ga.master.events, "~/Documents/GitHub/bradford/dashboard/data/ga_master_events.RDS")
saveRDS(ga.path.hashes.top20, "~/Documents/GitHub/bradford/dashboard/data/ga_path_hashes_top_20.RDS")
saveRDS(ga.session.hashes.top20, "~/Documents/GitHub/bradford/dashboard/data/ga_session_hashes_top_20.RDS")

system(command = "aws s3 sync ~/Documents/GitHub/bradford/dashboard/data/ s3://mass.gov-analytics/dashboards/bradford/data")
