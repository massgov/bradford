library(magrittr)
library(RGoogleAnalytics)

# TODO: 1. investigate source of NA values after joins.
#          Seems like some sessions have nothing associated with them except their session ID. These
#          may be bounces.


ga.metrics.events <- c("ga:uniqueEvents")

ga.dims.events <- c("ga:pagePath", "ga:dimension1", "ga:dimension4", "ga:eventCategory",
                    "ga:eventAction")

ga.dims.sessions <- c("ga:pagePath", "ga:dimension1", "ga:dimension2", "ga:dimension3",
                      "ga:dimension4", "ga:searchUsed")

ga.metrics.sessions <- c("ga:timeOnPage")

ga.dims.user <- c("ga:dimension1", "ga:medium", "ga:deviceCategory", "ga:operatingSystem",
                  "ga:browserSize", "ga:browser")

ga.metrics.user <- c("ga:sessionDuration")

start.date <- "2016-12-14"
end.date <-  as.character(lubridate::ymd(Sys.Date()))

# authorize with GA
load("data/GA_token/pilot_token")
RGoogleAnalytics::ValidateToken(token)

#### IMPORT DATA ####
ga.master.events <- RGoogleAnalytics::Init(start.date = start.date,
                                    end.date = end.date,
                                    dimensions = ga.dims.events,
                                    metrics = ga.metrics.events,
                                    max.results = 99999,
                                    table.id = "ga:132193522") %>%
  RGoogleAnalytics::QueryBuilder() %>%
  RGoogleAnalytics::GetReportData(token = token, split_daywise = F, paginate_query = T)

ga.master.sessions <- RGoogleAnalytics::Init(start.date = start.date,
                                           end.date = end.date,
                                           dimensions = ga.dims.sessions,
                                           metrics = ga.metrics.sessions,
                                           max.results = 99999,
                                           table.id = "ga:132193522") %>%
  RGoogleAnalytics::QueryBuilder() %>%
  RGoogleAnalytics::GetReportData(token = token, split_daywise = F, paginate_query = T)

ga.master.user <- RGoogleAnalytics::Init(start.date = start.date,
                                            end.date = end.date,
                                            dimensions = ga.dims.user,
                                            metrics = ga.metrics.user,
                                            max.results = 99999,
                                            table.id = "ga:132193522") %>%
  RGoogleAnalytics::QueryBuilder() %>%
  RGoogleAnalytics::GetReportData(token = token, split_daywise = F, paginate_query = F)


#### MUNGING ####
ga.no.conversion <- ga.master.events %>%
  dplyr::group_by(dimension1) %>%
  dplyr::summarise(no_conversion = ifelse("Conversion" %in% eventCategory == F, 1, 0))

ga.master.no.conversion <- ga.master.sessions %>%
  dplyr::group_by(dimension1) %>%  # group by session ID
  dplyr::mutate(hit_time_utc = lubridate::ymd_hms(dimension2)) %>%
  dplyr::filter(hit_time_utc == max(hit_time_utc)) %>%
  dplyr::full_join(ga.master.user, by = "dimension1") %>%
  dplyr::full_join(ga.no.conversion, by = "dimension1") %>%
  dplyr::rename(sessionID = dimension1,  # naming which does not comply with style
                hitTimestamp = dimension2,
                nodeID = dimension3,
                clientID = dimension4,
                timeOnLastPage = timeOnPage) %>%
  dplyr::ungroup() %>%
  .[complete.cases(.), ]  # remove records with NA

#### MODEL ####
no.conversion.logistic <- ga.master.no.conversion %>%
     dplyr::select(timeOnLastPage, medium:no_conversion, -browserSize) %>%
     purrr::map_if(is.character, as.factor) %>%
     data.frame() %>%
     dplyr::mutate(
         medium = relevel(medium, "(none)"),
         deviceCategory = relevel(deviceCategory, "desktop"),
         operatingSystem = relevel(operatingSystem, "Windows"),
         browser = relevel(browser, "Chrome")) %>%
     useful::build.x(no_conversion ~ ., data = .) %>%
     cbind(no_conversion = ga.master.no.conversion$no_conversion) %>%
     data.frame(.) %>%
     dplyr::select(-X.Intercept.) %>%
     glm(data = ., no_conversion ~ ., family = binomial(link = "logit"))

# get the metrics out
no.conversion.metrics <- no.conversion.logistic %>%
  broom::tidy() %>%  # convert to df
  dplyr::mutate(odds_ratio = exp(estimate),  # Odds ratio aka gradient
         var_diag = diag(vcov(no.conversion.logistic)),  # Variance of each coefficient
         odds_ratio_se = sqrt(odds_ratio ^ 2 * var_diag)) %>%  # Odds-ratio adjusted
  dplyr::filter(p.value < .05)

 #### SAVE DATA ####
saveRDS(no.conversion.metrics, "data/no_conversion_metrics.RDS")
saveRDS(ga.master.no.conversion, "data/ga_master.RDS")
saveRDS(ga.master.sessions, "data/ga_master_sessions.RDS")
