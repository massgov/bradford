library(magrittr)
library(RGoogleAnalytics)
library(ggplot2)
library(plotly)

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
end.date <- "2016-12-16"

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
  RGoogleAnalytics::GetReportData(token = token, split_daywise = F, paginate_query = F)

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
ga.conversions <- ga.master.events %>%
  dplyr::group_by(dimension1) %>%
  dplyr::summarise(conversion = ifelse("Conversion" %in% eventCategory == T, T, F))

ga.master <- ga.master.sessions %>%
  dplyr::group_by(dimension1) %>%  # group by session ID
  dplyr::mutate(hit_time_utc = lubridate::ymd_hms(dimension2)) %>%
  dplyr::filter(hit_time_utc == max(hit_time_utc)) %>%
  dplyr::full_join(ga.master.user, by = "dimension1") %>%
  dplyr::full_join(ga.conversions, by = "dimension1") %>%
  dplyr::rename(sessionID = dimension1,  # naming which does not comply with style 
                hitTimestamp = dimension2,
                nodeID = dimension3,
                clientID = dimension4) %>%
  dplyr::ungroup() %>%
  .[complete.cases(.),]

#### MODEL ####
conversion.logistic <- ga.master %>% 
  dplyr::select(timeOnPage, medium:conversion, - browserSize) %>%
  useful::build.x(conversion ~ . - 1, data = ., contrasts = F) %>%  # no intercept because we are not using reference levels
  cbind(conversion = ga.master$conversion) %>%
  data.frame(.) %>%
  glm(data = ., conversion ~ . - 1, family = binomial(link = "logit")) # again, no intercept
  
conversion.metrics <- conversion.logistic %>%  
  broom::tidy() %>%  # convert to df 
  dplyr::mutate(odds_ratio = exp(estimate),  # Odds ratio aka gradient
         var_diag = diag(vcov(conversion.logistic)),  # Variance of each coefficient
         odds_ratio_se = sqrt(odds_ratio ^ 2 * var_diag)) %>% # Odds-ratio adjusted 
  dplyr::filter(p.value < .05)
  
#### SAVE DATA ####
saveRDS(conversion.metrics, "data/conversion_metrics.RDS")
