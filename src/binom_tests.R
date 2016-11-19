library(magrittr)
library(foreach)
library(doParallel)

#source functions
source("src/funcs.R")

new.cols <- c("submit_time", "info_found", "problem_desc", "site", "content_author",
              "child_content_author", "referrer", "ip_addr", "id", "long", "lat",
              "browser", "os")

MIN.RESPONSES <- 50
CONF.LEVEL <- .975  # two-tailed
P.CUTOFF <- .05
POWER.CUTOFF <- .8

#### IMPORT DATA ####
formstack.master <- readr::read_csv('data/formstack/formstack_master.csv') %>%
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

#### RESPONSE SUMMARIES ####
# summaries of responses including proportion affirmative, and standard error (binomial)

# group by site
response.summary.site <- formstack.master %>%
  dplyr::filter(!is.na(site)) %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(n_affirmative = sum(info_found == "Yes", na.rm = T),
                   n_negative = sum(info_found == "No", na.rm = T),
                   n_total_responses = n()) %>%
  dplyr::filter(n_total_responses > MIN.RESPONSES) %>%
  dplyr::mutate(prop_affirmative = n_affirmative / n_total_responses,
                prop_affirmative_se = sqrt(prop_affirmative * (1 - prop_affirmative) / n_total_responses),
                site = droplevels(site)) %>%
  purrr::map_if(is.factor, as.character) %>%  # This is a shitty hack around for "Error in { : task 1 failed - "level sets of factors are different""
  data.frame(stringsAsFactors = F)

# group by page
response.summary.page <- formstack.master %>%
  dplyr::filter(!is.na(referrer)) %>%
  dplyr::group_by(referrer) %>%
  dplyr::summarise(n_affirmative = sum(info_found == "Yes", na.rm = T),
                   n_negative = sum(info_found == "No", na.rm = T),
                   n_total_responses = n()) %>%
  dplyr::filter(n_total_responses > MIN.RESPONSES) %>%
  dplyr::mutate(prop_affirmative = n_affirmative / n_total_responses,
                prop_affirmative_se = sqrt(prop_affirmative * (1 - prop_affirmative) / n_total_responses),
                referrer = droplevels(referrer)) %>%
  purrr::map_if(is.factor, as.character) %>%  # This is a shitty hack around for "Error in { : task 1 failed - "level sets of factors are different""
  data.frame(stringsAsFactors = F)

#### BINOMIAL EXACT TESTS ####
cl <- makeForkCluster(4)
registerDoParallel(cl)

# orgs
response.binom.site <- foreach(interest.site = iter(response.summary.site$site),
                           .packages = c("magrittr", "dplyr"),
                           .combine = 'rbind'
                           ) %dopar% {
  interest.pop = response.summary.site[response.summary.site$site == interest.site, ] # get metrics for pop of interest
  control.pop = formstack.master[formstack.master$site != interest.site, ] %>%  # create control population
    dplyr::summarise(n_affirmative = sum(info_found == "Yes", na.rm = T),
                                         n_negative = sum(info_found == "No", na.rm = T),
                                         n_total_responses = n()) %>%
    dplyr::mutate(prop_affirmative = n_affirmative / n_total_responses,
                  effect_size = abs(prop_affirmative - interest.pop$prop_affirmative))
  power.pop = pwr::pwr.2p2n.test(h = control.pop$effect_size,
                       sig.level = P.CUTOFF,
                       n1 = NULL,
                       n2 = control.pop$n_total_responses,
                       power = POWER.CUTOFF,
                       alternative = "two.sided")
  if (interest.pop$n_total_responses < power.pop$n1) {
    results = NULL
  } else {
    results = binom.test(x = interest.pop$n_affirmative,
               n = interest.pop$n_total_responses,
               p = control.pop$prop_affirmative,
               alternative = "two.sided",
               conf.level = CONF.LEVEL) %>%
      broom::tidy() %>%
      dplyr::mutate(site = interest.site, control.pop.mean = control.pop$prop_affirmative)
  }
  return(results)
}

# pages
response.binom.page <- foreach(interest.page = iter(response.summary.page$referrer),
                               .packages = c("magrittr", "dplyr"),
                               .combine = 'rbind',
                               .errorhandling = "remove"
                               ) %dopar% {
  interest.pop = response.summary.page[response.summary.page$referrer == interest.page, ]  # get metrics for pop of interest
  interest.site = unique(formstack.master[formstack.master$referrer == interest.page, ]$site)
  if (length(interest.site) > 1) {
    stop("multiple sites for the page in question!")
  }
  control.pop = formstack.master[formstack.master$referrer != interest.page, ] %>%
    dplyr::filter(site == interest.site) %>%
    dplyr::summarise(n_affirmative = sum(info_found == "Yes", na.rm = T),
                     n_negative = sum(info_found == "No", na.rm = T),
                     n_total_responses = n()) %>%
    dplyr::mutate(prop_affirmative = n_affirmative / n_total_responses,
                  effect_size = abs(prop_affirmative - interest.pop$prop_affirmative))
  power.pop = pwr::pwr.2p2n.test(h = control.pop$effect_size,
                                 sig.level = P.CUTOFF,
                                 n1 = NULL,
                                 n2 = control.pop$n_total_responses,
                                 power = POWER.CUTOFF,
                                 alternative = "two.sided")
  if (interest.pop$n_total_responses < power.pop$n1) {
    results = NULL
  } else {
    results = binom.test(x = interest.pop$n_affirmative,
               n = interest.pop$n_total_responses,
               p = control.pop$prop_affirmative,
               alternative = "two.sided",
               conf.level = CONF.LEVEL) %>%
      broom::tidy() %>%
      dplyr::mutate(site = interest.page, control.pop.mean = control.pop$prop_affirmative)
  }
    return(results)
}

# stop the cluster
stopCluster(cl)
gc()

# pull out the significant results lel (p < .05)
response.binom.site.signif <- response.binom.site %>%
  dplyr::filter(p.value < P.CUTOFF) %>%
  dplyr::select(site, p.value, estimate, conf.low, conf.high, control.pop.mean)

site.signif.greater <- response.binom.site.signif %>%
  dplyr::filter(ifelse(estimate > control.pop.mean, T, F))

site.signif.less <- response.binom.site.signif %>%
  dplyr::filter(ifelse(estimate < control.pop.mean, T, F))

response.binom.page.signif <- response.binom.page %>%
  dplyr::filter(p.value < P.CUTOFF) %>%
  dplyr::select(site, p.value, estimate, conf.low, conf.high, control.pop.mean)

page.signif.greater <- response.binom.page.signif %>%
  dplyr::filter(ifelse(estimate > control.pop.mean, T, F))

page.signif.less <- response.binom.page.signif %>%
  dplyr::filter(ifelse(estimate < control.pop.mean, T, F))

#### SAVE THE DATA ####
rm(formstack.master, response.summary.page, response.summary.site, cl, CONF.LEVEL, createTimeBucket,
   MIN.RESPONSES, POWER.CUTOFF, P.CUTOFF)

lapply(ls(), function(x) saveRDS(get(x), paste0("data/", x, ".RDS")))
