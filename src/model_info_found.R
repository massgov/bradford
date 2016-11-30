library(magrittr)
library(glmnet)
library(caret)

new.cols <- c("submit_time", "info_found", "problem_desc", "site", "content_author",
              "child_content_author", "referrer", "ip_addr", "id", "long", "lat",
              "browser", "os")

#### IMPORT DATA ####
formstack.master <- readr::read_csv('data/formstack/formstack_master.csv') %>%
  dplyr::rename_(.dots = setNames(object = paste0("`", names(.), "`"), nm = new.cols)) %>%
  purrr::map_if(is.character, stringr::str_trim) %>%
  data.frame() %>%
  dplyr::filter(ip_addr != "^(146\\.243\\.\\d{1,3}|170\\.63\\.\\d{1,3}|170\\.154\\.\\d{1,3}|65\\.217\\.255\\.\\d{1,3}|4.36.198.102|65.118.148.102|204.166.193.130|204.130.104.10)",
                info_found %in% c("Yes", "No"),
                referrer != "http://<!--") %>%
  dplyr::mutate(info_found = droplevels(info_found),
                referrer = droplevels(referrer))

rm(new.cols)

#### MODELLING ####
# create one-hot encoded dummy variables
expanded.global <- formstack.master %>% 
  dplyr::select( site, content_author, child_content_author, os) %>% 
  caret::dummyVars(formula = ~ . - 1) %>%
  predict(., newdata = formstack.master)

target <- formstack.master[complete.cases(global.expanded), "info_found"]
  
expanded.x.global <- expanded.global[complete.cases(expanded.global), ]

# get coefficients for model which show a relationship between some variable and a users inability to locate content
global.coef.negative <- glmnet(x = expanded.x.global, 
       y = target, 
       family = "binomial", 
       alpha = 1, 
       intercept = F) %>%
  coef(s = .01) %>%
  broom::tidy() %>%
  dplyr::filter(value < 0)
