library(magrittr)
library(DBI)
library(RPostgreSQL)

# Source creds
source("dashboard/get_data/db_connect.R")

db.driver <- dbDriver("PostgreSQL")

# Load Connection from config file sourced in global
db.connection <- dbConnect(db.driver, dbname = db.name,
                 host = host, port = port,
                 user = user, password = password)

#### Queries ####
# Drupal
paste(" SELECT *",
      "FROM", node.table) %>%
  RPostgreSQL::dbGetQuery(statement = ., conn = db.connection) %>%
  dplyr::select(-raw_json) %>% {
    saveRDS(., "dashboard/data/drupal_node_metadata.RDS")
    drupal.node.metadata <<- .
  }
  

drupal.node.descendants <- paste("SELECT ",
                                 "node_id,",
                                 "descendant_list",
                                 "FROM", descendants) %>%
  RPostgreSQL::dbGetQuery(statement = ., conn = db.connection)
 
# get all node meta types and descendants 
 drupal.node.descendants %>%
  dplyr::filter(descendant_list != "") %>%
  dplyr::right_join(drupal.node.metadata %>%
                      dplyr::select(content_type, node_id, title), by = "node_id") %>%  # join content type to descendant list
  dplyr::filter(is.na(descendant_list) == F) %>% 
  split(.$content_type) %>%
  purrr::map(function(x) split(x = x, f = x["title"])) %>%
  purrr::map(function(x) purrr::map(x, function(y) stringr::str_split(y["descendant_list"],  # go two levels deep in the list
                                                                      pattern = ",") %>% 
                                      data.frame("node_id" = ., "title" = y[["title"]]) %>%
                                      set_names(value = c("node_id", "title")) %>%
                                      dplyr::mutate(node_id = as.numeric(node_id))
                                    )
             ) %>%
             {
               saveRDS(., "dashboard/data/drupal_node_descendants.RDS")
               drupal.node.with.descendants <<- .
             }

drupal.node.with.descendants$`Section Landing` %>%
  dplyr::bind_rows() %>%
  dplyr::rename(site_section = title) %>%
  {
    saveRDS(., "dashboard/data/section_landing_node_ids.RDS")
    section.landing.ids <<- . 
  }

drupal.node.with.descendants$Topic %>%
  dplyr::bind_rows() %>%
  dplyr::rename(topic = title) %>%
  {
    saveRDS(., "dashboard/data/topic_node_ids.RDS")
    topic.ids <<- .
  }
 
drupal.node.with.descendants$Subtopic %>%
  dplyr::bind_rows() %>%
  dplyr::rename(subtopic = title) %>%
  {
    saveRDS(., "dashboard/data/subtopic_node_ids.RDS")
    subtopic.ids <<- .
  }

# Google analytics
ga.sessions <- paste(" SELECT page_path,",
              "features.session_id,",
              "hit_timestamp,",
              "node_id,",
              "client.client_id,",
              "features.medium,",
              "features.device_category,",
              "features.operating_system,",
              "features.source,",
              "features.browser",
              "FROM ", session.table, "AS session",
              "INNER JOIN ", client.table, "AS client",
              "ON session.session_id = client.session_id", 
              "INNER JOIN ", session.feature.table, "AS features",
              "ON client.session_id = features.session_id;") %>%
  RPostgreSQL::dbGetQuery(statement = ., conn = db.connection) %>%
  #dplyr::group_by(session_id) %>%  # group by session ID
  dplyr::mutate(hit_timestamp = lubridate::ymd_hms(hit_timestamp, tz = "UTC"),
                hit_timestamp_eastern = lubridate::with_tz(hit_timestamp, tz = "America/New_York")) %>%
  #dplyr::filter(hit_timestamp_eastern == max(hit_timestamp_eastern)) %>%  # take the last page in each session
  dplyr::ungroup()

# perhaps the retults of this pipeline should be written to a table for easier access to source of record re: conversions?
# Conversion logic not solid re: multiple conversion sessions and where in a session a conversion can occur 
ga.conversions <- paste(" SELECT *", 
                        "FROM", event.table,
                        "WHERE event_category = 'Conversion'") %>%
  RPostgreSQL::dbGetQuery(statement = ., conn = db.connection)

ga.sessions %>%
  dplyr::inner_join(ga.conversions, by = c("session_id" = "session_id", "page_path" = "page_path")) %>%
  dplyr::inner_join(drupal.node.metadata, by = "node_id") %>%
  unique() %>%
  {
    saveRDS(., "dashboard/data/ga_master_conversions.RDS")
    ga.master.conversions <<- .
  }