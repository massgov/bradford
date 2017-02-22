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
paste(" SELECT ",
      "node_id,",
      "content_type,",
      "title",
      "FROM", node.table,
      "WHERE is_disabled = False") %>%
  RPostgreSQL::dbGetQuery(statement = ., conn = db.connection) %>%
  {
    saveRDS(., "dashboard/data/drupal_node_metadata.RDS")
    drupal.node.metadata <<- .
  }



drupal.node.descendants <- paste("SELECT ",
                                 "descendants.node_id,",
                                 "descendant_list,",
                                 "meta.content_type,",
                                 "meta.title",
                                 "FROM", descendants, "AS descendants",
                                 "RIGHT JOIN ", node.table, "AS meta",
                                 "ON descendants.node_id = meta.node_id") %>%
  RPostgreSQL::dbGetQuery(statement = ., conn = db.connection)

# get all node meta types and descendants
drupal.node.descendants %>%
  dplyr::filter(descendant_list != "") %>%
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
  saveRDS(., "dashboard/data/section_landing_node_ids.RDS")

drupal.node.with.descendants$Topic %>%
  dplyr::bind_rows() %>%
  dplyr::rename(topic = title) %>%
  saveRDS(., "dashboard/data/topic_node_ids.RDS")

drupal.node.with.descendants$Subtopic %>%
  dplyr::bind_rows() %>%
  dplyr::rename(subtopic = title) %>%
  saveRDS(., "dashboard/data/subtopic_node_ids.RDS")

# Google analytics
# perhaps the retults of this pipeline should be written to a table for easier access to source of record re: conversions?
# Conversion logic not solid re: multiple conversion sessions and where in a session a conversion can occur
ga.conversions <- paste(" SELECT ",
                        "page_path,",
                        "session_id,",
                        "event_category,",
                        "event_action",
                        "FROM", event.table,
                        "WHERE event_category = 'Conversion'") %>%
  RPostgreSQL::dbGetQuery(statement = ., conn = db.connection)

paste(" SELECT page_path,",
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
  dplyr::mutate(hit_timestamp = lubridate::ymd_hms(hit_timestamp, tz = "UTC"),
                hit_timestamp_eastern = lubridate::with_tz(hit_timestamp, tz = "America/New_York")) %>%
  dplyr::ungroup() %>%
  dplyr::inner_join(ga.conversions, by = c("session_id" = "session_id", "page_path" = "page_path")) %>%
  dplyr::inner_join(drupal.node.metadata, by = "node_id") %>%
  unique() %>%
  saveRDS(., "dashboard/data/ga_master_conversions.RDS")

# Macro View query
c("SELECT parent_info.content_type as parent_type,
   parent_info.title as parent_title,
   des_info.content_type as des_content,
   des_info.title as des_title,
   count(DISTINCT s.session_id) AS sessions,
   sum(c.conversions) AS conversions
   FROM drupal_node_descendants_new as d
   INNER JOIN drupal_nodes AS des_info on d.descendant_id = des_info.node_id
   INNER JOIN drupal_nodes AS parent_info on d.node_id = parent_info.node_id
   RIGHT OUTER JOIN ga_session AS s ON s.node_id = des_info.node_id
   LEFT OUTER JOIN  (SELECT session_id, 1 AS conversions
   FROM ga_events
   WHERE event_category = 'Conversion') AS c
   ON c.session_id = s.session_id
   GROUP BY parent_info.content_type, parent_info.title, d.node_id, d.descendant_id, des_info.content_type,
   des_info.title;") %>%
  # Get dataframe and fill na conversions and sessions with 0
  RPostgreSQL::dbGetQuery(statement = ., db.connection) %>%
  dplyr::mutate(conversions = ifelse(is.na(conversions), 0, conversions),
                sessions = ifelse(is.na(sessions), 0, sessions)) %>%
  saveRDS(., "dashboard/data/grouped.sessions.conversions.RDS")
