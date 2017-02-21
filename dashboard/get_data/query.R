library(magrittr)
library(DBI)
library(RPostgreSQL)

# Source creds
source("db_connect.R")
source("../functions/constants.R")

db.driver <- dbDriver("PostgreSQL")

# Load Connection from config file sourced in global
db.connection <- dbConnect(db.driver, dbname = db.name,
                           host = host, port = port,
                           user = user, password = password)


paste("SELECT parent.content_type, 
      parent.title, 
      d.descendant_id
      FROM drupal_node_descendants_new as d
      INNER JOIN drupal_nodes AS parent ON d.node_id = parent.node_id") %>%
      RPostgreSQL::dbGetQuery(statement = ., conn = db.connection) %>%
      {
        saveRDS(., "../data/drupal_node_descendants.RDS")
        drupal.node.descendants <<- . 
      }


paste("SELECT page_id,
          date,
          action,
          category,
          child_type,
          child_title,",
          REFERER,",",
          "sum(conversions) as conversions
          FROM ", CONVERSION.TABLE,
          "GROUP BY 
          page_id,
          date,
          child_title,
          child_type,
          action,
          category,",
          REFERER) %>%
  RPostgreSQL::dbGetQuery(statement = ., conn = db.connection) %>%
  {
      saveRDS(., "../data/ga_master_conversions.RDS")
      ga.conversions <<- .
  }

# # Macro View query
# c("SELECT parent_info.content_type as parent_type,
#    parent_info.title as parent_title,
#    des_info.content_type as des_content,
#    des_info.title as des_title,
#    count(DISTINCT s.session_id) AS sessions,
#    sum(c.conversions) AS conversions
#    FROM drupal_node_descendants_new as d
#    INNER JOIN drupal_nodes AS des_info on d.descendant_id = des_info.node_id
#    INNER JOIN drupal_nodes AS parent_info on d.node_id = parent_info.node_id
#    RIGHT OUTER JOIN ga_session AS s ON s.node_id = des_info.node_id
#    LEFT OUTER JOIN  (SELECT session_id, 1 AS conversions
#    FROM ga_events
#    WHERE event_category = 'Conversion') AS c
#    ON c.session_id = s.session_id
#    GROUP BY parent_info.content_type, parent_info.title, d.node_id, d.descendant_id, des_info.content_type,
#    des_info.title;") %>%
#   # Get dataframe and fill na conversions and sessions with 0
#   RPostgreSQL::dbGetQuery(statement = ., db.connection) %>%
#   dplyr::mutate(conversions = ifelse(is.na(conversions), 0, conversions),
#                 sessions = ifelse(is.na(sessions), 0, sessions)) %>%
#   saveRDS(., "dashboard/data/grouped.sessions.conversions.RDS")
