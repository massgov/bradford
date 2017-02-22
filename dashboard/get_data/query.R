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
paste("SELECT title as topic_title,
       sum(conversions) as conversions
  FROM conversions as c
  INNER JOIN drupal_node_descendants_new as des ON c.page_id = des.descendant_id
  INNER JOIN drupal_nodes as parents ON des.node_id = parents.node_id
  WHERE parents.content_type = 'Topic' and c.category = 'Conversion'
  GROUP BY title") %>%
  RPostgreSQL::dbGetQuery(statement = ., conn = db.connection) %>%
  {
      saveRDS(., "../data/topic_conversions.RDS")
      topic.conversions <<- .
  }

  paste("SELECT parents.title as topic_title,
          sum(sessions) as sessions
          FROM sessions as s
          INNER JOIN drupal_node_descendants_new as des ON s.page_id = des.descendant_id
          INNER JOIN drupal_nodes as parents ON des.node_id = parents.node_id
          WHERE parents.content_type = 'Topic'
          GROUP BY parents.title") %>%
  RPostgreSQL::dbGetQuery(statement = ., conn = db.connection) %>%
  {
      saveRDS(., "../data/topic_sessions.RDS")
      topic.sessions <<- .
  }

