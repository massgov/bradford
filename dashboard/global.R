# Global for bradford dashboard, runs on initialization
library(magrittr)
library(ggplot2)
library(plotly)

data.dir <- "data/"

source("functions/read_data.R")
source("functions/make_plots.R")
source("functions/helper.R")

# color blind palette 
cb.palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

options(scipen = 10000000)

yesterday <- as.character(lubridate::ymd(Sys.Date()) - lubridate::days(1))

#### READ IN DATA ####
# metadata
ga.node.metadata <- readRDS(paste0(data.dir, "ga_node_metadata_joined.RDS"))

ga.node.parent.child <- readRDS(paste0(data.dir, "ga_node_parent_child.RDS"))

# User Satisfaction
formstack.master <- readRDS(paste0(data.dir, "formstack_master.RDS"))

referrer.breakouts.monthly <- readRDS(paste0(data.dir, "referrer.breakouts.monthly.RDS"))

referrer.summary.monthly <- readRDS(paste0(data.dir, "referrer.summary.monthly.RDS"))

global.summary.frames <- readIntoList(data.dir = data.dir, pattern = "^global.summary", 
                                      gsub.pattern = ".RDS")

site.summary.frames <- readIntoList(data.dir = data.dir, pattern = "^site.summary",
                                    gsub.pattern = ".RDS") 

global.breakout.frames <- readIntoList(data.dir = data.dir, pattern = "^global.breakouts",
                                       gsub.pattern = ".RDS")

site.breakout.frames <- readIntoList(data.dir = data.dir, pattern = "^site.breakouts",
                                     gsub.pattern = ".RDS")

# Converions
ga.conversions <- readRDS(paste0(data.dir, "ga_master_conversions.RDS")) %>%
  dplyr::mutate(nodeID = as.integer(nodeID)) %>%
  dplyr::inner_join(ga.node.metadata, by = c("nodeID" = "id"))

# Funnel Performance 
ga.path.hashes.top20 <- readRDS(paste0(data.dir, "ga_path_hashes_top_20.RDS"))

ga.session.hashes.top20 <- readRDS(paste0(data.dir, "ga_session_hashes_top_20.RDS"))
