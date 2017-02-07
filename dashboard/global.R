# Global for bradford dashboard, runs on initialization
library(magrittr)
library(ggplot2)
library(plotly)

data.dir <- "data/"

source("functions/read_data.R")
source("functions/make_plots.R")
source("functions/helper.R")

# color blind palette 
cb.palette <- c("#999999", 
                "#E69F00", 
                "#56B4E9", 
                "#009E73", 
                "#F0E442", 
                "#0072B2", 
                "#D55E00", 
                "#CC79A7")

options(scipen = 10000000)

yesterday <- as.character(lubridate::ymd(Sys.Date()) - lubridate::days(1))

#### READ IN DATA ####
# VISITOR SUCCESS 
# Node descendants
drupal.node.descendants <- readRDS(paste0(data.dir, "drupal_node_descendants.RDS"))

# Converions
ga.conversions <- readRDS(paste0(data.dir, "ga_master_conversions.RDS")) 

# node id join tables
section.landing.ids <- readRDS(paste0(data.dir, "section_landing_node_ids.RDS"))

topic.ids <- readRDS(paste0(data.dir, "topic_node_ids.RDS"))

subtopic.ids <- readRDS(paste0(data.dir, "subtopic_node_ids.RDS"))


# ANALYST - USER SATISFACTION
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

