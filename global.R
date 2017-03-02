# Global for bradford dashboard, runs on initialization
library(magrittr)
library(ggplot2)
library(plotly)

data.dir <- "data/"

source("functions/read_data.R")
source("functions/make_plots.R")
source("functions/helper.R")
source("functions/constants.R")

# set the scientific notation
options(scipen = 10000000)

yesterday <- as.character(lubridate::today(tzone = "America/New_York") - lubridate::days(1))

#### READ IN DATA ####
# VISITOR SUCCESS
# Node descendants
drupal.node.descendants <- readRDS(paste0(data.dir, "drupal_node_descendants.RDS"))

# Converions
ga.conversions <- readRDS(paste0(data.dir, "ga_master_conversions.RDS"))


topic.conversions <- readRDS(paste0(data.dir,"topic_conversions.RDS"))
topic.sessions <- readRDS(paste0(data.dir,"topic_sessions.RDS"))

pct.cutoffs <- c(seq(10,90,10),seq(91,100,1))
