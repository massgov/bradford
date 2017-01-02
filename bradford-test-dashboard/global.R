# Global for bradford dashboard, runs on initialization
library(magrittr)
library(ggplot2)
library(plotly)

DATA.DIR <- "~/Documents/GitHub/bradford/data/"

source("~/Documents/GitHub/bradford/bradford-test-dashboard/functions/read_data.R")
source("~/Documents/GitHub/bradford/bradford-test-dashboard/functions/make_plots.R")

# color blind palette 
cb.palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

options(scipen = 10000000)

#### READ IN DATA ####
# User Satisfaction
formstack.master <- readRDS(paste0(DATA.DIR, "formstack_master.RDS"))

referrer.breakouts.monthly <- readRDS(paste0(DATA.DIR, "referrer.breakouts.monthly.RDS"))

referrer.summary.monthly <- readRDS(paste0(DATA.DIR, "referrer.summary.monthly.RDS"))

global.summary.frames <- readIntoList(data.dir = DATA.DIR, pattern = "^global.summary", 
                                      gsub.pattern = ".RDS")

site.summary.frames <- readIntoList(data.dir = DATA.DIR, pattern = "^site.summary",
                                    gsub.pattern = ".RDS") 

global.breakout.frames <- readIntoList(data.dir = DATA.DIR, pattern = "^global.breakouts",
                                       gsub.pattern = ".RDS")

site.breakout.frames <- readIntoList(data.dir = DATA.DIR, pattern = "^site.breakouts",
                                     gsub.pattern = ".RDS")

# Converions
ga.conversions <- readRDS(paste0(DATA.DIR, "ga_master_conversions.RDS"))

conversion.metrics <- readRDS(paste0(DATA.DIR, "no_conversion_metrics.RDS"))

# Funnel Performance 
ga.path.hashes.top20 <- readRDS(paste0(DATA.DIR, "ga_path_hashes_top_20.RDS"))

ga.session.hashes.top20 <- readRDS(paste0(DATA.DIR, "ga_session_hashes_top_20.RDS"))
