# Global for bradford dashboard, runs on initialization
library(magrittr)
library(ggplot2)
library(plotly)

DATA.DIR <- "/srv/shiny-server/bradford/data/"

source("/srv/shiny-server/bradford/bradford-test-dashboard/functions/read_data.R")
source("/srv/shiny-server/bradford/bradford-test-dashboard/functions/make_plots.R")

options(scipen = 10000000)
#### READ IN DATA ####
formstack.master <- readRDS(paste0(DATA.DIR, "formstack_master.RDS"))

global.summary.frames <- readIntoList(data.dir = DATA.DIR, pattern = "^global.summary", 
                                      gsub.pattern = ".RDS")

site.summary.frames <- readIntoList(data.dir = DATA.DIR, pattern = "^site.summary",
                                    gsub.pattern = ".RDS") 

global.breakout.frames <- readIntoList(data.dir = DATA.DIR, pattern = "^global.breakouts",
                                       gsub.pattern = ".RDS")

site.breakout.frames <- readIntoList(data.dir = DATA.DIR, pattern = "^site.breakouts",
                                     gsub.pattern = ".RDS")

conversion.metrics <- readRDS(paste0(DATA.DIR, "conversion_metrics.RDS"))

#ga.master.sessions <- readRDS(paste0(DATA.DIR, "ga_master_sessions.RDS"))
