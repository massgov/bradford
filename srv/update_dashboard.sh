#!/bin/bash
Rscript ../get_data/query.R
sudo systemctl restart shiny-server
