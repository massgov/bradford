#!/bin/bash
# Update system
sudo apt-get update
sudo apt-get dist-upgrade

# Add new R CRAN
sudo echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list

# Add ssl reference for install
sudo echo "deb http://security.ubuntu.com/ubuntu lucid-security main" >> /etc/apt/sources.list

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
sudo apt-get install r-base
sudo apt-get install r-cran-xml libcurl4-gnutls-dev libxml2-dev
sudo apt-get install openssl
sudo apt-get install gdebi-core
sudo apt-get install jq
wget -O ~/shiny-server-1.5.1.834-amd64.deb https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.1.834-amd64.deb
sudo gdebi ~/shiny-server-1.5.1.834-amd64.deb

# Install postgres
sudo apt-get install postgresql postgresql-contrib libpq-dev

git clone https://github.com/massgov/bradford ~/

# Install packages
declare -a packages=('shiny'
                     'magrittr'
                     'dplyr'
                     'scales'
                     'ggplot2'
                     'RCurl'
                     'openssl'
                     'XML'
                     'RGoogleAnalytics'
                     'wordcloud'
                     'stringr'
                     'RPostgreSQL'
                     'forcats'
                     'plotly'
                     'purrr'
                     'lubridate'
                     'shinydashboard'
                     )

for package_name in "${packages[@]}"; do
  sudo su - -c "R -e \"install.packages('$package_name', repos='https://cran.rstudio.com/')\""
done

# restart the server
sudo systemctl restart shiny-server

sudo mkdir /srv/shiny-server/bradford
sudo mkdir /srv/shiny-server/bradford/data

# hack per https://github.com/ropensci/plotly/issues/494 to get shiny + plotly to play nicely
sudo chown -R shiny:shiny /srv/shiny-server

# move the dashboard to the shiny-server directory
sudo cp -r ~/bradford/dashboard/* /srv/shiny-server/bradford
