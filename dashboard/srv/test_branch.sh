# Update code
echo "Please supply the name of the directory containing code of interest in home dir"
read dir_name

cd  ~/$dir_name

echo "Are you testing master? [yes/no]"

read master
if [ $master = "yes" ]; then
  git checkout master
  git pull || exit 255
else
  echo "Please supply the name of the branch you wish to check out and test, followed by [ENTER]:"
  read branch
  git fetch
  git checkout $branch
  git pull || exit 255
fi

# get the connection creds
sudo aws s3 cp s3://mass.gov-analytics/dashboards/bradford/query_creds/db_connect.R ~/bradford/dashboard/get_data/

# run the query so we have up to date data
sudo Rscript ~/bradford/dashboard/get_data/query.R

test_dir=$dir_name"_test"

# create test dir if it does not exist
if [ ! -d "/srv/shiny-server/$test_dir" ]; then
  sudo mkdir /srv/shiny-server/$test_dir/
else
  sudo rm -rf /srv/shiny-server/$test_dir/
fi

sudo cp -r ~/$dir_name/dashboard/ /srv/shiny-server/$test_dir/
# restart the server
sudo systemctl restart shiny-server

# hack per https://github.com/ropensci/plotly/issues/494 to get shiny + plotly to play nicely
sudo chown -R shiny:shiny /srv/shiny-server
