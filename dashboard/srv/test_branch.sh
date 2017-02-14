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

test_dir=$dir_name"_TEST"

# create test dir if it does not exist
if [ ! -d "/srv/shiny-server/$test_dir" ]; then
  sudo mkdir /srv/shiny-server/$test_dir/
fi

sudo cp -r ~/$dir_name/dashboard /srv/shiny-server/$test_dir/
# restart the server
sudo systemctl restart shiny-server
