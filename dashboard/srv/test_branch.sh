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
append_dir = "_TEST"
sudo cp -r ~/$dir_name/* /srv/shiny-server/${dir_name}${append_dir}/
# restart the server
sudo restart shiny-server
