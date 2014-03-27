sudo apt-get install erlang erlang-doc erlang-manpages

to build from sources:
apt-get install build-essential 
apt-get install ncurses-dev
apt-get install openssl libcurl4-openssl-dev
apt-get install wx-common wx2.8-headers wx2.8-i18n
apt-get install freeglut3-dev libwxgtk2.8-dev libwxgtk2.8-dbg

download http://www.erlang.org/download.html
tar xzf otp...
./configure --with-ssl 
make
sudo make install

for mac:
./configure --with-ssl --enable-darwin-64bit