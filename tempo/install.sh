#!/bin/sh

cd ~

if grep XKBLAYOUT=.us. /etc/default/keyboard;
then
	echo US Keyboard OK.
else
	echo Switch from GB to US keyboard.
	sudo sed --in-place 's/XKBLAYOUT="gb"/XKBLAYOUT="us"/g' /etc/default/keyboard
fi

# TODO: US KEYBOARD LAYOUT
sudo apt-get -y update
sudo apt-get -y upgrade
sudo apt-get -y install emacs24 subversion
sudo apt-get -y install default-libmysqlclient-dev
sudo apt-get -y install libmysql++-dev

# Really grab the whole thing? It takes kinda long...
# XXX just svn up if it's already present.
echo yes | svn co svn+ssh://tom7@svn.code.sf.net/p/tom7misc/svn/trunk tom7misc
cd tom7misc/tempo
cp /boot/database-config.txt .
make
./tempo

