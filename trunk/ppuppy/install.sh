#!/bin/bash

sudo apt-get -y install subversion emacs24

mkdir tom7misc
cd tom7misc
svn co svn+ssh://tom7@svn.code.sf.net/p/tom7misc/svn/trunk/ppuppy ppuppy
svn co svn+ssh://tom7@svn.code.sf.net/p/tom7misc/svn/trunk/cc-lib cc-lib
cd ..

cd ~

if grep XKBLAYOUT=.us. /etc/default/keyboard;
then
	echo US Keyboard OK.
else
	echo Switch from GB to US keyboard.
	sudo sed --in-place 's/XKBLAYOUT="gb"/XKBLAYOUT="us"/g' /etc/default/keyboard
fi

if grep FONTFACE=.TerminusBold. /etc/default/console-setup;
then
	echo Console font/size OK.
else
	echo Set to low-res fonts for TV use.
	sudo sed --in-place 's/FONTFACE=.*/FONTFACE="TerminusBold"/g' /etc/default/console-setup
	sudo sed --in-place 's/FONTSIZE=.*/FONTSIZE="14x28"/g' /etc/default/console-setup
fi

# TODO: Also modify /etc/default/locale ?
sudo update-locale LC_ALL="en_US.UTF-8"
sudo update-locale LANGUAGE="en_US:en"
sudo update-locale LANG="en_US.UTF-8"

sudo systemctl disable hciuart.service
sudo systemctl disable bluealsa.service
sudo systemctl disable bluetooth.service
sudo apt-get purge bluez -y
sudo apt-get autoremove -y

sudo systemctl enable ssh
sudo systemctl start ssh
