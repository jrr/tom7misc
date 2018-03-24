#!/bin/sh

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

sudo apt-get -y update
sudo apt-get -y upgrade
sudo apt-get -y install emacs24 subversion
sudo apt-get -y install default-libmysqlclient-dev libmysql++-dev mysql-client

# Really grab the whole thing? It takes kinda long...
# XXX just svn up if it's already present.
echo yes | svn co svn+ssh://tom7@svn.code.sf.net/p/tom7misc/svn/trunk tom7misc
cd tom7misc/tempo
cp /boot/database-config.txt .
make

sudo echo "#!/bin/sh -e" > /etc/rc.local
sudo echo "# Installed by tempo on $(date)" >> /etc/rc.local
sudo echo "/home/pi/tom7misc/tempo/onstart.sh &" >> /etc/rc.local
sudo echo "exit 0" >> /etc/rc.local
sudo chmod a+rx /etc/rc.local
chmod a+rx /home/pi/tom7misc/tempo/onstart.sh

./tempo.exe

