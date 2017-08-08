# Install Spotify repo
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys BBEBDCB318AD50EC6865090613B00F1FD2C19886 0DF731E45CE24F27EEEB1450EFDC8610341D9410
echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list

sudo apt-get update
sudo apt-get install curl wget pass pwgen compton zsh libindicator7 libappindicator1 feh pinentry-gtk2 spotify-client autoconf libgtk-3-dev gnome-themes-standard
sudo apt-get purge pinentry-gnome3

ln -s ~/dotfiles/compton.conf
ln -s ~/dotfiles/.ghci
ln -s ~/dotfiles/.gitconfig
ln -s ~/dotfiles/.warprc
ln -s ~/dotfiles/.Xmodmap
ln -s ~/dotfiles/.xmonad
ln -s ~/dotfiles/.xprofile
ln -s ~/dotfiles/.Xresources
ln -s ~/dotfiles/.yubico

# oh my zsh
wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh
rm .zshrc
ln -s ~/dotfiles/.zshrc
chsh -s `which zsh`

# Install Arc GTK theme
cd ~/Downloads
git clone https://github.com/horst3180/arc-theme --depth 1 && cd arc-theme
./autogen.sh --prefix=/usr
sudo make install


# Install Haskell Stack
curl -sSL https://get.haskellstack.org/ | sh
 