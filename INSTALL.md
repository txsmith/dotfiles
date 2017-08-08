# Add the Spotify repo
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys BBEBDCB318AD50EC6865090613B00F1FD2C19886 0DF731E45CE24F27EEEB1450EFDC8610341D9410
echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list

# Install needed/useful packages
sudo apt-get update
sudo apt-get install curl wget pass pwgen compton zsh libindicator7 libappindicator1 feh pinentry-gtk2 spotify-client autoconf libgtk-3-dev gnome-themes-standard unity-tweak-tool rofi cabal-install libghc-libxml-sax-dev c2hs libasound2-dev libiw-dev libxpm-dev xdotool xmonad pcscd scdaemon

# Replace pinentry-gnome3 with pinentry-gtk2,
# because the gnome version seems to be crazy slow when used in xmonad
sudo apt-get purge pinentry-gnome3

# Link all config files so they are accessible from your home dir
ln -s ~/dotfiles/compton.conf
ln -s ~/dotfiles/.fonts
ln -s ~/dotfiles/.ghci
ln -s ~/dotfiles/.gitconfig
ln -s ~/dotfiles/.gtkrc-2.0
ln -s ~/dotfiles/.warprc
ln -s ~/dotfiles/.Xmodmap
ln -s ~/dotfiles/.xmonad
ln -s ~/dotfiles/.xprofile
ln -s ~/dotfiles/.Xresources
ln -s ~/dotfiles/.yubico

# Install oh my zsh
wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh
rm .zshrc
ln -s ~/dotfiles/.zshrc
chsh -s `which zsh`

# Install Arc GTK theme
cd ~/Downloads
git clone https://github.com/horst3180/arc-theme --depth 1 && cd arc-theme
./autogen.sh --prefix=/usr
sudo make install

# Rebuid font cache
fc-cache -f -v

# Install latest xmonad and xmobar binaries
cabal update
cabal install xmonad xmonad-contrib
cabal install xmobar --flags="all_extensions"
sudo cp ~/.cabal/bin/xmonad /usr/local/bin/xmonad
sudo cp ~/.cabal/bin/xmobar /usr/local/bin/xmobar

# Install Haskell Stack and build the xmonad config
curl -sSL https://get.haskellstack.org/ | sh
cd .xmonad
stack setup
stack build intero
stack build
xmonad --rebuild

# Install VS Code config
ln -s ~/dotfiles/vscode/keybindings.json ~/.config/Code/User/
ln -s ~/dotfiles/vscode/settings.json ~/.config/Code/User/

# Install keybase 
curl -O https://prerelease.keybase.io/keybase_amd64.deb
sudo dpkg -i keybase_amd64.deb
sudo apt-get install -f
run_keybase 

# Don't forget to import your public key

# Install Yubikey udev rules
sudo ln -s ~/dotfiles/.yubico/85-yubikey.rules /etc/udev/rules.d/
sudo service udev reload

# Prime gpg-agent to support ssh
echo "enable-ssh-support" >> ~/.gnupg/gpg-agent.conf

# Also make sure the paths in 85-yubikey.rules are correct for you
# And change authorized_yubikeys accordingly

