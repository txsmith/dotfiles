# Add the Spotify repo
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys BBEBDCB318AD50EC6865090613B00F1FD2C19886 0DF731E45CE24F27EEEB1450EFDC8610341D9410
echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list

# Install needed/useful packages
sudo apt-get update
sudo apt-get install -y curl wget pass pwgen compton zsh libindicator7 libappindicator1 feh pinentry-gtk2 spotify-client autoconf libgtk-3-dev gnome-themes-standard unity-tweak-tool rofi cabal-install libghc-libxml-sax-dev c2hs libasound2-dev libiw-dev libxpm-dev xdotool xmonad pcscd scdaemon libtool help2man libpam-dev yubikey-personalization yubikey-personalization-gui asciidoc libcurl4-gnutls-dev build-essential m4 ruby texinfo libbz2-dev libcurl4-openssl-dev libexpat-dev libncurses-dev zlib1g-dev texlive-full

# Install JDK 8
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get install oracle-java8-installer

# libpam-yubico

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
# Fix npm permissions
mkdir ~/.npm-global
npm config set prefix '~/.npm-global'


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

# Install Node and NPM
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
sudo apt-get install -y nodejs
sudo npm install -g n
sudo n stable

# Install keybase 
cd ~/Downloads
curl -O https://prerelease.keybase.io/keybase_amd64.deb
sudo dpkg -i keybase_amd64.deb
sudo apt-get install -f
run_keybase 
# Don't forget to import your public key into gpg2

# Install Yubico stuff from source to support challenge-response login
# First install yubico-c
git clone https://github.com/Yubico/yubico-c.git
cd yubico-c
autoreconf --install
./configure
sudo make check install

# Install the `ykclient` cli tool
cd ~/Downloads
git clone https://github.com/Yubico/yubico-c-client.git
cd yubico-c-client
autoreconf --install
./configure
sudo make check install

# Install the `yubico-pam` authentication module
cd ~/Downloads
git clone https://github.com/Yubico/yubico-pam.git
cd yubico-pam
autoreconf --install
./configure
sudo make check install

# Activate the yubico-pam module
sudo mv /usr/local/lib/security/pam_yubico.so /lib/security/
# Add the moe to the auth stack by adding this line to /etc/pam.d/common-auth
# auth sufficient pam_yubico.so id=[... your id ... ] key=[..  your api key ...] mode=client

# Install Yubikey udev rules
sudo ln -s ~/dotfiles/.yubico/85-yubikey.rules /etc/udev/rules.d/
sudo service udev reload

# Prime gpg-agent to support ssh
echo "enable-ssh-support" >> ~/.gnupg/gpg-agent.conf

# Also make sure the paths in 85-yubikey.rules are correct for you
# And change authorized_yubikeys accordingly

