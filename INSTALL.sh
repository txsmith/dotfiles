spo# Install needed/useful packages
sudo apt update
sudo apt install -y curl wget pass pwgen compton zsh libindicator7 libappindicator1 feh pinentry-gtk2 autoconf libgtk-3-dev gnome-themes-standard unity-tweak-tool rofi cabal-install libghc-libxml-sax-dev c2hs libasound2-dev libiw-dev libxpm-dev xdotool xmonad pcscd scdaemon libtool help2man libpam-dev libusb-1.0.0-dev yubikey-personalization yubikey-personalization-gui asciidoc libcurl4-gnutls-dev build-essential m4 ruby texinfo libbz2-dev libcurl4-openssl-dev libexpat-dev libncurses-dev zlib1g-dev texlive-full davfs2 pass gnome-tweak-tool gnome-shell-extensions htop
sudo apt install libcurl4-gnutls-dev

# Install JDK 8
sudo add-apt-repository ppa:webupd8team/java
sudo apt update
sudo apt install oracle-java8-installer

# Replace pinentry-gnome3 with pinentry-gtk2,
# because the gnome version seems to be crazy slow when used in xmonad
sudo apt purge pinentry-gnome3

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
ln -s ~/dotfiles/.dav2fs

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

# Install Node and NPM
sudo apt install nodejs
sudo apt install npm
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
sudo apt install -y nodejs
mkdir ~/.npm-global
npm config set prefix '~/.npm-global'
sudo npm install -g n
sudo n stable


# Install keybase
cd ~/Downloads
curl -O https://prerelease.keybase.io/keybase_amd64.deb
sudo dpkg -i keybase_amd64.deb
sudo apt install -f
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

cd ~/Downloads
git clone https://github.com/Yubico/yubikey-personalization.git
cd yubico-personalization
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
sudo mv /usr/local/lib/security/pam_yubico.so /lib/x86_64-linux-gnu/security/
# Add the moe to the auth stack by adding this line to /etc/pam.d/common-auth
# auth sufficient pam_yubico.so id=[... your id ... ] key=[..  your api key ...] mode=client

# Install Yubikey udev rules
sudo ln -s ~/dotfiles/.yubico/85-yubikey.rules /etc/udev/rules.d/
sudo service udev reload

# Prime gpg-agent to support ssh
echo "enable-ssh-support" >> ~/.gnupg/gpg-agent.conf

# Also make sure the paths in 85-yubikey.rules are correct for you
# And change authorized_yubikeys accordingly

# Increase the inotify watch limit for IntelliJ
echo "fs.inotify.max_user_watches = 524288" | sudo tee /etc/sysctl.d/idea.conf > /dev/null
sudo sysctl -p --system

# TransIP Stack WebDAV mount
sudo usermod -aG davfs2 thomas
mkdir ~/stack
echo "https://tnsmith.stackstorage.com/remote.php/webdav/ /home/thomas/stack davfs user,rw,noauto 0 0" | sudo tee --append /etc/fstab
