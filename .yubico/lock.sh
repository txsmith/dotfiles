#!/bin/bash
# Double checking if the Yubikey is actually removed, Challenge-Response won't trigger the screensaver this way.
if [ -z "$(lsusb | grep Yubico)" ]; then
  logger "YubiKey Removed or Changed"
  export DISPLAY=:0.0
  gnome-screensaver-command -l
fi
