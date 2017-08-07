#!/bin/bash
# Double checking if the Yubikey is actually inserted.
if [ -n "$(lsusb | grep Yubico)" ]; then
  logger "YubiKey Inserted"
  export DISPLAY=:0.0
  xdotool key BackSpace
fi
