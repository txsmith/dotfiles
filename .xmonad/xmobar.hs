
Config { 
    font = "xft:SFNS Display:size=11:antialias=true"
  , additionalFonts = ["xft:FontAwesome:size=11:antialias=true"]
  , border = NoBorder
  , allDesktops = True
  , overrideRedirect = True
  , alpha = 0
  , bgColor = "#efefef"
  , fgColor = "#2f343f"
  , position = TopSize C 100 24
  , lowerOnStart = True

  , commands = 
    [ Run Battery [
          "--template" , "<acstatus>",
          "--Low"      , "20",       -- units: %
          "--High"     , "50",       -- units: %
          "--low"      , "darkred",
          "--normal"   , "darkorange",
          "--", -- battery specific options
          -- discharging status
          "-o"	, "<leftipat>  <left>%",
          -- AC "on" status
          "-O"	, "<leftipat>  <left>%",
          "-i"	, "<leftipat>  <left>%",
          -- charged status
          --"-i"	, "<fn=1>\xf240</fn>",
          "--off-icon-pattern", "<fn=1>\xf240</fn>",
          "--on-icon-pattern", "<fn=1>\xf0e7</fn>",
          "--idle-icon-pattern", "<fn=1>\xf0e7</fn>"
        ] 50
      , Run Date "%_d %b %H:%M" "date" 10
      , Run UnsafeStdinReader
      , Run Volume "default" "Master" [
          "-t", "<status>  <volume>%",
          "--",
          "-o", "<fn=1>\xf026</fn>",
          "-O", "<fn=1>\xf028</fn>",
          "-c", "#2f343f",
          "-C", "#2f343f"
        ] 10
      , Run Wireless "wlan0" [
          "-t", "<fn=1>\xf1eb</fn>  <essid>",
          "-x", "Not Connected"
        ] 10
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "  %UnsafeStdinReader% } %date% { %wlan0wi%    %default:Master%    %battery%    <action=`oblogout` button=1><fn=1><raw=1:/></fn></action>  "
}
