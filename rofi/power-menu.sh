#!/usr/bin/env bash

uptime=$(uptime | awk -e '{print $3 " " $4}')
theme=$HOME/Dotfiles/rofi/power-menu.rasi

rofi_command="rofi -theme $theme"

# Options
shutdown=" Shutdown"
reboot=" Restart"
lock=" Lock"
suspend=" Sleep"
logout=" Logout"

# Variable passed to rofi
options="$lock\n$suspend\n$logout\n$reboot\n$shutdown"

chosen="$(echo -e "$options" | $rofi_command -p "Uptime: $uptime" -dmenu -selected-row 0)"
case $chosen in
    $shutdown) systemctl poweroff
        ;;
    $reboot) systemctl reboot
        ;;
    $lock) xautolock -locknow
        ;;
    $suspend) systemctl suspend
        ;;
    $logout) pkill xmonad
        ;;
esac
