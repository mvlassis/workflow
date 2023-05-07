#!/usr/bin/env bash

usb_print() {
    devices=$(lsblk -Jfplno NAME,TYPE,FSTYPE,LABEL,RM,SIZE,MOUNTPOINT,VENDOR)
    output=""
    counter=0

    for unmounted in $(echo "$devices" | jq -r '.blockdevices[] | select(.fstype != null) | select(.rm == true) | select(.mountpoint == null) | .name'); do
        # unmounted=$(echo "$unmounted" | tr -d "[:digit:]")
        unmounted=$(echo "$devices" | jq -r '.blockdevices[] | select(.name == "'"$unmounted"'") | .label')
        unmounted=$(echo "$unmounted" | tr -d ' ')

        if [ $counter -eq 0 ]; then
            space=""
        else
            space="  "
        fi
        counter=$((counter + 1))

        # output="$output$space#1 $unmounted"
		# Uses primary color
		output="${output}${space}%{F#F0C674}%{F-} ${unmounted}"
    done

    for mounted in $(echo "$devices" | jq -r '.blockdevices[] | select(.fstype != null) | select(.rm == true) | select(.mountpoint != null) | .name'); do
		# mounted=$(echo "$mounted" | tr -d "[:digit:]")
		mounted=$(echo "$devices" | jq -r '.blockdevices[] | select(.name == "'"$mounted"'") | .label + "/" + .size')
		mounted=$(echo "$mounted" | tr -d ' ')
        if [ $counter -eq 0 ]; then
            space=""
        else
            space="  "
        fi
        counter=$((counter + 1))

        # output="${output}${space}#2 $mounted"
		output="${output}${space}%{F#F0C674}%{F-} ${mounted}"
    done

    echo "$output"
}

usb_update() {
    pid=$(cat "$path_pid")

    if [ "$pid" != "" ]; then
        kill -10 "$pid"
    fi
}

path_pid="/tmp/polybar-system-usb-udev.pid"

case "$1" in
    --update)
        usb_update
        ;;
    --mount)
        devices=$(lsblk -Jfplno NAME,TYPE,FSTYPE,RM,MOUNTPOINT)

        for mount in $(echo "$devices" | jq -r '.blockdevices[] | select(.fstype != null) | select(.rm == true) | select(.mountpoint == null) | .name'); do
            udisksctl mount --no-user-interaction -b "$mount"

            # mountpoint=$(udisksctl mount --no-user-interaction -b $mount)
            # mountpoint=$(echo $mountpoint | cut -d " " -f 4- | tr -d ".")
            # terminal -e "bash -lc 'filemanager $mountpoint'"
        done
        usb_update
        ;;
    --unmount)
        devices=$(lsblk -Jfplno NAME,FSTYPE,TYPE,RM,MOUNTPOINT)

        for unmount in $(echo "$devices" | jq -r '.blockdevices[] | select(.fstype != null) | select(.rm == true) | select(.mountpoint != null) | .name'); do
            udisksctl unmount --no-user-interaction -b "$unmount"
            udisksctl power-off --no-user-interaction -b "$unmount"
        done

        usb_update
        ;;
    *)
        echo $$ > $path_pid

        trap exit INT
        trap "true" USR1

        while true; do
            usb_print

            sleep 8 &
            wait
        done
        ;;
esac
