#!/usr/bin/env bash

if pulseaudio-ctl | grep "Is source muted" | grep -q "yes"; then
	echo ""
else
	echo ""
fi
