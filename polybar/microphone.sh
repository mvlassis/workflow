#!/usr/bin/env bash

if pactl list sinks | grep "Mute" | grep "yes"; then
	echo ""
else
	echo ""
fi	
