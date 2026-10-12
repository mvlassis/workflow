#! /usr/bin/env bash

instance=$(ps aux | grep -wc "pavucontrol")
if [[ "$instance" -gt 1 ]]; then
	killall -9 "pavucontrol"
else
	pavucontrol --tab=3 &
fi
