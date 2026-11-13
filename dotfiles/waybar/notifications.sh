#!/usr/bin/env bash

if ! command -v dunstctl > /dev/null 2>&1; then
    printf '%s\n' '{"text":"💬","tooltip":"dunstctl is not installed","class":"unavailable"}'
    exit 0
fi

if [[ "$(dunstctl is-paused 2> /dev/null)" == "true" ]]; then
    printf '%s\n' '{"text":"🔕","tooltip":"Notifications disabled — click to enable","class":"disabled"}'
else
    printf '%s\n' '{"text":"🔔","tooltip":"Notifications enabled — click to disable","class":"enabled"}'
fi
