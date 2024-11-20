#!/bin/bash
# Check if the first argument is empty or unset
if [ -z "$1" ]; then
    echo "Usage: you must enter the name for the node. "
    exit 1
fi
echo "Starting $1@$1.keatonsmith.com"
rebar3 shell --relname=$1 --name $1@$1.keatonsmith.com --setcookie package_tracker_cookie
