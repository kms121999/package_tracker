#!/bin/bash
# Check if the first argument is empty or unset
if [ -z "$1" ]; then
    echo "Usage: you must enter the name for the node. "
    exit 1
fi
rebar3 shell --apps=$1 --name $1@backend.keatonsmith.com --setcookie package_tracker_cookie