#!/bin/bash
# Check if the first argument is empty or unset
if [ -z "$1" ]; then
    echo "Usage: you must enter the name for the node. "
    exit 1
fi
if [ -z "$2" ]; then
    echo "Usage: you must enter the domain for the node. "
    exit 1
fi
echo "Starting $1@$2.keatonsmith.com"
rebar3 shell --apps=$1 --name $1@$2.keatonsmith.com --setcookie package_tracker_cookie