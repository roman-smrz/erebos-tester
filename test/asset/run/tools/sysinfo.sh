#!/bin/sh

while read cmd; do
    case "$cmd" in
        network-info)
            ip -o addr show | sed -e 's/[0-9]*: \([a-z0-9]*\).*inet6\? \([0-9a-f:.]*\).*/ip \1 \2/'
            ;;
    esac
done
