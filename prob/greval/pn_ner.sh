#!/bin/sh

awk '/^<w>/ {gsub(/w> .*$/, "w> NP")} {print}'
