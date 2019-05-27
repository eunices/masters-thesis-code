#!/bin/bash
# Backup files

# Backup data to Dropbox
d=$(date +%Y-%m-%d)
echo Today is "$d"

fn=msc-thesis-code-data-$d.zip
echo "$fn"
zip -r $fn data/*
echo File is backed up as $fn

dir="../../Dropbox/msc-thesis/data-dump/backups"
mv $fn $dir/$fn