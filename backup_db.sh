#!/bin/bash
# Backup files

# Backup data
d=$(date +%Y-%m-%d)
echo Today is "$d"

file=msc-thesis-code-data-$d

# Option 1: as zip file in dropbox
zip -r $file data/lookup/* data/geo_processed/*
dir="../../Dropbox/msc-thesis/data-dump/backups"
mv $file.zip $dir