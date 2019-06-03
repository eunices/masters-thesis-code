#!/bin/bash
# Backup files

# Backup data
d=$(date +%Y-%m-%d)
echo Today is "$d"

file=msc-thesis-code-data-$d

# Option 1: as zip file in dropbox
# zip -r $file data/*
# dir="../../Dropbox/msc-thesis/data-dump/backups"
# mv $fn "$dir/$file.zip"

# Option 2: in portable hard disk drive
dir="/mnt/e/msc"
mkdir $dir/$file
cp -R --verbose -n data/* $dir/$file