#!/bin/bash
# Backup files

# Backup data
d=$(date +%Y-%m-%d)
echo Today is "$d"

# Option 2: in portable hard disk drive
file=msc-thesis-code-data-$d
dir="/mnt/e/msc"
mkdir -p $dir/$file
cp -R --verbose -n data/* $dir/$file
echo "Files copied to $dir/$file"