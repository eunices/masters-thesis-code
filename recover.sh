#!/bin/bash
# Recover files

# Recovery filename
date='2019-06-03'
file=msc-thesis-code-data-$d

# Option 1: from zip file in dropbox
# dir="../../Dropbox/msc-thesis/data-dump/backups"
# unzip -r $dir/$file data/*

# Option 2: from portable hard disk drive
dir="/mnt/e/msc"
mkdir data
cp -R $dir/$file data/* 