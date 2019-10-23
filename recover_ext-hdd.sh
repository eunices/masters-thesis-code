#!/bin/bash
# Recover files

# Recovery filename
date='2019-06-03'
file=msc-thesis-code-data-$d

# From portable hard disk drive
dir="/mnt/e/msc"
mkdir data
cp -R $dir/$file data/*