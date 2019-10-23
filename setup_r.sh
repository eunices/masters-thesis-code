#!/bin/bash
# This contains hypothetical step if one would like to put the script on a server.
# The steps are also to be followed on a development computer.

# Install R
sudo apt-get install r-base

# Install Java JDK / ensure it is added to environment path
# TODO: 

# Install R packages
Rscript -e "source('install.r')"

