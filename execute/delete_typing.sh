#!/bin/bash

cd execute
unset R_HOME

# Get path to loocal 
del_path=$(Rscript -e "cat(readRDS('del_local.rds'))")

rm $del_path