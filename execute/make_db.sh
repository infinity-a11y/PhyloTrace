#!/bin/bash 

cd execute
unset R_HOME

db_directory=$(Rscript -e "cat(readRDS('new_db.rds'))") 
mkdir "$db_directory/Database"
