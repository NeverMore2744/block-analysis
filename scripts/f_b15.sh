#!/bin/bash

source common.sh

echo "analyze Findings B.15 (Update intervals) ..";
fn=`echo $0 | rev | cut -d '/' -f 1 | rev`
if [[ `ls | grep "$fn" | wc -l` -eq 1 ]]; then
  cd ..
fi

analyze_multiple_files "_ud" "_ud" "src/analyze_update_distance.cc" "update distances" 
process_file "ud" "_ud" "ud" "src/process_ud.cc" "Update distance"
cd r
Rscript f_b15.r
