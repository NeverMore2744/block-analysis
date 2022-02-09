#!/bin/bash

source common.sh

echo "analyze Findings B.8 (Traffic across daytime and nighttime) ..";
fn=`echo $0 | rev | cut -d '/' -f 1 | rev`
if [[ `ls | grep "$fn" | wc -l` -eq 1 ]]; then
  cd ..
fi

# Midnight to midnight
analyze_multiple_files "_mid2mid" "_mid2mid" "src/analyze_mid2mid.cc" "Traffic per day" 
process_file "mid2mid" "_mid2mid" "mid2mid" "src/process_mid2mid.cc" "Traffic per day"
cd r
Rscript f_b8.r
