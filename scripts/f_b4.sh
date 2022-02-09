#!/bin/bash

source common.sh

echo "analyze Findings B.4 (Interarrival time) ..";
fn=`echo $0 | rev | cut -d '/' -f 1 | rev`
if [[ `ls | grep "$fn" | wc -l` -eq 1 ]]; then
  cd ..
fi

analyze_multiple_files "_int" "_int" "src/analyze_interarrival_time.cc" "interarrival time" 
cd r
Rscript f_b4.r
