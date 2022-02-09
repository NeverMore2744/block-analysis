#!/bin/bash

source common.sh

echo "analyze Findings B.3 (Burstiness diversity) ..";
fn=`echo $0 | rev | cut -d '/' -f 1 | rev`
if [[ `ls | grep "$fn" | wc -l` -eq 1 ]]; then
  cd ..
fi

analyze_multiple_files "_traffic" "_traffic" "src/analyze_traffic.cc" "traffic" 
process_file "traffic" "_traffic" "traffic" "src/process_traffic.cc" "Traffic"
cd r
Rscript f_b3.r
