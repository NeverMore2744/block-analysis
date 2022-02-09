#!/bin/bash

source common.sh

echo "analyze Findings B.16 (Miss ratios) ..";
fn=`echo $0 | rev | cut -d '/' -f 1 | rev`
if [[ `ls | grep "$fn" | wc -l` -eq 1 ]]; then
  cd ..
fi

analyze_multiple_files "_mr" "_mr" "src/analyze_miss_ratio.cc" "miss ratios" 
merge "" "_mr/" "_mr.data"
cd r
Rscript f_b16.r
