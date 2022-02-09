#!/bin/bash

source common.sh

echo "analyze Findings B.9 (Randomness) ..";
fn=`echo $0 | rev | cut -d '/' -f 1 | rev`
if [[ `ls | grep "$fn" | wc -l` -eq 1 ]]; then
  cd ..
fi

analyze_multiple_files "_rand" "_rand" "src/analyze_randomness.cc" "randomness" 32 32 
merge "" "_rand/" "_rand.data"
cd r
Rscript f_b9.r
