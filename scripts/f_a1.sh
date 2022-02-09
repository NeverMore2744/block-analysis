#!/bin/bash

source common.sh

echo "analyze Findings A.1 (Read working set sizes) ..";
fn=`echo $0 | rev | cut -d '/' -f 1 | rev`
if [[ `ls | grep "$fn" | wc -l` -eq 1 ]]; then
  cd ..
fi

analyze_multiple_files "_bs" "_bs" "src/analyze_basic_stats.cc" "basic statistics"
process_file "bs" "_bs" "bs" "src/process_rwfreq.cc" "Read Write Frequency"
cd r
Rscript f_a1.r
