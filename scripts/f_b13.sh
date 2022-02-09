#!/bin/bash

source common.sh

echo "analyze Findings B.13 (RAW and WAW) ..";
fn=`echo $0 | rev | cut -d '/' -f 1 | rev`
if [[ `ls | grep "$fn" | wc -l` -eq 1 ]]; then
  cd ..
fi

analyze_multiple_files "_arw" "_arw" "src/analyze_after_read_write.cc" "RAR, WAR, RAW, WAW"
process_file "arw" "_arw" "arw" "src/process_arw.cc" "RWARW"
cd r
Rscript f_b13.r
