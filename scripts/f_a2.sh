#!/bin/bash

source common.sh

echo "analyze Findings A.2 (Request sizes) ..";
fn=`echo $0 | rev | cut -d '/' -f 1 | rev`
if [[ `ls | grep "$fn" | wc -l` -eq 1 ]]; then
  cd ..
fi

analyze_multiple_files "_reqsz" "_reqsz" "src/analyze_request_size_sector.cc" "request sizes" 
process_file "reqsz" "_reqsz" "reqsz" "src/process_reqsize.cc" "Request sizes"
cd r
Rscript f_a2.r

