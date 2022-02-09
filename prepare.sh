#!/bin/bash

source scripts/common.sh
cd scripts/
./split.sh

flag=0

check() {
  dir_path=$1
  files_path=$2
  cat $files_path | while read line; do
    filename=${dir_path}/${line}.csv
    if [[ ! -f $filename ]]; then
      echo "not exist: $filename ... exiting"
      return 1 
    fi
  done
}

echo "Check whether trace files exist..."
echo "AliCloud ...." 
check $ALICLOUD_PATH $ALICLOUD_FILE_PATH
if [[ $? -eq 1 ]]; then
  exit
fi

echo "TencentCloud ...." 
check $TENCENTCLOUD_PATH $TENCENTCLOUD_FILE_PATH
if [[ $? -eq 1 ]]; then
  exit
fi

echo "MSRC ...." 
check $MSRC_PATH $MSRC_FILE_PATH
if [[ $? -eq 1 ]]; then
  exit
fi


echo "Preparation for trace properties  .. ";
analyze_multiple_files "_property" "_property" "src/analyze_property.cc" "properties"
merge "" "_property/" "_property.data" 

analyze_multiple_files "_bs" "_bs" "src/analyze_basic_stats.cc" "basic statistics"
process_file "bs" "_bs" "bs" "src/process_rwfreq.cc" "Read Write Frequency"

echo ""
