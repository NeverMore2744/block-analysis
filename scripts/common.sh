#!/bin/bash

# Modify the paths of the analysed traces
ALI_DOWNLOAD_FILE_PATH=""  # The csv trace file you downloaded from the Alibaba GitHub
TENCENT_DOWNLOAD_DIR_PATH="" # The directory you downloaded from the SNIA, containing tgz files
MSRC_DOWNLOAD_DIR_PATH=""    # The directory you dwonloaded from the SNIA, containing two tar files

ALICLOUD_PATH=""        # The directory for storing final AliCloud trace files
TENCENTCLOUD_PATH=""    # The directory for storing final TencentCloud trace files
MSRC_PATH=""            # The directory for storing final MSRC trace files

ALICLOUD_FILE_PATH="./etc/ali_traces"
TENCENTCLOUD_FILE_PATH="./etc/tc_traces"
MSRC_FILE_PATH="./etc/msr_traces"

trace_prefix=("ali" "tc" "msr");
define_names=("ALICLOUD" "TENCENTCLOUD" "MSRC");
display_names=("AliCloud" "TencentCloud" "MSRC");
trace_paths=($ALICLOUD_PATH $TENCENTCLOUD_PATH $MSRC_PATH);
trace_file_paths=($ALICLOUD_FILE_PATH $TENCENTCLOUD_FILE_PATH $MSRC_FILE_PATH);

analyze_multiple_files() {
  bin_suffix=$1;
  output_suffix=$2;
  src=$3;
  disp=$4;
  params=()

  if [[ $# -gt 4 ]]; then
    params=("${@:5}");
    echo "$params[@]"
  fi

  for ((K=0; K<${#define_names[@]}; K++)); do
    bin="bin/${trace_prefix[$K]}${bin_suffix}"
    output_dir="result/${trace_prefix[$K]}${output_suffix}"
    if [[ ! -d $output_dir ]]; then
      mkdir -p $output_dir
    fi
    if [[ ! -d bin ]]; then
      mkdir bin
    fi

    property_file="result/${trace_prefix[$K]}_property.data"
    echo "Analyzing ${display_names[$K]} on $disp ... output at directory $output_dir"

    g++ $src -o $bin -std=c++11 -O3 -D${define_names[$K]} -Werror -Wall
    if [[ $? -ne 0 ]]; then 
      echo "Compile failed"
      exit
    fi

    cat ${trace_file_paths[$K]} | while read line; do
      trace_file=${trace_paths[$K]}/$line.csv
      output=${output_dir}/$line.data
      sz=`ls -s ${output} 2>/dev/null | awk '{print $1;}'`
      if [[ $? -ne 0 || $sz -eq 0 ]]; then  # Not exist, or empty file
        $bin $line $trace_file $property_file ${params[@]} >> $output
        if [[ $? -ne 0 ]]; then
          echo "have error on volume $line, break" >> error_msg.txt
            sleep 2
        fi
      else
        echo "Volume $line in ${display_names[$K]} is analyzed before, skip"
      fi
    done
  done
}

process_file() {
  bin_suffix=$1;
  analyzed_suffix=$2
  output_suffix=$3;
  src=$4;
  disp=$5;
  params=()

  if [[ $# -gt 5 ]]; then
    params=("${@:6}");
    echo "$params"
  fi

  for ((K=0; K<${#define_names[@]}; K++)); do
    bin="bin/${bin_suffix}"
    output_dir="processed/$output_suffix/"
    analyzed_dir="result/${trace_prefix[$K]}${analyzed_suffix}/"
    property_file="result/${trace_prefix[$K]}_property.data"

    echo "bin = $bin, analyzed_dir = $analyzed_dir, output_dir = $output_dir"
    if [[ -d $output_dir ]]; then  # Continue if the directory has been created before
      continue
    fi

    if [[ ! -d $output_dir ]]; then
      mkdir -p $output_dir
    fi
    if [[ ! -d bin ]]; then
      mkdir bin
    fi

    echo "Processing ${display_names[$K]} on $disp ... output at directory $output_dir"

    g++ $src -o $bin -std=c++11 -Werror -Wall
    if [[ $? -ne 0 ]]; then 
      echo "Compile failed"
      exit
    fi

    output=tmp.txt
    $bin $analyzed_dir ${trace_file_paths[$K]} ${params[@]} ${property_file} > $output

    cat $output

    cat $output | while read line; do
      mv $line $output_dir/${trace_prefix[$K]}_$line
      echo "mv $line $output_dir/${trace_prefix[$K]}_$line"
    done
    rm -f $output 

  done
}

merge() {
  input_prefix=$1
  input_suffix=$2;
  output_suffix=$3;
  
  for ((K=0; K<${#define_names[@]}; K++)); do
    output="result/${trace_prefix[$K]}${output_suffix}"
    rm -f $output
    
    cat ${trace_file_paths[$K]} | while read line; do
      input="result/${input_prefix}${trace_prefix[$K]}${input_suffix}${line}.data"
      if [[ ! -f $input ]]; then
        echo "Error: input $input not exist"
        return 1
      fi
      cat $input >> $output
    done
  done
}
