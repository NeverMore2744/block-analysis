#!/bin/bash

source common.sh
cd ..

if [[ ! -d bin ]]; then
  mkdir bin
fi

################# Split the Alibaba Cloud traces
g++ src/split.cc -o bin/split -std=c++11 -O3 -DALICLOUD
if [[ ! -f $ALI_DOWNLOAD_FILE_PATH ]]; then
  echo "ALI_DOWNLOAD_FILE_PATH not set or not exist; please set in scripts/common.sh"
  exit
fi
if [[ ! -d $ALICLOUD_PATH ]]; then
  echo "ALICLOUD_PATH not set or not exist; please set in scripts/common.sh"
  exit
fi
bin/split $ALI_DOWNLOAD_FILE_PATH $ALICLOUD_PATH

################# Split the Tencent Cloud traces
g++ src/split.cc -o bin/split -std=c++11 -Wall -Werror -O3 -DTENCENTCLOUD
if [[ ! -d $TENCENT_DOWNLOAD_DIR_PATH ]]; then
  echo "TENCENT_DOWNLOAD_DIR_PATH not set or not exist; please set in scripts/common.sh"
  exit
fi
if [[ ! -d $TENCENTCLOUD_PATH ]]; then
  echo "TENCENTCLOUD_PATH not set or not exist; please set in scripts/common.sh"
  exit
fi

for tgz_file in `ls ${TENCENT_DOWNLOAD_DIR_PATH}/*.tgz`; do
  echo "Extracting $tgz_file"
  name=`echo $tgz_file | rev | cut -d'/' -f 1 | rev | cut -d'.' -f 1`
  tar xzf $tgz_file 
  INPUT="cbs_trace1/atc_2020_trace/trace_ori/${name}"
  echo "Spliting $INPUT"
  bin/split $INPUT $TENCENTCLOUD_PATH
  rm $INPUT
done

################# Split the MSRC traces
if [[ ! -d $MSRC_DOWNLOAD_DIR_PATH ]]; then
  echo "MSRC_DOWNLOAD_DIR_PATH not set or not exist; please set in scripts/common.sh"
  exit
fi
if [[ ! -d $MSRC_PATH ]]; then
  echo "MSRC_PATH not set or not exist; please set in scripts/common.sh"
  exit
fi
f1="${MSRC_DOWNLOAD_DIR_PATH}/msr-cambridge1.tar"
f2="${MSRC_DOWNLOAD_DIR_PATH}/msr-cambridge2.tar"
if [[ ! -f $f1 ]]; then
  echo "$f1 not exist"
  exit
fi
if [[ ! -f $f2 ]]; then
  echo "$f2 not exist"
  exit
fi

tar xvf $f1
tar xvf $f2
for tgz_file in `ls ${MSRC_DOWNLOAD_DIR_PATH}/MSR-Cambridge/*.gz`; do
  echo "Extracting $tgz_file"
  gzip -d $tgz_file
done
mv ${MSRC_DOWNLOAD_DIR_PATH}/MSR-Cambridge/*.csv ${MSRC_PATH}/
