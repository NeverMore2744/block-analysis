#include <iostream>
#include <unordered_map>
#include <string>
#include <cstdint>
#include <map>
#include <cstdio>
#include <vector>
#include <set>
#include <algorithm>
#include "large_array.h"
#include <sys/time.h>
#include "trace.h"

class Analyzer : Analyzer_base {
  uint64_t nReadReqs_ = 0, nWriteReqs_ = 0;
  uint64_t nReadBlocks_ = 0, nWriteBlocks_ = 0;
  uint64_t nUpdateBlocks_ = 0;
  uint64_t maxLba = 0;
  LargeArray<uint64_t>* writeFreq_;
  LargeArray<uint64_t>* readFreq_;

public:

  void init(char *propertyFileName, char *volume) {
    std::string volumeId(volume);
    trace_.loadProperty(propertyFileName, volume);
    writeFreq_ = new LargeArray<uint64_t>(trace_.getMaxLba(volumeId) + 1);
    readFreq_ = new LargeArray<uint64_t>(trace_.getMaxLba(volumeId) + 1);
  }

  void analyze(char *inputTrace)
  {
    uint64_t offset, length, timestamp;
    bool isWrite;
    openTrace(inputTrace);
    trace_.myTimer(true, "basic statistics");

    while (trace_.readNextRequestFstream(*is_, timestamp, isWrite, offset, length, line2_)) {
      if (isWrite) { // write request
        nWriteReqs_ ++;
        nWriteBlocks_ += length;
        for (uint64_t i = 0; i < length; i += 1) {
          if (writeFreq_->get(offset + i) != 0) nUpdateBlocks_ ++;
          writeFreq_->inc(offset + i);
        }
      } else { // read request
        nReadReqs_ ++;
        nReadBlocks_ += length;
        for (uint64_t i = 0; i < length; i += 1) readFreq_->inc(offset + i);
      }

      trace_.myTimer(false, "basic statistics");
    }

    std::cout << nReadReqs_ << " " << nReadBlocks_ << " ";
    std::cout << nWriteReqs_ << " " << nWriteBlocks_ << " ";
    std::cout << nUpdateBlocks_ << std::endl;

    readFreq_->outputNonZero();
    writeFreq_->outputNonZero();
  }
};

// params: volumeId, filename, propertyfilename
int main(int argc, char *argv[]) {
  Analyzer analyzer;
  analyzer.init(argv[3], argv[1]);
  analyzer.analyze(argv[2]);
}
