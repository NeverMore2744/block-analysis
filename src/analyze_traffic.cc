#include "large_array.h"
#include "trace.h"
#include <cassert>

class Analyzer : Analyzer_base {
  // LBA to number of reads and number of writes
  LargeArray<uint64_t> *readReqs_;
  LargeArray<uint64_t> *writeReqs_;
  LargeArray<uint64_t> *readTraffic_;
  LargeArray<uint64_t> *writeTraffic_;
  LargeArray<uint64_t> *updateTraffic_;
  uint64_t maxLba_;

public:
  Analyzer() {
    readReqs_ = new LargeArray<uint64_t>(31*24*60+1);
    writeReqs_ = new LargeArray<uint64_t>(31*24*60+1);
    readTraffic_ = new LargeArray<uint64_t>(31*24*60+1);
    writeTraffic_ = new LargeArray<uint64_t>(31*24*60+1);
    updateTraffic_ = new LargeArray<uint64_t>(31*24*60+1);
  }

  void init(char *propertyFileName, char *volume) {
    volume_id_ = std::string(volume);
    trace_.loadProperty(propertyFileName, volume);
    maxLba_ = trace_.getMaxLba(volume_id_);
    std::cerr << "Traffic: " << volume_id_ << " " << maxLba_ << " " << std::endl;
  }

  void analyze(char *inputTrace)
  {
    uint64_t offset, length, timestamp;
    bool isWrite;

    openTrace(inputTrace);
    trace_.myTimer(true, "traffic");
    uint64_t size_array = (maxLba_ + 8) / 8; 
    uint64_t* wwss = new uint64_t[size_array];
    memset(wwss, 0, sizeof(uint64_t) * size_array);

    // timestamp in 1e-7 second
    while (trace_.readNextRequestFstream(*is_, timestamp, isWrite, offset, length, line2_)) {
      if (offset / 64 > size_array) {
        std::cerr << "Offset too large! (" << offset << " and " << size_array << ")" << std::endl;
        exit(1);
      }

      uint64_t timeInMin = timestamp / 10000000 / 60;
      if (timeInMin > readReqs_->getSize()) {
        std::cerr << timeInMin << " " << trace_.cnt << std::endl;
      }

      if (isWrite) { // write request
        writeTraffic_->incValue(timeInMin, length);
        writeReqs_->inc(timeInMin);
        for (uint64_t i = offset; i < offset + length; i++) {
          if ((wwss[i / 64] & (((uint64_t)1) << (i % 64))) != 0) {
            updateTraffic_->inc(timeInMin);
          }
          wwss[i / 64] |= ((uint64_t)1) << (i % 64); 
        }
      } else { // read request
        readTraffic_->incValue(timeInMin, length);
        readReqs_->inc(timeInMin);
      }

      trace_.myTimer(false, "traffic");
    }

    assert(readReqs_->getSum() == trace_.getNumRReq(volume_id_));
    assert(writeReqs_->getSum() == trace_.getNumWReq(volume_id_));
    assert(readTraffic_->getSum() == trace_.getTrb(volume_id_));
    assert(writeTraffic_->getSum() == trace_.getTwb(volume_id_));
    assert(updateTraffic_->getSum() == trace_.getTub(volume_id_));

    readReqs_->outputNonZero();
    writeReqs_->outputNonZero();
    readTraffic_->outputNonZero();
    writeTraffic_->outputNonZero();
    updateTraffic_->outputNonZero();
    delete wwss;
  }
};

int main(int argc, char *argv[]) {
  setbuf(stderr, NULL);
  Analyzer analyzer;
  analyzer.init(argv[3], argv[1]);
  analyzer.analyze(argv[2]);
}
