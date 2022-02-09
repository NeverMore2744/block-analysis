#include "large_array.h"
#include "trace.h"

class Analyzer : Analyzer_base {
  char volume_cstr[100];

  LargeArray<uint64_t>* indexMap_;
  LargeArray<uint64_t>* lastTimestamp_;

  LargeArray<uint64_t>* intervalHistogramByTime_;
//  LargeArray<uint64_t>* intervalHistogramByDataAmount_;

  uint64_t currentId_ = 1;

  uint64_t getDistance(uint64_t off) {
    uint64_t distance = currentId_ - indexMap_->get(off);
    distance /= 256;
    if (distance >= n_blocks_ * 8 / 256) distance = n_blocks_ * 8 / 256;
    return distance;
  }

  uint64_t getTimeIn01Sec(uint64_t off, uint64_t timestamp) {
    uint64_t diff = timestamp - lastTimestamp_->get(off);
    diff /= 1000000; // Unit: 0.1 s
    return diff;
  }

public:

  // initialize properties
  void init(char *propertyFileName, char *volume) {
    std::string volumeId(volume);
    volume_id_ = volumeId;

    strcpy(volume_cstr, volume);
    trace_.loadProperty(propertyFileName, volume);

    uint64_t maxLba = trace_.getMaxLba(volumeId);
    n_blocks_ = maxLba + 1;

    indexMap_ = new LargeArray<uint64_t>(n_blocks_);
    lastTimestamp_ = new LargeArray<uint64_t>(n_blocks_);
    // every 256 blocks in one bucket, in total maintain (maxLba / 4096 + 1) * 8

    intervalHistogramByTime_ = new LargeArray<uint64_t>(31 * 24 * 36000);
    // intervalHistogramByDataAmount_ = new LargeArray<uint64_t>(n_blocks_ * 8 / 256 + 1);
  }

  void analyze(char *input_trace_file) {
      uint64_t offset, length, timestamp;
      bool isWrite;
      openTrace(input_trace_file);

      trace_.myTimer(true, "update distance");

//      bool first = true;
      bool skip;
      uint64_t skipped = 0;
//      uint64_t lastReqTimestamp;

      while (trace_.readNextRequestFstream(*is_, timestamp, isWrite, offset, length, line2_)) {
//        if (first) {
//          first = false;
//        } else if (timestamp < lastReqTimestamp) {
//          continue;
//        }
//        lastReqTimestamp = timestamp;

        if (isWrite) { // write request
          for (uint64_t i = 0; i < length; i += 1) {
            if (indexMap_->get(offset + i) != 0) {
//              intervalHistogramByDataAmount_->inc(getDistance(offset + i));

              skip = false;
              if (lastTimestamp_->get(offset + i) > timestamp) {
                skip = true;
                skipped++;
              }

              uint64_t updateIntervalIn01Sec = getTimeIn01Sec(offset + i, timestamp);

#if defined(TENCENTCLOUD)
              // last timestamp before Oct.8th 1:01 AM, and 
              // curr timestamp after Oct.8th 1:59 AM 
              last = lastTimestamp_->get(offset + i);
              if (!skip && last <= 608460ull * 1e7 && timestamp >= 611940ull * 1e7) {
                skip = true;
              }
#endif

              if (!skip){
                if (updateIntervalIn01Sec >= intervalHistogramByTime_->getSize()) {
                  intervalHistogramByTime_->inc(intervalHistogramByTime_->getSize() - 1);
                } else {
                  intervalHistogramByTime_->inc(updateIntervalIn01Sec);
                }
              }
            }

            lastTimestamp_->put(offset + i, timestamp);
            indexMap_->put(offset + i, currentId_++);
          }
        }

        trace_.myTimer(false, "update distance");
      }

      std::cerr << "Skipped " << skipped << std::endl;

      intervalHistogramByTime_->outputNonZero();
//      intervalHistogramByDataAmount_->outputNonZero();
  }
};

int main(int argc, char *argv[]) {
  Analyzer analyzer;
  analyzer.init(argv[3], argv[1]);
  analyzer.analyze(argv[2]);
}
