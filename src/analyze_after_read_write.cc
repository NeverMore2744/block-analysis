#include "large_array.h"
#include "trace.h"
#include <cassert>

class Analyzer : Analyzer_base {
  LargeArray<uint64_t>* lba2index_;
  LargeArray<uint64_t>* indexMap_;
  LargeArray<uint64_t>* lastState_;

  struct {
    LargeArray<uint64_t>* intervalHistogramByTime_;
//    LargeArray<uint64_t>* intervalHistogramByDataAmount_;
  } rar_, war_, raw_, waw_;

  uint64_t currentId_ = 0;

  uint64_t getTimeWithLastTime(uint64_t index, uint64_t timestamp, uint64_t lastTimestamp) {
    if (timestamp < lastTimestamp) {
      return -1llu;
    }
    return (timestamp - lastTimestamp) / 1000000; // Unit: 0.1 s
  }

public:

  // initialize properties
  void init(char *propertyFileName, char *volume) {
    std::string volumeId(volume);
    trace_.loadProperty(propertyFileName, volume);
    volume_id_ = std::string(volume);

    uint64_t maxLba = trace_.getMaxLba(volumeId);
    uint64_t wss = trace_.getUniqueLba(volumeId);

    n_blocks_ = maxLba + 1;

    lba2index_ = new LargeArray<uint64_t>(n_blocks_);
    indexMap_ = new LargeArray<uint64_t>(wss + 10);
    lastState_ = new LargeArray<uint64_t>(wss + 10);

    rar_.intervalHistogramByTime_ = new LargeArray<uint64_t>(31 * 24 * 36000);
    war_.intervalHistogramByTime_ = new LargeArray<uint64_t>(31 * 24 * 36000);

    // every 256 blocks in one bucket, in total maintain (maxLba / 4096 + 1) * 8
    waw_.intervalHistogramByTime_ = new LargeArray<uint64_t>(31 * 24 * 36000);
//    waw_.intervalHistogramByDataAmount_ = new LargeArray<uint64_t>(n_blocks_ * 8 / 256 + 1);

    // every 256 blocks in one bucket, in total maintain (maxLba / 4096 + 1) * 8
    raw_.intervalHistogramByTime_ = new LargeArray<uint64_t>(31 * 24 * 36000);
//    raw_.intervalHistogramByDataAmount_ = new LargeArray<uint64_t>(n_blocks_ * 8 / 256 + 1);

  }

  void analyze(char *inputTrace) {
    uint64_t offset, length, timestamp;
    bool isWrite;
    openTrace(inputTrace);

    bool skip = false;

    trace_.myTimer(true, "{RW}A{RW}");

    uint64_t wssPtr = 1, lba, index, last;
    uint64_t time_interval;
    uint64_t skipped_read_blocks = 0;
    uint64_t skipped_write_blocks = 0;
    uint64_t skipped_read_blocks_reversed = 0;
    uint64_t skipped_write_blocks_reversed = 0;
    uint64_t skipped_reason = 0;  // first req: 0, time reversed: 1 

    while (trace_.readNextRequestFstream(*is_, timestamp, isWrite, offset, length, line2_)) {
      for (uint64_t i = 0; i < length; i += 1) {
        lba = offset + i;
        index = lba2index_->get(lba);
        skip = false;
        skipped_reason = 0;

        if (index == 0) { // First request
          index = wssPtr++;
          lba2index_->put(lba, index);
          skip = true;
        } 

        last = lastState_->get(index);

#if defined(TENCENTCLOUD)
        // last timestamp before Oct.8th 1:01 AM, and 
        // curr timestamp after Oct.8th 1:59 AM 
        if (!skip && last / 256 <= 608460ull * 1e7 && timestamp >= 611940ull * 1e7) {
          skip = true;
        }
#endif

        if (timestamp < last / 256) {  // Timestamp reversed, ignore this
          skip = true;
          skipped_reason = 1;
        }

        if (!skip) {
          time_interval = getTimeWithLastTime(index, timestamp, last / 256);

          if (!isWrite) {
            if (last % 256 == 'W') {
              raw_.intervalHistogramByTime_->inc(time_interval);
            } else if (last % 256 == 'R') {
              rar_.intervalHistogramByTime_->inc(time_interval);
            } 

          } else {
            if (last % 256 == 'W') {
              waw_.intervalHistogramByTime_->inc(time_interval);
            } else if (last % 256 == 'R') {
              war_.intervalHistogramByTime_->inc(time_interval);
            }
          }

        } else if (!isWrite) {  // For validation
          skipped_read_blocks++;
          skipped_read_blocks_reversed += skipped_reason;
        } else {
          skipped_write_blocks++;
          skipped_write_blocks_reversed += skipped_reason;
        }

        if (!isWrite) {
          lastState_->put(index, (timestamp * 256) + 'R');
        } else {
          lastState_->put(index, (timestamp * 256) + 'W');
          indexMap_->put(index, currentId_++);
        }
      }

      trace_.myTimer(false, "{RW}A{RW}");
    }

    // Check
    uint64_t wss = trace_.getUniqueLba(volume_id_);
    if (wss != wssPtr - 1) {
      std::cerr << "wss not match: " << wss << " vs " << wssPtr - 1 << std::endl; 
      exit(1);
    }

    uint64_t n_read_blocks = trace_.getTrb(volume_id_);
    uint64_t n_write_blocks = trace_.getTwb(volume_id_);
    uint64_t total_rar = rar_.intervalHistogramByTime_->getSum();
    uint64_t total_war = war_.intervalHistogramByTime_->getSum();
    uint64_t total_raw = raw_.intervalHistogramByTime_->getSum();
    uint64_t total_waw = waw_.intervalHistogramByTime_->getSum();

    uint64_t recorded_read_blocks = total_rar + total_raw; 
    uint64_t recorded_write_blocks = total_war + total_waw; 
    uint64_t total_requests = n_read_blocks + n_write_blocks;
    uint64_t expected = total_requests - wss;
    uint64_t skipped_reversed = 
      skipped_read_blocks_reversed + skipped_write_blocks_reversed;

    if (recorded_read_blocks + recorded_write_blocks != 
        expected - skipped_reversed) {
      fprintf(stderr, "record %lu vs %lu = (req) %lu - (wss) %lu - (rev) %lu\n",
         recorded_read_blocks + recorded_write_blocks, 
         expected - skipped_reversed, 
         total_requests, wss, skipped_reversed);
      assert(recorded_read_blocks + recorded_write_blocks <= 
          expected - skipped_reversed); 
    }

    if (n_read_blocks != recorded_read_blocks + skipped_read_blocks) {
      std::cerr << "trb not match: " << n_read_blocks << " vs " 
        << recorded_read_blocks << " + " << skipped_read_blocks << std::endl; 
      exit(1);
    }
    if (n_write_blocks != recorded_write_blocks + skipped_write_blocks) {
      std::cerr << "twb not match: " << n_write_blocks << " vs " 
        << recorded_write_blocks << " + " << skipped_write_blocks << std::endl; 
      exit(1);
    }

    std::cerr << "Output: rar time (total " << total_rar << ")" << std::endl;
    rar_.intervalHistogramByTime_->outputNonZero();
    std::cerr << "Output: war time (total " << total_war << ")" << std::endl;
    war_.intervalHistogramByTime_->outputNonZero();
    std::cerr << "Output: raw time (total " << total_raw << ")" << std::endl;
    raw_.intervalHistogramByTime_->outputNonZero();
    //      std::cerr << "Output: raw data amount" << std::endl;
    //      raw_.intervalHistogramByDataAmount_->outputNonZero();
    std::cerr << "Output: waw time (total " << total_waw << ")" << std::endl;
    waw_.intervalHistogramByTime_->outputNonZero();

    std::cerr << "Skipped read blocks: " << skipped_read_blocks 
      << " (" << 100.0 * skipped_read_blocks / n_read_blocks << " %, " 
      << 100.0 * skipped_read_blocks_reversed / n_read_blocks << " %)" << std::endl;
    std::cerr << "Skipped write blocks: " << skipped_write_blocks 
      << " (" << 100.0 * skipped_write_blocks / n_write_blocks << " %, " 
      << 100.0 * skipped_write_blocks_reversed / n_write_blocks << " %)" << std::endl;
    //      std::cerr << "Output: waw data amount" << std::endl;
    //      waw_.intervalHistogramByDataAmount_->outputNonZero();
  }
};

int main(int argc, char *argv[]) {
  Analyzer analyzer;
  analyzer.init(argv[3], argv[1]);
  analyzer.analyze(argv[2]);

  return 0;
}
