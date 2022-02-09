#include "large_array.h"
#include "trace.h"
#include <cassert>

class Analyzer : Analyzer_base {
  // LBA to number of reads and number of writes
  LargeArray<uint64_t> *read_traffic_;
  LargeArray<uint64_t> *write_traffic_;
  LargeArray<uint64_t> *total_traffic_;
  LargeArray<uint64_t> *n_read_requests_;
  LargeArray<uint64_t> *n_write_requests_;
  LargeArray<uint64_t> *n_total_requests_;
  uint64_t maxLba_;

public:
  Analyzer() {
    read_traffic_ = new LargeArray<uint64_t>(24*60);
    write_traffic_ = new LargeArray<uint64_t>(24*60);
    total_traffic_ = new LargeArray<uint64_t>(24*60);
    n_read_requests_ = new LargeArray<uint64_t>(24*60);
    n_write_requests_ = new LargeArray<uint64_t>(24*60);
    n_total_requests_ = new LargeArray<uint64_t>(24*60);
  }

  void init(char *propertyFileName, char *volume) {
    std::string volumeId(volume);
    trace_.loadProperty(propertyFileName, volume);
    maxLba_ = trace_.getMaxLba(volumeId);
    volume_id_ = volumeId;
  }

  void analyze(char *inputTrace)
  {
    uint64_t offset, length, timestamp;
    bool isWrite;

    openTrace(inputTrace);
    trace_.myTimer(true, "mid2mid");

    // timestamp in 1e-7 second
    while (trace_.readNextRequestFstream(*is_, timestamp, isWrite, offset, length, line2_)) {
      uint64_t time_in_min_aligned_zone = 
        ((timestamp / 10000000 + trace_.timeOffsetInSeconds_) % (24 * 60 * 60)) / 60;
      
      if (isWrite) { // write request
        write_traffic_->incValue(time_in_min_aligned_zone, length);
        n_write_requests_->inc(time_in_min_aligned_zone);
      } else { // read request
        read_traffic_->incValue(time_in_min_aligned_zone, length);
        n_read_requests_->inc(time_in_min_aligned_zone);
      }
      total_traffic_->incValue(time_in_min_aligned_zone, length);
      n_total_requests_->inc(time_in_min_aligned_zone);

      trace_.myTimer(false, "mid2mid");
    }

    assert(n_read_requests_->getSum() == trace_.getNumRReq(volume_id_));
    assert(n_write_requests_->getSum() == trace_.getNumWReq(volume_id_));
    assert(read_traffic_->getSum() == trace_.getTrb(volume_id_));
    assert(write_traffic_->getSum() == trace_.getTwb(volume_id_));

    n_read_requests_->outputNonZero();
    n_write_requests_->outputNonZero();
    n_total_requests_->outputNonZero();
    read_traffic_->outputNonZero();
    write_traffic_->outputNonZero();
    total_traffic_->outputNonZero();
  }
};

int main(int argc, char *argv[]) {
  Analyzer analyzer;
  analyzer.init(argv[3], argv[1]);
  analyzer.analyze(argv[2]);
}
