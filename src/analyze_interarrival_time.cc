#include "large_array.h"
#include "trace.h"

class Analyzer : public Analyzer_base {
  LargeArray<uint64_t>** intervalHistograms_;
  uint64_t intt_num = 0;

  // diff is in 1e-7 seconds
  void updateInterArrivalTime(uint64_t diff) {
    uint64_t unitIn100ms = diff / 1000000; // change 1e-7 to 1e-1
    uint64_t index, value;
    if (unitIn100ms < 1) { // < 0.1s
      index = 0, value = diff;
    } else if (unitIn100ms < 10) { // < 1s, use 10^-6 s
      index = 1, value = diff / 10;
    } else if (unitIn100ms < 100) { // < 10s, use 10^-4 s
      index = 2, value = diff / 1000;
    } else if (unitIn100ms < 1000) { // < 100s, use 10^-2 s
      index = 3, value = diff / 100000;
    } else if (unitIn100ms < 10000) { // < 1000s, use 10^-1 s
      index = 4, value = diff / 1000000;
    } else {
      index = 5, value = diff / 10000000; // >= 1000s
    }

    intervalHistograms_[index]->inc(value);
    intt_num ++;
  }

public:
  void summary() {
    uint64_t cnt = 0, pct = 1;
    for (int i = 0; i < 6; i++) {
      for (int j = 0; j < intervalHistograms_[i]->getSize(); j++) {
        uint64_t diffCnt = intervalHistograms_[i]->get(j);
        cnt += diffCnt;
        while (pct <= 100 &&intt_num / 100 * pct <= cnt) {
          uint64_t timeIn100ns = j;
          if (i == 1) timeIn100ns *= 10;
          else if (i == 2) timeIn100ns *= 1000;
          else if (i == 3) timeIn100ns *= 100000;
          else if (i == 4) timeIn100ns *= 1000000;
          else if (i == 5) timeIn100ns *= 10000000;
          printf("%s %.7lf %lldth\n", volume_id_.c_str(), (double)timeIn100ns / 10000000, pct);

          pct++;
        }
      }
    }
  }

  void analyze(char* inputTrace) {
    intervalHistograms_ = new LargeArray<uint64_t>*[6];
    intervalHistograms_[0] = new LargeArray<uint64_t>(1 * 1000000); // 0-0.1s, by 10^-7 (totally 10^-1 / 10^-7)
    intervalHistograms_[1] = new LargeArray<uint64_t>(10 * 100000); // 0.1-1s, by 10^-6 (totally 1 / 10^-6)
    intervalHistograms_[2] = new LargeArray<uint64_t>(100 * 1000); // 1-10s, by 10^-4 (totally 10^1 / 10^-4)
    intervalHistograms_[3] = new LargeArray<uint64_t>(1000 * 10); // 10-100s, by 10^-2
    intervalHistograms_[4] = new LargeArray<uint64_t>(10000 * 1); // 100-1000s, by 10^-1
    intervalHistograms_[5] = new LargeArray<uint64_t>(31 * 24 * 3600); // > 1000s, by 1s

    uint64_t offset, length, timestamp;
    uint64_t prevTimestamp = -1ull;
    bool isWrite;
    openTrace(inputTrace);
    trace_.myTimer(true, "interarrival time");

    while (trace_.readNextRequestFstream(*is_, timestamp, isWrite, offset, length, line2_)) {
      if (prevTimestamp != -1ull) {
        if (timestamp < prevTimestamp) {
          continue;
        }
        updateInterArrivalTime(timestamp - prevTimestamp);
        prevTimestamp = timestamp;
      } else {
        prevTimestamp = timestamp;
      }

      trace_.myTimer(false, "interarrival time");
    }

//      for (int i = 0; i < 6; ++i) {
//        intervalHistograms_[i]->outputNonZero();
//      }
    summary();
  }
};

int main(int argc, char *argv[]) {
  Analyzer analyzer;
  analyzer.init(argv[3], argv[1]);
  analyzer.analyze(argv[2]);
}
