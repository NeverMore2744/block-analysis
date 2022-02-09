#include "large_array.h"
#include "trace.h"

class Analyzer : Analyzer_base {

  std::string volume_id_;
  uint64_t n_blocks_ = -1ull;

  uint64_t getLbaDistance(uint64_t lba1, uint64_t lba2) {
    uint64_t distance = (lba1 < lba2) ? lba2 - lba1 : lba1 - lba2;
    if (distance >= n_blocks_ / 2) {
      distance = n_blocks_ / 2;
    }
    return distance;
  }

public:

  void init(char *propertyFileName, char *volume) {
    std::string volumeId(volume);
    volume_id_ = volumeId;
    trace_.loadProperty(propertyFileName, volume);

    uint64_t maxLba = trace_.getMaxLba(volumeId);
    n_blocks_ = maxLba + 1;
  }

  void analyze(char *inputTrace, uint64_t recentRequestNumber, uint64_t windowSize)
  {
    uint64_t offset, length, timestamp;
    bool isWrite;
    std::vector<uint64_t> nextLbas;
    int nextPtr = 0;
    openTrace(inputTrace);

    trace_.myTimer(true, "randomness");
    uint64_t ansRandom = 0, ansAll = 0;

    while (trace_.readNextRequestFstream(*is_, timestamp, isWrite, offset, length, line2_)) {
      uint64_t minDis = n_blocks_, dis;

      if (nextLbas.size() < recentRequestNumber) {
        nextLbas.push_back(offset);
        nextPtr = 0;
      } else {
        minDis = n_blocks_;
        for (int k = 0; k < (int)nextLbas.size(); k++) {
          dis = getLbaDistance(offset, nextLbas[k]);
          minDis = (minDis > dis) ? dis : minDis;
        }

        if (minDis < windowSize) ansRandom++;
        ansAll++;

        nextLbas[nextPtr] = offset;
        nextPtr = (nextPtr + 1) % recentRequestNumber;

        trace_.myTimer(false, "randomness");
      }
    }

    assert(ansAll == trace_.getNumRReq(volume_id_) + 
        trace_.getNumWReq(volume_id_) - recentRequestNumber);

    std::cout << volume_id_ << " " << recentRequestNumber << " " << windowSize << " " 
      << ansRandom << " " << ansAll << " " << std::endl;
  }
};

int main(int argc, char *argv[]) {
  uint64_t recentRequestNumber, windowSize;
  if (argc < 5) {
    std::cerr << "Input error: <volume> <trace file> <property> <recentRequestNumber> <windowSize>" 
      << std::endl;
    return 1;
  }
  sscanf(argv[4], "%lu", &recentRequestNumber);
  sscanf(argv[5], "%lu", &windowSize);

  Analyzer analyzer;
  analyzer.init(argv[3], argv[1]);
  analyzer.analyze(argv[2], recentRequestNumber, windowSize);
}
