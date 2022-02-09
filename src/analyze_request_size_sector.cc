#include "large_array.h"
#include "trace.h"

class Analyzer : Analyzer_base {
  std::map<uint64_t, std::pair<uint64_t, uint64_t>> reqSize_;
  uint64_t maxLba_;

public:
  void init(char *propertyFileName, char *volume) {
    std::string volumeId(volume);
    trace_.loadProperty(propertyFileName, volume);
    maxLba_ = trace_.getMaxLba(volumeId);
    volume_id_ = std::string(volume);
  }

  void analyze(char *inputTrace)
  {
    uint64_t offset, length, timestamp;
    bool isWrite;
    openTrace(inputTrace);

    fprintf(stderr, "Volume id = %s\n", volume_id_.c_str());

    trace_.myTimer(true, "request sizes");
    while (trace_.readNextRequestFstream(*is_, timestamp, isWrite, offset, length, line2_, true)) {
      if (!isWrite) { // read request
        reqSize_[length].first += 1;
      } else if (isWrite) { // write request
        reqSize_[length].second += 1;
      }
      trace_.myTimer(false, "request sizes");
    }

    std::cout << reqSize_.size() << std::endl;
    for (auto pr : reqSize_) {
      std::cout << volume_id_ << " " << pr.first * 512 
        << " " << pr.second.first << " " << pr.second.second << std::endl;
    }
  }
};

int main(int argc, char *argv[]) {
  Analyzer analyzer;
  analyzer.init(argv[3], argv[1]);
  analyzer.analyze(argv[2]);
}
