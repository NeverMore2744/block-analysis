#include "large_array.h"
#include "trace.h"

class Analyzer : Analyzer_base {
  struct Cache {
    Cache(uint64_t capacity, double ratio) {
      if (capacity == 0) capacity = 1;
      capacity_ = capacity;
      ratio_ = ratio;
    }

    std::list<uint64_t> items_;
    std::map<uint64_t, std::list<uint64_t>::iterator> map_;
    uint64_t capacity_;
    uint64_t size_ = 0;
    uint64_t numReadHits_ = 0;
    uint64_t numWriteHits_ = 0;
    double ratio_;

    void update(uint64_t addr, bool isRead) {
      if (map_.find(addr) != map_.end()) {
        size_ -= 1;
        if (isRead) {
          numReadHits_ ++;
        } else {
          numWriteHits_ ++;
        }
        items_.erase(map_[addr]);
      } else if (size_ >= capacity_) {
        size_ -= 1;
        map_.erase(items_.back());
        items_.pop_back();
      }
      size_ += 1;
      items_.push_front(addr);
      map_[addr] = items_.begin();
    }
  };

  std::vector<Cache> caches;

  uint64_t workingSetSize_ = -1ull;

  void initCaches(std::string deviceId) {
    std::cerr << caches.size() << std::endl;
  }

public:

  // initialize properties
  void init(char *propertyFileName, char *volume) {
    std::string volumeId(volume);
    volume_id_ = volumeId;
    trace_.loadProperty(propertyFileName, volume);

    workingSetSize_ = trace_.getUniqueLba(volumeId);
    caches.emplace_back(workingSetSize_ * 0.01, 0.01);
    caches.emplace_back(workingSetSize_ * 0.05, 0.05);
    caches.emplace_back(workingSetSize_ * 0.1, 0.1);
  }

  void analyze(char *inputTrace)
  {
//      caches.emplace_back(workingSetSize_ * 0.01, 0.01);
//      caches.emplace_back(workingSetSize_ * 0.1, 0.1);

      uint64_t offset, length, timestamp;
      bool isWrite;
      uint64_t numReadBlocks = 0, numWriteBlocks = 0;
      openTrace(inputTrace);

      trace_.myTimer(true, "miss ratio");

      while (trace_.readNextRequestFstream(*is_, timestamp, isWrite, offset, length, line2_)) {
        for (uint64_t i = 0; i < length; i += 1) {
          if (!isWrite) {
            numReadBlocks += 1;
          } else {
            numWriteBlocks += 1;
          }
          uint64_t addr = offset + i;
          for (Cache &cache : caches) {
            cache.update(addr, !isWrite);
          }
        }

        trace_.myTimer(false, "miss ratio");
      }

      for (Cache &cache : caches) {
        std::cout << volume_id_ << " " << numReadBlocks << " " << numWriteBlocks << " "  
          << cache.ratio_ << " " << cache.numReadHits_ << " " << cache.numWriteHits_ << std::endl;
      }
  }
};

int main(int argc, char *argv[]) {
  Analyzer analyzer;
  analyzer.init(argv[3], argv[1]);
  analyzer.analyze(argv[2]);
}
