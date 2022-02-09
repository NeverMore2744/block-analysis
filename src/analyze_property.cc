#include "large_array.h"
#include "trace.h"

class Analyzer : Analyzer_base {
  // LBA to number of reads and number of writes
  uint64_t max_lba = 0;
  LargeArray<uint8_t> *lbas_;
  uint64_t arrSize = 100000;

  void resize(uint64_t lba) {
    LargeArray<uint8_t> *newArray;
    std::cerr << "Resize volume " << volume_id_ << " LBA array size from " << arrSize << " to ";
    while (lba >= arrSize) {
      arrSize *= 2; 
    }
    newArray = new LargeArray<uint8_t>(arrSize);
    
    // move
    for (int i = 0; i < (int)lbas_->getSize(); i++) {
      uint8_t v = lbas_->get(i);
      if (v > 0) {
        newArray->put(i, v);
      }
    }

    std::cerr << arrSize << std::endl;
    delete lbas_; 
    lbas_ = newArray;
  }

public:
  void init(char *volume) {
    std::string volumeId(volume);
    volume_id_ = volumeId;
    trace_.loadProperty(nullptr, volume);
  }

  void analyze(char *inputTrace, char *volumeId)
  {
    uint64_t offset, length, timestamp, wss = 0, rwss = 0, wwss = 0, uwss = 0;
    uint64_t n_read_requests = 0, n_write_requests = 0, n_requests = 0;
    uint64_t n_read_blocks = 0, n_write_blocks = 0, n_update_blocks = 0, n_blocks = 0;
    bool isWrite;
    openTrace(inputTrace);
    trace_.myTimer(true, "property");

    lbas_ = new LargeArray<uint8_t>(arrSize);

    while (trace_.readNextRequestFstream(*is_, timestamp, isWrite, offset, length, line2_)) {
      max_lba = std::max(max_lba, offset + length - 1);
      for (uint64_t i = 0; i < length; i += 1) {
        if (offset + i >= arrSize) {
          resize(offset + i);
        }
        uint8_t v = lbas_->get(offset + i);
        v |= 1;

        if (isWrite) {
          if (v & 2) {
            v |= 8;
            n_update_blocks++;
          }
          v |= 2;
        } else {
          v |= 4;
        }
        lbas_->put(offset + i, v);
      }
      n_requests++;
      n_blocks += length;

      if (isWrite) {
        n_write_requests++;
        n_write_blocks += length;
      } else {
        n_read_requests++;
        n_read_blocks += length;
      }
      trace_.myTimer(false, "property");
    }

    for (int i = 0; i < (int)lbas_->getSize(); i++) {
      uint8_t v = lbas_->get(i);
      if (v > 0) wss++;
      if (v & 2) wwss++;
      if (v & 4) rwss++;
      if (v & 8) uwss++;
    }
    std::cout << volumeId << " " << wss << " " << max_lba << " " 
      << n_read_requests << " " << n_read_blocks << " " 
      << n_write_requests << " " << n_write_blocks << " " << n_update_blocks << " " 
      << rwss << " " << wwss << " " << uwss << std::endl;
  }
};

int main(int argc, char *argv[]) {
  Analyzer analyzer;
  analyzer.init(argv[1]);
  analyzer.analyze(argv[2], argv[1]);
}
