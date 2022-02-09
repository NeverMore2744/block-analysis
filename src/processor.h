#include<string>
#include<fstream>
#include<iostream>
#include<vector>
#include<map>
#include<set>
#include<cstring>
#include<cassert>
#include<cmath>
#include<algorithm>
#include<unordered_set>
#include<unordered_map>
#include<queue>

class Processor_base {
  public:
  std::unordered_map<std::string, std::pair<uint64_t, uint64_t>> properties;
  std::unordered_map<std::string, uint64_t> volume_n_read_requests;
  std::unordered_map<std::string, uint64_t> volume_n_read_blocks;
  std::unordered_map<std::string, uint64_t> volume_n_write_requests;
  std::unordered_map<std::string, uint64_t> volume_n_write_blocks;
  std::unordered_map<std::string, uint64_t> volume_n_update_blocks;
  std::unordered_map<std::string, uint64_t> volume_rwss;
  std::unordered_map<std::string, uint64_t> volume_wwss;
  std::unordered_map<std::string, uint64_t> volume_uwss;

  std::string line_, volume_id_;
  std::filebuf fb_;
  std::istream* is_;
  std::vector<std::string> filenames_;

  Processor_base() {
    is_ = nullptr;
  }

  ~Processor_base() {
    delete is_;
  }

  void loadProperty(const char *propertyFile) {
    FILE *f = nullptr; 
    uint64_t uniqueLba, maxLba;
    uint64_t n_read_requests, n_read_blocks;
    uint64_t n_write_requests, n_write_blocks, n_update_blocks;
    uint64_t rwss, wwss, uwss;

    std::string volumeId;

    if (propertyFile != nullptr) {
      f = fopen(propertyFile, "r");
      if (f == nullptr) {
        std::cerr << "Propertyfile " << propertyFile << " not exist." << std::endl;
        exit(1);
      }

      char tmp[200];
      char tmpVolume[200];

      char* retChar = fgets(tmp, 200, f);
      int ret;

      while (retChar) {
        ret = sscanf(tmp, "%s %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu", 
            tmpVolume, &uniqueLba, &maxLba, 
            &n_read_requests, &n_read_blocks,
            &n_write_requests, &n_write_blocks, 
            &n_update_blocks, &rwss, &wwss, &uwss); 

        if (ret < 3) {
          std::cerr << "At least three items in each line of property\n";
          exit(1);
        }

        volumeId = std::string(tmpVolume);
        properties[volumeId] = std::make_pair(uniqueLba, maxLba);

        if (ret >= 4) {
          volume_n_read_requests[volumeId] = n_read_requests; 
        } 
        if (ret >= 5) {
          volume_n_read_blocks[volumeId] = n_read_blocks; 
        } 
        if (ret >= 6) {
          volume_n_write_requests[volumeId] = n_write_requests; 
        } 
        if (ret >= 7) {
          volume_n_write_blocks[volumeId] = n_write_blocks; 
        } 
        if (ret >= 8) {
          volume_n_update_blocks[volumeId] = n_update_blocks; 
        } 
        if (ret >= 9) {
          volume_rwss[volumeId] = rwss; 
        } 
        if (ret >= 10) {
          volume_wwss[volumeId] = wwss; 
        } 
        if (ret >= 11) {
          volume_uwss[volumeId] = uwss; 
        } 

        retChar = fgets(tmp, 200, f);
      }
    }
  }

  uint64_t getUniqueLba(std::string volumeId) {
    return properties[volumeId].first;
  }

  uint64_t getMaxLba(std::string volumeId) {
    return properties[volumeId].second;
  }

  uint64_t getNumRReq(std::string volumeId) {
    return volume_n_read_requests[volumeId];
  }

  uint64_t getNumWReq(std::string volumeId) {
    return volume_n_write_requests[volumeId];
  }

  uint64_t getTrb(std::string volumeId) {
    return volume_n_read_blocks[volumeId];
  }

  uint64_t getTwb(std::string volumeId) {
    return volume_n_write_blocks[volumeId];
  }

  uint64_t getTub(std::string volumeId) {
    return volume_n_update_blocks[volumeId];
  }

  uint64_t getRwss(std::string volumeId) {
    return volume_rwss[volumeId];
  }

  uint64_t getWwss(std::string volumeId) {
    return volume_wwss[volumeId];
  }

  uint64_t getUwss(std::string volumeId) {
    return volume_uwss[volumeId];
  }


  void openVolumeFile(const char* file) {
    if (!fb_.open(file, std::ios::in)) {
      std::cerr << "Input file error: " << file << std::endl;
      exit(1);
    }
    is_ = new std::istream(&fb_);
  }

  void outputFileNames() {
    for (auto& it : filenames_) {
      std::cout << it << std::endl;
    }
  }

  FILE* openOutputFile(const char* file) {
    filenames_.push_back(file);
    return fopen(file, "w");
  }
  
  uint64_t* emptyArray(int size) {
    uint64_t* t = new uint64_t[size];
    memset(t, 0, size * sizeof(uint64_t));
    return t;
  }

  void reportError(const char* msg) {
    std::cerr << msg << std::endl;
    exit(1);
  }
};
