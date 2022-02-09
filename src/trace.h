#include<fstream>
#include<iostream>
#include<cassert>
#include<cinttypes>
#include<chrono>
#include<list>
#include<sys/time.h>
#include<utility>
#include<unordered_map>
#include<string>
#include<cstdint>
#include<map>
#include<cstdio>
#include<vector>
#include<set>
#include<algorithm>
#include<unistd.h>
#include<cstdlib>

typedef std::chrono::high_resolution_clock Clock;
typedef std::chrono::milliseconds milliseconds;

class Trace {
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
  uint64_t totReadBytes_ = 0;

  public:
  uint64_t cnt = 0;
#if defined(ALICLOUD) // || defined(TENCENTCLOUD)
  uint64_t timeOffsetInSeconds_ = 0;  // use Unix time (but in microseconds), starts from 0 AM GMT+8
#elif defined(TENCENTCLOUD) || defined(TENCENTCLOUD_ORIGINAL) 
  uint64_t timeOffsetInSeconds_ = 23*60*60 + 59*60 + 59; // use Unix time, starts from 15:59:59 GMT+0, or 11:59:59 PM GMT+8
#elif defined(MSRC)
  uint64_t timeOffsetInSeconds_ = 17*60*60; // use FILETIME, starts from 5 PM GMT+0
#else
  uint64_t timeOffsetInSeconds_ = 0;
#endif

  void loadProperty(char *propertyFile, const char* selectedVolume) {
    FILE *f = nullptr; 
    uint64_t uniqueLba, maxLba;
    uint64_t n_read_requests, n_read_blocks;
    uint64_t n_write_requests, n_write_blocks, n_update_blocks;
    uint64_t rwss, wwss, uwss;

    std::string volumeId;
    volume_id_ = std::string(selectedVolume);

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

  void myTimer(bool start, const char* event = "") {
    static Clock::time_point ts;
    if (start) {
      ts = Clock::now();
      cnt = 0;
    } else {
      if (++cnt % 1000000 == 0) {
        Clock::time_point te = Clock::now();
        double duration2 = (std::chrono::duration_cast <std::chrono::milliseconds> 
            (te - ts)).count() / 1024.0;
        int n_requests = 0; 
        if (volume_n_read_requests.count(volume_id_) && 
            volume_n_write_requests.count(volume_id_)) {
          n_requests = 
            volume_n_read_requests[volume_id_] + volume_n_write_requests[volume_id_];
        }

        fprintf(stderr, "Volume %s analysis on %s: %lu M reqs (%.2lf %%), "
            "%.2lf sec, read %.2lf GiB, speed %.2lf MiB/s\n", 
            volume_id_.c_str(), event, cnt / 1000000, 
            (n_requests > 0) ? 100.0 * cnt / n_requests : 0, 
            duration2, (double)totReadBytes_ / 1024.0 / 1024.0 / 1024.0, 
            (double)totReadBytes_ / 1024.0 / 1024.0 / duration2);
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

  // timestamp: in 10e-7 second
  // offset and length: in 4KiB ("sector" enabled: 512B)
  int readNextRequestFstream(std::istream& is, uint64_t &timestamp, bool &isWrite, uint64_t &offset, uint64_t &length, char* line_cstr, bool sector = false) {
    while (std::getline(is, line_)) {
      strcpy(line_cstr, line_.c_str());
      totReadBytes_ += line_.length() + 1;

#if defined(TENCENTCLOUD) || defined(TENCENTCLOUD_ORIGINAL) 
//    uint64_t beginTimestampInSec = 1538326799llu; // Minimum timestamp in Tencent, 16:59:59 UTC 30/9/2018
      uint64_t beginTimestampInSec = 1538323199llu; // Minimum timestamp in Tencent, 15:59:59 UTC 30/9/2018

      int pos = strlen(line_cstr) - 1;
      for ( ; pos >= 0 && line_cstr[pos] != ','; pos--)
        ; 
      pos++; 
      int pos2 = pos-2;
      for ( ; pos2 >= 0 && line_cstr[pos2] != ','; pos2--)
        ; 
      pos2++;
      int pos3 = pos2-2;
      for ( ; pos3 >= 0 && line_cstr[pos3] != ','; pos3--)
        ; 
      pos3++;

      offset = 0, length = 0, timestamp = 0;
      isWrite = line_cstr[pos] != '0';  // 1 == Write
      for (int i = pos2; isdigit(line_cstr[i]); i++) {
        length = length*10 + (line_cstr[i] - '0');
      }
      for (int i = pos3; isdigit(line_cstr[i]); i++) {
        offset = offset*10 + (line_cstr[i] - '0');
      }
      for (int i = 0; isdigit(line_cstr[i]); i++) {
        timestamp = timestamp*10 + (line_cstr[i] - '0');
      }

      timestamp = (timestamp - beginTimestampInSec) * 10000000;

      if (!sector) {
        length = (offset + length + 7) / 8 - offset / 8;
        offset /= 8;
      }
      return 1;

#elif defined(ALICLOUD)
      uint64_t beginTimestampInUsec = 1577808000000000llu;
      int pos = strlen(line_cstr) - 1;
      for ( ; pos >= 0 && line_cstr[pos] != ','; pos--)
        ; 
      pos++; 
      int pos2 = pos-2;
      for ( ; pos2 >= 0 && line_cstr[pos2] != ','; pos2--)
        ; 
      pos2++;
      int pos3 = pos2-2;
      for ( ; pos3 >= 0 && line_cstr[pos3] != ','; pos3--)
        ; 
      pos3++;

      offset = 0, length = 0, timestamp = 0;
      isWrite = (line_cstr[pos3-2] == 'w' || line_cstr[pos3-2] == 'W');
      for (int i = pos; isdigit(line_cstr[i]); i++) {
        timestamp = timestamp*10 + (line_cstr[i] - '0');
      }
      for (int i = pos2; isdigit(line_cstr[i]); i++) {
        length = length*10 + (line_cstr[i] - '0');
      }
      for (int i = pos3; isdigit(line_cstr[i]); i++) {
        offset = offset*10 + (line_cstr[i] - '0');
      }

      timestamp = (timestamp - beginTimestampInUsec) * 10; // Change to 100 ns
      if (!sector) {
        length = (offset + length + 4095) / 4096 - offset / 4096;
        offset /= 4096;
      } else {
        length = (offset + length + 511) / 512 - offset / 512;
        offset /= 512;
      }
      return 1;

#elif defined(MSRC)
      uint64_t beginTimestampIn100ns = 128166372000000000llu; // Minimum timestamp in MSRC 
      char volume[100], typeStr[4];
      uint64_t volumeNum, latency;

      sscanf(line_cstr, "%lu,%[^,],%lu,%[^,],%lu,%lu,%lu", &timestamp, volume,
          &volumeNum, typeStr, &offset, &length, &latency);
      timestamp -= beginTimestampIn100ns;

      // Discard the current request because it is after 7 days
      if (timestamp / 10000000 / 60 >= 7 * 24 * 60) return 0;  

      isWrite = typeStr[0] == 'W';
      if (!sector) {
        length = (offset + length + 4095) / 4096 - offset / 4096;
        offset /= 4096;
      } else {
        length = (offset + length + 511) / 512 - offset / 512;
        offset /= 512;
      }
      return 1;

#elif defined(SSDTRACE)
      uint64_t beginTimestampInNsec = 0;
      uint64_t volumeId;
      int pos = strlen(line_cstr) - 3;  // Jump over "success"
      for ( ; pos >= 0 && line_cstr[pos] != ' '; pos--); pos++; 
      pos = pos - 2;  // Jump over "elapsed"
      for ( ; pos >= 0 && line_cstr[pos] != ' '; pos--); pos++; // 4th: timestamp
      int pos2 = pos-2;
      for ( ; pos2 >= 0 && line_cstr[pos2] != ' '; pos2--); pos2++; // 3th: length
      int pos3 = pos2-2;
      for ( ; pos3 >= 0 && line_cstr[pos3] != ' '; pos3--); pos3++; // 2th: offset 

      offset = 0, length = 0, timestamp = 0;
      isWrite = line_cstr[0] == 'W';
      type = line_cstr[0];
      for (int i = pos; isdigit(line_cstr[i]); i++) timestamp = timestamp*10 + (line_cstr[i] - '0');
      for (int i = pos2; isdigit(line_cstr[i]); i++) length = length*10 + (line_cstr[i] - '0');
      for (int i = pos3; isdigit(line_cstr[i]); i++) offset = offset*10 + (line_cstr[i] - '0'); // unit: 512B

      if (!sector) {
        length = (offset + length + 7) / 8 - offset / 8;  // Change to unit: 4 kib blocks
        offset /= 8;
      } 
      return 1;

#endif
      return 0;
    }

    // Not file content any more. Exit
    return 0;
  }
};

class Analyzer_base {
  public:
  Trace trace_;

  std::string line_, volume_id_;
  std::filebuf fb_;
  std::istream* is_;
  uint64_t n_blocks_ = -1ull;
  char line2_[200];
  
  void init(char *propertyFileName, char *volume) {
    trace_.loadProperty(propertyFileName, volume);
    volume_id_ = std::string(volume);
  }

  void openTrace(const char *inputTrace) {
    if (!fb_.open(inputTrace, std::ios::in)) {
      std::cerr << "Input file error: " << inputTrace << std::endl;
      exit(1);
    }
    is_ = new std::istream(&fb_);
  }
};
