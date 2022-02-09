#include <cstdio>
#include <cstring>
#include <cinttypes>
#include <map>
#include <set>
#include <queue>
#include <utility>

#include "processor.h"

class Processor : Processor_base {
  public:
    void analyze(const char* file_prefix, const char* volume_file) {
      FILE* fin;
      FILE* fout = openOutputFile("request_size.data");
      FILE* fout2 = openOutputFile("overall_request_size.data");

      std::map<uint64_t, std::pair<uint64_t, uint64_t>> rs2nums;

      fprintf(fout, "log size read_num write_num\n"); 
      fprintf(fout2, "size read_num write_num\n");

      openVolumeFile(volume_file);
      char s[200], filename[200];

      while (std::getline(*is_, volume_id_)) {
        sprintf(filename, "%s/%s.data", file_prefix, volume_id_.c_str());
        fin = fopen(filename, "r");

        if (fin == nullptr) {
          std::cerr << "Analysis of volume " << volume_id_ << " missed" << std::endl;
          continue;
        }

        uint64_t sz, read_num, write_num;
        char tmp[200];
        int ret;
        char* retChar;

        retChar = fgets(s, 200, fin);
        if (retChar == NULL) break;

        bool flag = false;

        while (1) {
          retChar = fgets(s, 200, fin);
          if (retChar == NULL) break;
          ret = sscanf(s, "%s %ld %ld %ld", tmp, &sz, &read_num, &write_num);
          if (sz % 512 && !flag) {
            fprintf(stderr, "volume %s sz %ld\n", volume_id_.c_str(), sz);
            flag = true;
          }
          if (ret <= 2) break;

          if (rs2nums.count(sz)) {
            rs2nums[sz].first += read_num;
            rs2nums[sz].second += write_num;
          } else {
            rs2nums[sz] = {read_num, write_num};
          }
          fprintf(fout, "%s %ld %ld %ld\n", volume_id_.c_str(), sz, 
              read_num, write_num);
        }
        fclose(fin);
      }

      for (auto& it : rs2nums) {
        fprintf(fout2, "%ld %ld %ld\n", it.first, it.second.first, it.second.second);
      }
      fclose(fout);
      fclose(fout2);

      outputFileNames();
    }
};

int main(int argc, char** argv) {
  setbuf(stderr, NULL);
  setbuf(stdout, NULL);

  if (argc < 3) {
    std::cerr << "Error - params not enough. " << argv[0] << " <file_prefix> <volume_file>\n";
    exit(1);
  }

  Processor processor;
  processor.analyze(argv[1], argv[2]);

  return 0;
}
