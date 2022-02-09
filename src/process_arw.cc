#include <stdio.h>
#include <fstream>
#include <iostream>
#include <string.h>
#include <inttypes.h>
#include <assert.h>
#include <vector>
#include <map>
#include "large_array.h"
#include "processor.h"

uint64_t d[10] = {1, 10, 100, 1000, 10000, 100000, 1000000};
uint64_t cp[10] = {100, 1000, 10000, 100000, 1000000, 10000000, 999999999};
uint64_t deltaInTime[10] = {1, 60, 600, 6000, 60000, 600000, 6000000, 60000000};
uint64_t chkpInTime[10] = {600, 6000, 60000, 600000, 6000000, 60000000, 600000000, 999999999};

class Processor : Processor_base {
  FILE* fin, *fout, *fout2, *fout3, *fout4, *fout5; // time, time_pcts, global_pct, global_cnt, per_volume_cnt
  LargeArray<uint64_t> *globalRwarwCounts_[4];
  LargeArray<uint64_t> *singleTimeIn100ms_;
  LargeArray<uint64_t> *singleRwarwCounts_;
  uint64_t singleTotalCounts_;
  uint64_t globalRwarwTotalCounts_[4];
  uint64_t singleRwarwTotalCounts_[4];

  public:
    std::vector<uint64_t> get_pcts(uint64_t sum_cnts) {
      std::vector<uint64_t> rets;
      uint64_t tmp_cnt = 0;
      uint64_t ind = 1;
      assert(singleTimeIn100ms_->getSize() == singleRwarwCounts_->getSize());
      for (int i = 0; i < (int)singleRwarwCounts_->getSize(); i++) {
        if (ind > 100) break;
        uint64_t t = singleRwarwCounts_->get(i);
        while (ind <= 100 && tmp_cnt + t >= (double)sum_cnts * ind / 100) {
          rets.push_back(singleTimeIn100ms_->get(i));
          ind++;
        }
        tmp_cnt = tmp_cnt + t;
      }
      return rets;
    }

    void takeSecond(uint64_t rows, int prefices_i) {
      char* retChar;
      char s[200];
      uint64_t entrySecond, entryCounts, compressedCounts;
      int ret;
      uint64_t cumu = 0, zeros = 0, index = 0, lastDis = 0;
      uint64_t lastZeroIndex = 0;

      std::vector<uint64_t> timeVector;
      std::vector<uint64_t> cntVector;
      singleTotalCounts_ = 0;

      uint64_t lastTimeSecond = -1;

      for (uint64_t outi = 0; outi < rows; outi++) {
        retChar = fgets(s, 200, fin);
        if (retChar == NULL) break;
        ret = sscanf(s, "%lu %lu", &entrySecond, &entryCounts);
        assert(ret == 2);
        globalRwarwCounts_[prefices_i]->incValue(entrySecond, entryCounts);
        singleTimeIn100ms_->put(outi, entrySecond);
        singleRwarwCounts_->put(outi, entryCounts);
        singleTotalCounts_ += entryCounts;
        globalRwarwTotalCounts_[prefices_i] += entryCounts;
        singleRwarwTotalCounts_[prefices_i] += entryCounts;

        for (uint64_t i = lastTimeSecond + 1; i <= entrySecond; i++) {
          compressedCounts = 0;
          if (i == entrySecond) compressedCounts = entryCounts;
          cumu += compressedCounts;

          // fout output: Summarize and compress to one file 
          if (outi == rows - 1 && i == entrySecond) {
            fprintf(fout, "%s %.1f %lu\n", volume_id_.c_str(), (double)i / 10, cumu);
          } else if (i == chkpInTime[index] || (i - lastDis) % deltaInTime[index] == 0) {
            if (cumu == 0) {
              if (!zeros) {
                fprintf(fout, "%s %.1f 0\n", volume_id_.c_str(), (double)i / 10);
                zeros = 1;
                lastZeroIndex = i;
              }
            } else {
              if (zeros && lastZeroIndex < i - deltaInTime[index]) {
                fprintf(fout, "%s %.1f 0\n", volume_id_.c_str(), (double)(i - deltaInTime[index]) / 10);
              }
              fprintf(fout, "%s %.1f %lu\n", volume_id_.c_str(), (double)i/10, cumu);
              zeros = 0;
            }
            lastDis = i; cumu = 0;

            if (i == chkpInTime[index]) index++;
          }
        }

        lastTimeSecond = entrySecond;
      }
    }

    void single_summary() {
      // fout2 output: time_pcts
      std::vector<uint64_t> pcts = get_pcts(singleTotalCounts_);
      for (int i = 0; i < (int)pcts.size(); i++) {
        fprintf(fout2, "%s %.1f %d\n", volume_id_.c_str(), (double)pcts[i] / 10, i);
      }
    }

    void summary() {
      // fout3 output: global_pcts
      for (uint64_t prefices_i = 0; prefices_i < 4; prefices_i++) {
        fout3 = fopen(filenames_[prefices_i * 5 + 2].c_str(), "a");

        uint64_t tmp_cnt = 0, indInThousand = 1;
        uint64_t sum_cnts = globalRwarwTotalCounts_[prefices_i];

        for (uint64_t i = 0; i < globalRwarwCounts_[prefices_i]->getSize(); i++) {
          if (indInThousand > 1000) break;
          uint64_t t = globalRwarwCounts_[prefices_i]->get(i);
          while (indInThousand <= 1000 && tmp_cnt + t >= (double)sum_cnts * indInThousand / 1000) {
            fprintf(fout3, "%.1f %.1f\n", (double)i / 10, (double)indInThousand / 10);
            indInThousand++;
          }
          tmp_cnt = tmp_cnt + t;
        }

        fclose(fout3);
      }

      // fout4 output: global_cnts
      for (uint64_t prefices_i = 0; prefices_i < 4; prefices_i++) {
        fout4 = fopen(filenames_[prefices_i * 5 + 3].c_str(), "a");

        for (uint64_t i = 0; i < globalRwarwCounts_[prefices_i]->getSize(); i++) {
          uint64_t t = globalRwarwCounts_[prefices_i]->get(i);
          if (t == 0) continue;
          fprintf(fout4, "%.1f %lu\n", (double)i / 10, globalRwarwCounts_[prefices_i]->get(i));
        }

        fclose(fout4);
      }
    }

    void analyze(const char* file_prefix, const char* volume_file) {
      char filename[200], s[200];
      char prefices[4][10] = {"rar", "war", "raw", "waw"};
      char fn_time[200], fn_time_pcts[200], fn_global_pct[200];
      char fn_global_cnt[200];
      char fn_single_cnt[200];
      std::string volume;

      memset(globalRwarwTotalCounts_, 0, sizeof(globalRwarwTotalCounts_));
      openVolumeFile(volume_file); 

      while (std::getline(*is_, volume)) {
        sprintf(filename, "%s/%s.data", file_prefix, volume.c_str());

        fin = fopen(filename, "r");
        if (fin == nullptr) {
          std::cerr << "Analysis of volume " << volume << " missed" << std::endl;
          continue;
        }

        std::cerr << "Processing " << volume << std::endl;
        volume_id_ = volume;
        memset(singleRwarwTotalCounts_, 0, sizeof(singleRwarwTotalCounts_));

        for (int prefices_i = 0; prefices_i < 4; prefices_i++) {
          std::cerr << prefices[prefices_i] << std::endl;

          if (filenames_.size() < 20) {
            sprintf(fn_time, "%s_time.data", prefices[prefices_i]);
            sprintf(fn_time_pcts, "%s_time_pcts.data", prefices[prefices_i]);
            sprintf(fn_global_pct, "%s_global_pct.data", prefices[prefices_i]);
            sprintf(fn_global_cnt, "%s_global_cnt.data", prefices[prefices_i]);
            sprintf(fn_single_cnt, "%s_single_total_cnt.data", prefices[prefices_i]);

            filenames_.push_back(fn_time);
            filenames_.push_back(fn_time_pcts);
            filenames_.push_back(fn_global_pct);
            filenames_.push_back(fn_global_cnt);
            filenames_.push_back(fn_single_cnt);

            fout = fopen(filenames_[prefices_i * 5].c_str(), "w");
            fout2 = fopen(filenames_[prefices_i * 5 + 1].c_str(), "w");
            fout3 = fopen(filenames_[prefices_i * 5 + 2].c_str(), "w");
            fout4 = fopen(filenames_[prefices_i * 5 + 3].c_str(), "w");
            fout5 = fopen(filenames_[prefices_i * 5 + 4].c_str(), "w");

            fprintf(fout, "log timeInSec cnt\n");
            fprintf(fout2, "log timeInSec pct\n");
            fprintf(fout3, "timeInSec pct\n");
            fprintf(fout4, "timeInSec cnt\n");
            fprintf(fout5, "log rar war raw waw\n");

            fclose(fout3);
            fclose(fout4);

            globalRwarwCounts_[prefices_i] = new LargeArray<uint64_t>(31 * 24 * 36000);
          } else {
            fout = fopen(filenames_[prefices_i * 5].c_str(), "a");
            fout2 = fopen(filenames_[prefices_i * 5 + 1].c_str(), "a");
          }

          uint64_t rows;

          // fout1: raw time
          char* retChar = fgets(s, 200, fin);
          if (retChar == nullptr) {
            std::cerr << "Empty file\n";
          }
          sscanf(s, "%lu", &rows);
          singleRwarwCounts_ = new LargeArray<uint64_t>(rows);
          singleTimeIn100ms_ = new LargeArray<uint64_t>(rows);

          takeSecond(rows, prefices_i);
          single_summary();

          fclose(fout);
          fclose(fout2);
        }
        fclose(fin);

        fprintf(fout5, "%s %lu %lu %lu %lu\n", volume_id_.c_str(), 
            singleRwarwTotalCounts_[0], singleRwarwTotalCounts_[1], 
            singleRwarwTotalCounts_[2], singleRwarwTotalCounts_[3]);
        fprintf(stderr, "%s %lu %lu %lu %lu\n", volume_id_.c_str(), 
            singleRwarwTotalCounts_[0], singleRwarwTotalCounts_[1], 
            singleRwarwTotalCounts_[2], singleRwarwTotalCounts_[3]);
      }
      summary();

      outputFileNames();
    }
};

int main(int argc, char** argv) {
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);

  if (argc < 3) {
    std::cerr << "Error - params not enough. " << argv[0] << " <file_prefix> <volume_file>\n";
    exit(1);
  }

  Processor processor;
  processor.analyze(argv[1], argv[2]);

  return 0;
}
