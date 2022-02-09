#include "processor.h"
#include "large_array.h"

class Processor : Processor_base {
  const uint64_t freqThreshold = 123456789;

  public:

  void init(const char* property_file) {
    loadProperty(property_file);
  }

  LargeArray<uint64_t>* resize(LargeArray<uint64_t>* arr, uint64_t size) {
    uint64_t ori_size = arr->getSize(), new_size = ori_size, value;
    LargeArray<uint64_t>* new_arr;
    if (ori_size >= size) return arr;
    while (new_size < size) {
      new_size *= 2;
    }

    if (new_size >= freqThreshold) reportError("freq too large, exit");

    new_arr = new LargeArray<uint64_t>(new_size);
    std::cerr << "Resize " << ori_size << " to " << new_size << std::endl;

    // Copy
    for (uint64_t i = 0; i < ori_size; i++) {
      value = arr->get(i);
      if (value > 0) {
        new_arr->put(i, value);
      }
    }

    delete arr;
    return new_arr;
  }

  void analyze(const char* file_prefix, const char* volume_file) {
    FILE* fin;
    FILE* fout_basic = openOutputFile("attr.data");
    FILE* fout_read_freq = openOutputFile("readFreq.data");
    FILE* fout_write_freq = openOutputFile("writeFreq.data");
    FILE* fout_rw_only = openOutputFile("rw_only.data");

    LargeArray<uint64_t>* freq2cnt, *lba2read;
    LargeArray<uint8_t>* lba2flag;
    std::map<uint64_t, uint64_t> freq2cnt_map;

    fprintf(fout_basic, "log wss maxLBA numRReq trb numWReq twb tub urb uwb uub\n");
    fprintf(fout_read_freq, "log freq cnt\n");
    fprintf(fout_write_freq, "log freq cnt\n");
    fprintf(fout_rw_only, "log read_only write_only rw_interleaved read_on_read_only write_on_write_only read_on_others write_on_others\n"); // Each line is an LBA

    uint64_t n_read_reqs, n_write_reqs;
    uint64_t wss, read_wss, write_wss, update_wss = 0; // in blocks
    uint64_t max_lba; // in blocks
    uint64_t n_read_blks, n_write_blks, n_update_blks;
    uint64_t property_wss, property_max_lba;

    openVolumeFile(volume_file);
    char s[200], filename[200];

    while (std::getline(*is_, volume_id_)) {
      sprintf(filename, "%s/%s.data", file_prefix, volume_id_.c_str());
      fin = fopen(filename, "r");

      if (fin == nullptr) {
        std::cerr << "Analysis of volume " << volume_id_ << " missed" << std::endl;
        continue;
      }

      std::cerr << "Analyzing volume " << volume_id_ << std::endl;

      // Directly output the properties
      fprintf(fout_basic, "%s %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu\n", 
          volume_id_.c_str(), 
          getUniqueLba(volume_id_), getMaxLba(volume_id_), 
          getNumRReq(volume_id_), getTrb(volume_id_),  
          getNumWReq(volume_id_), getTwb(volume_id_), getTub(volume_id_),  
          getRwss(volume_id_), getWwss(volume_id_), getUwss(volume_id_)); 

      property_wss = properties[volume_id_].first;
      property_max_lba = properties[volume_id_].second;

      freq2cnt = new LargeArray<uint64_t>(200000);
      lba2flag = new LargeArray<uint8_t>(property_max_lba + 1);
      lba2read = new LargeArray<uint64_t>(property_max_lba + 1);

      fprintf(stderr, "property_max_lba %lu\n", property_max_lba);

      fscanf(fin, "%lu%lu%lu%lu%lu", &n_read_reqs, &n_read_blks, &n_write_reqs, &n_write_blks, &n_update_blks);
      fscanf(fin, "%lu", &read_wss);

      fgets(s, 200, fin);
      uint64_t addr, times;
      int ret;
      char* retChar;
      max_lba = update_wss = 0;

      // Prepare: rw_only
      uint64_t read_only = 0, write_only = 0, rw_interleaved = 0;
      uint64_t read_on_read_only = 0, write_on_write_only = 0, read_on_others = 0, write_on_others = 0;
      std::cerr << "Reading: read freq\n";

      // Part 2: fout_read_freq: read frequency
      for (uint64_t i = 0; i < read_wss; i++) {
        retChar = fgets(s, 200, fin);
        ret = sscanf(s, "%lu%lu", &addr, &times);

        if (retChar == nullptr || ret != 2) reportError("Error: data file not complete - read wss");

        if (addr > max_lba) max_lba = addr;

        lba2flag->put(addr, 1);
        lba2read->put(addr, times);

        // Get freq2cnt
        if (times >= freqThreshold) {
          freq2cnt_map[times]++;
        } else {
          if (freq2cnt->getSize() < times) {
            freq2cnt = resize(freq2cnt, times);
          }
          freq2cnt->inc(times);
        }
      }

      // Check read freq
      assert(lba2read->getSum() == getTrb(volume_id_));

      std::cerr << "Output: read freq\n";
      for (uint64_t i = 0; i < freq2cnt->getSize(); i++) {
        uint64_t t = freq2cnt->get(i);
        if (t > 0) fprintf(fout_read_freq, "%s %lu %lu\n", volume_id_.c_str(), i, t);
      }
      if (freq2cnt_map.size() > 0) {
        std::cerr << "Volume " << volume_id_ << " have " << freq2cnt_map.size() << " LBAs with large read frequencies\n";
      }
      for (auto& it : freq2cnt_map) {
        fprintf(fout_read_freq, "%s %lu %lu\n", volume_id_.c_str(), it.first, it.second);
      }
      freq2cnt->clear();
      freq2cnt_map.clear();

      // Part 3: fout_write_freq: write frequency
      retChar = fgets(s, 200, fin);
      ret = sscanf(s, "%lu", &write_wss);
      if (retChar == nullptr || ret != 1) {
        reportError("Error: data file not complete - write wss");
      }
      std::cerr << "Reading: write freq\n";

      for (uint64_t i = 0; i < write_wss; i++) {
        retChar = fgets(s, 200, fin);
        if (retChar == NULL) break;
        ret = sscanf(s, "%lu%lu", &addr, &times);

        if (retChar == nullptr || ret != 2) {
          reportError("Error: data file not complete - write wss");
        }

        if (times > 1) update_wss++; 
        if (addr > max_lba) max_lba = addr;
        lba2flag->put(addr, 1);

        // Part 4. consider the read/write only blocks
        uint64_t read_times = lba2read->get(addr);
        uint64_t write_times = times;
        uint32_t sum_num = read_times + write_times;
        
        // Check the read/write-only blocks for those with writes
        if ((double)read_times / sum_num <= 0.05) {
          write_only++, write_on_write_only += write_times, read_on_others += read_times;
        } else if ((double)write_times / sum_num <= 0.05) {
          read_only++, read_on_read_only += read_times, write_on_others += write_times;
        } else {
          rw_interleaved++, read_on_others += read_times, write_on_others += write_times;
        }

        if (read_times > 0) {
          lba2read->put(addr, 0);
        }

        // Get freq2cnt
        if (times >= freqThreshold) {
          freq2cnt_map[times]++;
        } else {
          if (freq2cnt->getSize() < times) {
            freq2cnt = resize(freq2cnt, times);
          }
          freq2cnt->inc(times);
        }
      }

      std::cerr << "Output: write freq\n";
      for (uint64_t i = 0; i < freq2cnt->getSize(); i++) {
        uint64_t t = freq2cnt->get(i);
        if (t > 0) fprintf(fout_write_freq, "%s %lu %lu\n", volume_id_.c_str(), i, t);
      }
      if (freq2cnt_map.size() > 0) {
        std::cerr << "Volume " << volume_id_ << " have " << freq2cnt_map.size() << " LBAs with large write frequencies\n";
      }
      for (auto& it : freq2cnt_map) {
        fprintf(fout_read_freq, "%s %lu %lu\n", volume_id_.c_str(), it.first, it.second);
      }

      // Check the read/write-only blocks for those without writes
      for (uint64_t i = 0; i < lba2read->getSize(); i++) {
        uint64_t t = lba2read->get(i);
        if (t > 0) {
          read_only++, read_on_read_only += t;
        }
      }

      std::cerr << "Output: rwonly\n";
      fprintf(fout_rw_only, "%s %lu %lu %lu %lu %lu %lu %lu\n", volume_id_.c_str(),
          read_only, write_only, rw_interleaved,
          read_on_read_only, write_on_write_only, read_on_others, write_on_others);

      wss = lba2flag->countNonZero();
      assert(wss == property_wss);
      assert(wss <= read_wss + write_wss);
      assert(write_wss >= update_wss);

      assert(wss == getUniqueLba(volume_id_));
      assert(max_lba == getMaxLba(volume_id_));
      assert(n_read_reqs == getNumRReq(volume_id_));
      assert(n_read_blks == getTrb(volume_id_));
      assert(n_write_reqs == getNumWReq(volume_id_));
      assert(n_write_blks == getTwb(volume_id_));
      assert(n_update_blks == getTub(volume_id_));
      assert(read_wss == getRwss(volume_id_));
      assert(write_wss == getWwss(volume_id_));
      assert(update_wss == getUwss(volume_id_));

      fclose(fin);

      delete freq2cnt;
      delete lba2read;
      delete lba2flag;
    }

    fclose(fout_basic);
    fclose(fout_read_freq);
    fclose(fout_write_freq);
    fclose(fout_rw_only);

    outputFileNames();
  }
};


int main(int argc, char** argv) {
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);

  if (argc < 4) {
    std::cerr << "Error - params not enough. " << argv[0] << " <file_prefix> <volume_file> <property_file>\n";
    exit(1);
  }

  Processor processor;
  processor.init(argv[3]);
  processor.analyze(argv[1], argv[2]);

  return 0;
}
