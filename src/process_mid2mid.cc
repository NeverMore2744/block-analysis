#include "processor.h"
#include "large_array.h"

class Processor : Processor_base {
  std::set<uint64_t> mins;
  std::map<std::string, int> minTimestamps;
  std::map<std::string, int> maxTimestamps;

  std::map<int, std::set<std::string>> min2active_volumes;
  std::map<int, std::set<std::string>> read_active;
  std::map<int, std::set<std::string>> write_active;

  std::set<std::string> read_active_sets;
  std::set<std::string> write_active_sets;

  void readDataGroup(FILE* fin, std::vector<uint64_t>& data) {
    char s[200];
    uint64_t time_in_min, value, rows;
    int ret;
    char* retChar;

    fgets(s, 200, fin);
    ret = sscanf(s, "%lu", &rows);
    assert(ret == 1);

    data.clear();

    for (int i = 0; i < (int)rows; i++) {
      retChar = fgets(s, 200, fin);
      if (retChar == NULL) break;
      ret = sscanf(s, "%lu%lu", &time_in_min, &value);
      assert(ret == 2);

      // Zero padding
      while ((int)data.size() < (int)time_in_min) {
        data.push_back(0);
      }
      data.push_back(value);

      if (value > 0) {
        mins.insert(i);
      }
    }
  }

  void outputPeakAvgStddev(std::vector<uint64_t>& data, FILE* file_out, const char* type) {
    uint64_t peak = 0;
    double avg = 0.0, stddev = 0.0;

    for (int i = 0; i < (int)data.size(); i++) {
      avg += (double)data[i];
      if (data[i] > peak) peak = data[i];  
    }

    if (strlen(type) == 1 && type[0] == 'R') {
      std::cerr << "Volume " << volume_id_ << " peakR = " << peak <<std::endl;
    }

    int endTime = maxTimestamps[volume_id_], startTime = minTimestamps[volume_id_];
    avg /= (double)(endTime - startTime + 1);

    for (int i = startTime; i <= endTime; i++) {
      stddev += pow((double)data[i] - avg, 2);
    }
    stddev = sqrt(stddev / (endTime - startTime + 1));

    fprintf(file_out, "%s %lu %.6lf %.6lf %s\n", volume_id_.c_str(), peak, avg, stddev, type);
  }

  void addVector(std::vector<uint64_t>& data, std::vector<uint64_t>& data1, std::vector<uint64_t>& data2) {
    data.clear();
    for (int i = 0; i < (int)std::min(data1.size(), data2.size()); i++) {
      data.push_back(data1[i] + data2[i]);
    }

    if (data1.size() > data2.size()) {
      for (int i = data2.size(); i < (int)data1.size(); i++) {
        data.push_back(data1[i]);
      }
    } else {
      for (int i = data1.size(); i < (int)data2.size(); i++) {
        data.push_back(data2[i]);
      }
    }
  }

  void aggregateByMap(std::vector<uint64_t>& input, std::vector<uint64_t>& output, 
      std::map<int,int>& mp, int target_size = 0) {
    output.clear();
    for (int i = 0; i < (int)input.size(); i++) {
      int o = mp[i];
      while ((int)output.size() <= o) {
        output.push_back(0);
      }
      output[o] += input[i];
    }

    while (target_size > (int)output.size()) {
      output.push_back(0);
    }
  } 

  void outputByMap(FILE* file_out, const char* type, std::vector<uint64_t>& data, std::map<int,int>* mp) {
    for (int i = 0; i < (int)data.size(); i++) {
      fprintf(file_out, "%s %d %lu %s\n", volume_id_.c_str(), (mp) ? (*mp)[i] : i, data[i], type);
    }
  }

  // Merge the per-10min data (req or traffic) to a variable that 
  // maps time_in_10min to <traffic, volume> pairs
  void merge(std::vector<std::vector<std::pair<uint64_t, std::string>>>& 
      time_in_10min_2_data_and_volume, 
      std::vector<uint64_t> per_10min_data) {
    if (time_in_10min_2_data_and_volume.size() == 0) {
      for (int i = 0; i < (int)per_10min_data.size(); i++) {
        time_in_10min_2_data_and_volume.push_back({});
      }
    }

    // i is the index of time_10_min. It may be the same as the number of 10 mins
    for (int i = 0; i < (int)per_10min_data.size(); i++) {
      time_in_10min_2_data_and_volume[i].push_back(
          std::make_pair(per_10min_data[i], volume_id_));
    }
  }

  void setTimeIn10min2Req(std::vector<std::map<std::string, uint64_t>>& time_in_10min_2_volume_2_reqs,
      std::vector<uint64_t> per_10min_reqs) {
    for (int i = 0; i < (int)per_10min_reqs.size(); i++) {
      if ((int)time_in_10min_2_volume_2_reqs.size() <= i) {
        time_in_10min_2_volume_2_reqs.push_back({});
      }
      time_in_10min_2_volume_2_reqs[i][volume_id_] = per_10min_reqs[i];
    }
  }

  void outputPerDevicePer10minRank(FILE* file_out, const char* type, 
      std::vector<std::map<std::string, uint64_t>>& time_in_10min_2_volume_2_reqs,
      std::vector<std::vector<std::pair<uint64_t, std::string>>>& time_in_10min_2_data_and_volume) {
    for (int i = 0; i < (int)time_in_10min_2_data_and_volume.size(); i++) {
      std::sort(time_in_10min_2_data_and_volume[i].begin(), time_in_10min_2_data_and_volume[i].end());
      uint64_t sum = 0;
      for (int j = 0; j < (int)time_in_10min_2_data_and_volume[i].size(); j++) {
        sum += time_in_10min_2_data_and_volume[i][j].first;
      }

      for (int rank = 1; rank <= std::min(128, (int)time_in_10min_2_data_and_volume[i].size()); rank++) {
        int index = time_in_10min_2_data_and_volume[i].size() - rank;
        std::string volume = time_in_10min_2_data_and_volume[i][index].second;
        uint64_t num_reqs = time_in_10min_2_volume_2_reqs[i][volume];
        assert(time_in_10min_2_volume_2_reqs[i].count(volume));

        fprintf(file_out, "%s %d %lu %lu %.6lf %.6lf %.6lf %s %d\n", 
            volume.c_str(),
            i * 10, 
            num_reqs,
            time_in_10min_2_data_and_volume[i][index].first, 
            (double)time_in_10min_2_data_and_volume[i][index].first / 1024 / 256, 
            (double)time_in_10min_2_data_and_volume[i][index].first * 4 / num_reqs, 
            (double)time_in_10min_2_data_and_volume[i][index].first / sum,
            type,
            rank);
      } 
    }
  }

  void outputPerDeviceByHourRank(FILE* file_out, const char* type,
      std::vector<std::map<std::string, uint64_t>>& hour_2_volume_2_reqs,
      std::vector<std::vector<std::pair<uint64_t, std::string>>>& hour_2_data_and_volume) {
  }

  void outputCerr(std::vector<uint64_t>& vec, const char* msg) {
    std::cerr << msg << std::endl;
    for (int i = 0; i < std::min(10, (int)vec.size()); i++) {
      std::cerr << i << " " << vec[i] << std::endl;
    }
  }

  public:
  void analyze(const char* file_prefix, const char* volume_file) {
    FILE* fin;

    FILE* fout_per_device_per_10min_traffic = 
      openOutputFile("per_device_per_10min_traffic.data");

    char filename[200];

    std::cerr << "1. Output per_device_per_10min_traffic\n";
    fprintf(fout_per_device_per_10min_traffic, "log timeInMin value type\n");

    // total in all devices, fout_total_traffic
    std::map<int, int> per_10min_map, mul10_map;

    std::vector<uint64_t> read_reqs_per_day;
    std::vector<uint64_t> write_reqs_per_day;
    std::vector<uint64_t> reqs_per_day;
    std::vector<uint64_t> read_traffic_per_day;
    std::vector<uint64_t> write_traffic_per_day;
    std::vector<uint64_t> traffic_per_day;

    std::vector<std::map<std::string, uint64_t>> time_in_10min_2_volume_2_read_reqs;
    std::vector<std::map<std::string, uint64_t>> time_in_10min_2_volume_2_write_reqs;
    std::vector<std::map<std::string, uint64_t>> time_in_10min_2_volume_2_total_reqs;

    std::vector<std::vector<std::pair<uint64_t, std::string>>> time_in_10min_2_read_traffic_and_volume;
    std::vector<std::vector<std::pair<uint64_t, std::string>>> time_in_10min_2_write_traffic_and_volume;
    std::vector<std::vector<std::pair<uint64_t, std::string>>> time_in_10min_2_total_traffic_and_volume;

    openVolumeFile(volume_file);

    // Initialize
    for (int i = 0; i < 31 * 24 * 60; i++) {
      per_10min_map[i] = i / 10 % 144;
    }
    for (int i = 0; i < 31 * 24 * 6; i++) {
      mul10_map[i] = i * 10;
    }

    while (std::getline(*is_, volume_id_)) {
      sprintf(filename, "%s/%s.data", file_prefix, volume_id_.c_str());
      fin = fopen(filename, "r");

      if (fin == nullptr) {
        std::cerr << "Analysis of volume " << volume_id_ << " missed" << std::endl;
        continue;
      }

      std::cerr << "Analyzing volume " << volume_id_ << std::endl;

      std::set<uint64_t> activeDays;
      std::vector<uint64_t> per_10min_read, per_10min_write, per_10min_total;

      // 1. requests
      // 1.1 Read request in minutes (per day), and aggregate to 10-minute intervals
      readDataGroup(fin, read_reqs_per_day);
      aggregateByMap(read_reqs_per_day, per_10min_read, per_10min_map, 144);

      readDataGroup(fin, write_reqs_per_day);
      aggregateByMap(write_reqs_per_day, per_10min_write, per_10min_map, 144);

      readDataGroup(fin, reqs_per_day);
      aggregateByMap(reqs_per_day, per_10min_total, per_10min_map, 144);

//      outputCerr(read_reqs, "read reqs");
//      outputCerr(write_reqs, "write reqs");
//      outputCerr(reqs, "reqs");
//      exit(1);

      setTimeIn10min2Req(time_in_10min_2_volume_2_read_reqs, per_10min_read);
      setTimeIn10min2Req(time_in_10min_2_volume_2_write_reqs, per_10min_write);
      setTimeIn10min2Req(time_in_10min_2_volume_2_total_reqs, per_10min_total);

      // Output: per_device_per_10min_traffic
      outputByMap(fout_per_device_per_10min_traffic, "RR", per_10min_read, &mul10_map);
      outputByMap(fout_per_device_per_10min_traffic, "WR", per_10min_write, &mul10_map);
      outputByMap(fout_per_device_per_10min_traffic, "R", per_10min_total, &mul10_map);

      readDataGroup(fin, read_traffic_per_day);
      aggregateByMap(read_traffic_per_day, per_10min_read, per_10min_map, 144);

      readDataGroup(fin, write_traffic_per_day);
      aggregateByMap(write_traffic_per_day, per_10min_write, per_10min_map, 144);

      readDataGroup(fin, traffic_per_day);
      aggregateByMap(traffic_per_day, per_10min_total, per_10min_map, 144);

      merge(time_in_10min_2_read_traffic_and_volume, per_10min_read);
      merge(time_in_10min_2_write_traffic_and_volume, per_10min_write);
      merge(time_in_10min_2_total_traffic_and_volume, per_10min_total);

      // Output: per_device_per_10min_traffic
      outputByMap(fout_per_device_per_10min_traffic, "RT", per_10min_read, &mul10_map);
      outputByMap(fout_per_device_per_10min_traffic, "WT", per_10min_write, &mul10_map);
      outputByMap(fout_per_device_per_10min_traffic, "T", per_10min_total, &mul10_map);

      fclose(fin);
    }

    fclose(fout_per_device_per_10min_traffic);

    std::cerr << "2. Output per_device_per_10min_rank\n";
    FILE* fout_per_device_per_10min_rank = openOutputFile(
        "per_device_per_10min_rank.data"
    );
    fprintf(fout_per_device_per_10min_rank, 
        "log timeInMin reqs tfcInBlks tfcInGiB reqszInKiB pct type rank\n");
    outputPerDevicePer10minRank(
        fout_per_device_per_10min_rank, "RT", 
        time_in_10min_2_volume_2_read_reqs,
        time_in_10min_2_read_traffic_and_volume);
    outputPerDevicePer10minRank(
        fout_per_device_per_10min_rank, "WT", 
        time_in_10min_2_volume_2_write_reqs,
        time_in_10min_2_write_traffic_and_volume);
    outputPerDevicePer10minRank(
        fout_per_device_per_10min_rank, "T", 
        time_in_10min_2_volume_2_total_reqs,
        time_in_10min_2_total_traffic_and_volume);
    fclose(fout_per_device_per_10min_rank);

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
