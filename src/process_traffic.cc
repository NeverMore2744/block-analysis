#include "processor.h"
#include "large_array.h"

int d[10] = {1, 10, 100, 1000, 10000, 100000, 1000000};
int cp[10] = {100, 1000, 10000, 100000, 1000000, 10000000, 999999999};
int dt[10] = {1, 60, 600, 6000, 60000, 600000, 6000000, 60000000};
int cpt[10] = {600, 6000, 60000, 600000, 6000000, 60000000, 600000000, 999999999};

class Processor : Processor_base {
  std::set<uint64_t> mins;
  std::map<std::string, int> minTimeInMin;
  std::map<std::string, int> maxTimeInMin;

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

    // The average considers the time between the first and the last request (Finding B.1)
    int endTime = maxTimeInMin[volume_id_], startTime = minTimeInMin[volume_id_];
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

  void aggregateByMap(std::vector<uint64_t>& input, std::vector<uint64_t>& output, std::map<int,int>& mp, int target_size = 0) {
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

  void outputCerr(std::vector<uint64_t>& vec, const char* msg) {
    std::cerr << msg << std::endl;
    for (int i = 0; i < std::min(10, (int)vec.size()); i++) {
      std::cerr << i << " " << vec[i] << std::endl;
    }
  }

  public:
  void analyze(const char* file_prefix, const char* volume_file) {
    FILE* fin;

    FILE* fout_per_device_active_days = openOutputFile("per_device_active_days.data"); 
    FILE* fout_per_device_traffic = openOutputFile("per_device_traffic.data");
    FILE* fout_per_device_active_time = openOutputFile("per_device_active_time.data");
    FILE* fout_per_device_hour_traffic = openOutputFile("per_device_hour_traffic.data");

    char filename[200];

    //      fprintf(fout_per_device_traffic, "log peakRR peakWR peakRT peakWT peakUT avgRR avgWR avgRT avgWT avgUT stddevRR stddevWR stddevRT stddevWT stddevUT peakR peakT avgR avgT stddevR stddevT minTimeInMin maxTimeInMin activeTimeInMin readActiveTimeInMin writeActiveTimeInMin\n");

    fprintf(fout_per_device_active_days, "log activeDays\n");
    fprintf(fout_per_device_traffic, "log peak avg stddev type\n");
    fprintf(fout_per_device_active_time, "log minTimeInMin maxTimeInMin activeTimeInMin readActiveTimeInMin writeActiveTimeInMin\n");
    fprintf(fout_per_device_hour_traffic, "log timeInHour value type\n");

    // total in all devices, fout_total_traffic
    uint64_t* min2read_reqs_total = emptyArray(31 * 24 * 60 + 10);
    uint64_t* min2write_reqs_total = emptyArray(31 * 24 * 60 + 10);
    uint64_t* min2read_traffic_total = emptyArray(31 * 24 * 60 + 10);
    uint64_t* min2write_traffic_total = emptyArray(31 * 24 * 60 + 10);
    uint64_t* min2update_traffic_total = emptyArray(31 * 24 * 60 + 10);

    uint64_t* hour2read_reqs_total = emptyArray(31 * 24 + 10);
    uint64_t* hour2write_reqs_total = emptyArray(31 * 24 + 10);
    uint64_t* hour2read_traffic_total = emptyArray(31 * 24 + 10);
    uint64_t* hour2write_traffic_total = emptyArray(31 * 24 + 10);
    uint64_t* hour2update_traffic_total = emptyArray(31 * 24 + 10);

    uint64_t maxMin = 0;

    std::map<int, int> hour_map, mul10_map;

    std::vector<uint64_t> read_reqs;
    std::vector<uint64_t> write_reqs;
    std::vector<uint64_t> reqs;
    std::vector<uint64_t> read_traffic;
    std::vector<uint64_t> write_traffic;
    std::vector<uint64_t> update_traffic;
    std::vector<uint64_t> traffic;

    openVolumeFile(volume_file);

    // Initialize
    for (int i = 0; i < 31 * 24 * 60; i++) {
      hour_map[i] = i / 60;
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
      std::vector<uint64_t> hour_read, hour_write, hour_update, hour_total;

      // 1. requests
      readDataGroup(fin, read_reqs);
      aggregateByMap(read_reqs, hour_read, hour_map, 31 * 24);

      readDataGroup(fin, write_reqs);
      aggregateByMap(write_reqs, hour_write, hour_map, 31 * 24);

      addVector(reqs, read_reqs, write_reqs);

//      outputCerr(read_reqs, "read reqs");
//      outputCerr(write_reqs, "write reqs");
//      outputCerr(reqs, "reqs");
//      exit(1);

      outputByMap(fout_per_device_hour_traffic, "RR", hour_read, nullptr);
      outputByMap(fout_per_device_hour_traffic, "WR", hour_write, nullptr);

      minTimeInMin[volume_id_] = 0;
      maxTimeInMin[volume_id_] = 0;
      int first = 1;
      for (int r = 0; r < (int)reqs.size(); r++) {
        if (reqs[r] > 0) {
          if (maxMin < (uint64_t)r) maxMin = r;

          if (first) minTimeInMin[volume_id_] = r, first = 0; 
          maxTimeInMin[volume_id_] = r;

          min2active_volumes[r/10*10].insert(volume_id_);
          if (r < (int)read_reqs.size() && read_reqs[r] > 0) {
            read_active[r/10*10].insert(volume_id_);
            read_active_sets.insert(volume_id_);
            min2read_reqs_total[r] += read_reqs[r];
            hour2read_reqs_total[r / 60] += read_reqs[r];
          }
          if (r < (int)write_reqs.size() && write_reqs[r] > 0) {
            write_active[r/10*10].insert(volume_id_);
            write_active_sets.insert(volume_id_);
            min2write_reqs_total[r] += write_reqs[r];
            hour2write_reqs_total[r / 60] += write_reqs[r];
          }

          activeDays.insert(r / 1440);
        }
      }

      int activeTime = 0, readActiveTime = 0, writeActiveTime = 0;
      bool f1 = false, f2 = false, f3 = false;
      for (int r = 0; r < (int)reqs.size(); r++) {
        if (reqs[r] > 0) {
          if (!f1) activeTime += 10, f1 = true;
          if (!f2 && read_reqs[r] > 0) readActiveTime += 10, f2 = true;
          if (!f3 && write_reqs[r] > 0) writeActiveTime += 10, f3 = true;
        }
        if (r % 10 == 9) f1 = f2 = f3 = false;
      }

      readDataGroup(fin, read_traffic);
      aggregateByMap(read_traffic, hour_read, hour_map, 31*24);
      outputByMap(fout_per_device_hour_traffic, "RT", hour_read, nullptr);

      readDataGroup(fin, write_traffic);
      aggregateByMap(write_traffic, hour_write, hour_map, 31*24);
      outputByMap(fout_per_device_hour_traffic, "WT", hour_write, nullptr);

      readDataGroup(fin, update_traffic);
      aggregateByMap(update_traffic, hour_update, hour_map, 31*24);
      outputByMap(fout_per_device_hour_traffic, "UT", hour_update, nullptr);

      addVector(traffic, read_traffic, write_traffic);

      // Output: per_device_traffic
      // All of them are in minutes.
      { 
        outputPeakAvgStddev(read_reqs, fout_per_device_traffic, "RR");
        outputPeakAvgStddev(write_reqs, fout_per_device_traffic, "WR");
        outputPeakAvgStddev(reqs, fout_per_device_traffic, "R");
        outputPeakAvgStddev(read_traffic, fout_per_device_traffic, "RT");
        outputPeakAvgStddev(write_traffic, fout_per_device_traffic, "WT");
        outputPeakAvgStddev(update_traffic, fout_per_device_traffic, "UT");
        outputPeakAvgStddev(traffic, fout_per_device_traffic, "T");
      }

      for (int r = 0; r < (int)reqs.size(); r++) {
        if (maxMin < (uint64_t)r) maxMin = r;

        if (r < (int)read_traffic.size()) {
          min2read_traffic_total[r] += read_traffic[r];
          hour2read_traffic_total[r / 60] += read_traffic[r];
        }

        if (r < (int)write_traffic.size()) {
          min2write_traffic_total[r] += write_traffic[r];
          hour2write_traffic_total[r / 60] += write_traffic[r];
        }

        if (r < (int)update_traffic.size()) {
          min2update_traffic_total[r] += update_traffic[r];
          hour2update_traffic_total[r / 60] += update_traffic[r];
        }
      }

      // Output: per_device_active_time
      fprintf(fout_per_device_active_time, "%s %d %d %d %d %d\n", 
          volume_id_.c_str(), 
          minTimeInMin[volume_id_], maxTimeInMin[volume_id_], 
          activeTime, readActiveTime, writeActiveTime);

      // Output: per_device_active_days
      fprintf(fout_per_device_active_days, "%s %d\n", 
          volume_id_.c_str(), (int)activeDays.size());

      fclose(fin);
    }

    fclose(fout_per_device_traffic);
    fclose(fout_per_device_active_time);
    fclose(fout_per_device_active_days);
    fclose(fout_per_device_hour_traffic);

    std::cerr << "1. Output total_traffic\n";
    FILE* fout_total_traffic = openOutputFile("total_traffic.data"); 
    fprintf(fout_total_traffic, "timeInMin RR WR RT WT UT\n"); 
    for (int i = 0; i <= (int)maxMin; i++) {
      fprintf(fout_total_traffic, "%d %lu %lu %lu %lu %lu\n", i, 
          min2read_reqs_total[i],
          min2write_reqs_total[i],
          min2read_traffic_total[i],
          min2write_traffic_total[i],
          min2update_traffic_total[i]);
    }
    fclose(fout_total_traffic);

    std::cerr << "2. Output total_traffic by hour\n";
    FILE* fout_total_traffic_by_hour = openOutputFile("total_traffic_by_hour.data"); 
    fprintf(fout_total_traffic_by_hour, "timeInHour RR WR RT WT UT\n"); 
    for (int i = 0; i <= (int)maxMin / 60; i++) {
      fprintf(fout_total_traffic_by_hour, "%d %lu %lu %lu %lu %lu\n", i, 
          hour2read_reqs_total[i],
          hour2write_reqs_total[i],
          hour2read_traffic_total[i],
          hour2write_traffic_total[i],
          hour2update_traffic_total[i]);
    }
    fclose(fout_total_traffic_by_hour);

    std::cerr << "3. Output total_active_devices\n";
    FILE* fout_total_active_devices = openOutputFile("total_active_devices.data"); 
    fprintf(fout_total_active_devices, "timeInMin activeDevices readActive writeActive\n");
    for (auto& it : min2active_volumes) {
      fprintf(fout_total_active_devices, "%d %d %d %d\n", it.first, (int)(it.second.size()), 
          (int)(read_active[it.first].size()), (int)(write_active[it.first].size()));
    }
    fclose(fout_total_active_devices);

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
