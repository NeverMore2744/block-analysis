#include "large_array.h"
#include "trace.h"

class Split : Analyzer_base {

public:

  void analyze(char *inputTrace, char* outputPrefix) {
      char filename[300];
      openTrace(inputTrace);

      std::map<int, std::vector<std::string>> leftStrings;
      uint64_t saved = 0;

      trace_.myTimer(true, "split");

      while (std::getline(*is_, line_)) {
        strcpy(line2_, line_.c_str());
        int pos = strlen(line2_) - 1;
        for ( ; pos >= 0 && line2_[pos] != ','; pos--); 
        std::string s = std::string(line2_, pos);

        int res = 0;
        for (int i = pos+1; line2_[i] != '\0'; i++) if (isdigit(line2_[i])) res = res*10 + (line2_[i] - '0');

        leftStrings[res].push_back(s);
        saved++;
        trace_.myTimer(false, "split");

//        if (cnt % 80000000 == 0) {
//          int ct = 0;
//          for(auto& it : leftStrings) {
//            if (ct % 100 == 0) {
//              std::cout << "writing file " << it.first << ".csv" << std::endl;
//            }
//
//            sprintf(filename, "%s/%d.csv", outputPrefix, it.first);
//            std::ofstream fs;
//            fs.open(filename, std::ofstream::out | std::ofstream::app);
//            for(auto& it0 : it.second) {
//              fs << it0 << "\n";
//            }
//            fs.close();
//
//            ct++;
//          }
//          leftStrings.clear();
//          saved = 0;
//          gettimeofday(&tv2, NULL);
//          std::cout << tv2.tv_sec - tv1.tv_sec << " seconds " << std::endl; 
//        }
      }

      for(auto& it : leftStrings) {
        sprintf(filename, "%s/%d.csv", outputPrefix, it.first);
        std::ofstream fs;
        fs.open(filename, std::ofstream::out | std::ofstream::app);
        for(auto& it0 : it.second) {
          fs << it0 << "\n";
        }
        fs.close();
      }
  }
};

int main(int argc, char *argv[]) {
  Split split;
  split.analyze(argv[1], argv[2]);
}
