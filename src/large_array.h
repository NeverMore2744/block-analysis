#include <cstring>
#include <cinttypes>
#include <iostream>

template<class T>
class LargeArray {
  private:
    T **arrays_;
    uint32_t unitSize_ = 128 * 1024 * 1024;
    uint64_t size_;

  public:
    void clear() {
      uint32_t nArrays = (size_ + unitSize_ - 1) / unitSize_;
      uint32_t lastArraySize = (size_ - 1) % unitSize_ + 1;
      
      for (int i = 0; i < (int)nArrays; ++i) {
        if (i == (int)nArrays - 1) {
          memset(arrays_[i], 0, sizeof(T) * lastArraySize);
        } else {
          memset(arrays_[i], 0, sizeof(T) * unitSize_);
        }
      }
    }
    
    LargeArray(uint64_t size) {
      if (size == 0) {
        std::cerr << "Warning: LargeArray size = 0\n";
        size = 1024;
      }
      uint32_t nArrays = (size + unitSize_ - 1) / unitSize_;
      uint32_t lastArraySize = (size - 1) % unitSize_ + 1;
      size_ = size;
      
      arrays_ = (T**)malloc(sizeof(T*) * nArrays);
      for (int i = 0; i < (int)nArrays; ++i) {
        if (i == (int)nArrays - 1) {
          arrays_[i] = (T*)malloc(sizeof(T) * lastArraySize);
        } else {
          arrays_[i] = (T*)malloc(sizeof(T) * unitSize_);
        }
      }
      clear();
    }

    ~LargeArray() {
      uint32_t nArrays = (size_ + unitSize_ - 1) / unitSize_;
      for (int i = 0; i < (int)nArrays; ++i) {
        if (arrays_[i]) {
          free(arrays_[i]);
          arrays_[i] = nullptr;
        }
      }
      free(arrays_);
    }

    uint64_t countNonZero() {
      uint64_t num = 0;
      for (uint64_t i = 0; i < size_; ++i) {
        if (get(i)) ++num;
      }
      return num;
    }

    void put(uint64_t key, T value) {
      uint32_t arrayId = key / unitSize_;
      uint32_t id = key % unitSize_;

      if (key >= size_) {
        std::cerr << "Too large: key = " << key << " >= size = " << getSize() << std::endl;
        exit(1);
      }

      arrays_[arrayId][id] = value;
    }

    T get(uint64_t key) {
      if (key >= getSize()) {
        std::cerr << "Too large: key = " << key << " >= size = " << getSize() << std::endl;
        exit(1);
      }
      uint32_t arrayId = key / unitSize_;
      uint32_t id = key % unitSize_;
      return arrays_[arrayId][id];
    }

    void inc(uint64_t key) {
      if (key >= getSize()) {
        std::cerr << "Too large: key = " << key << " >= size = " << getSize() << std::endl;
        exit(1);
      }
      put(key, get(key) + 1);
    }

    void incValue(uint64_t key, T value) {
      put(key, get(key) + value);
    }

    void output() {
      std::cout << size_ << std::endl;
      for (uint64_t i = 0; i < size_; ++i) {
        std::cout << i << " " << get(i) << std::endl;
      }
    }

    void outputNonZero() {
      uint64_t num = 0;
      for (uint64_t i = 0; i < size_; ++i) {
        if (get(i)) ++num;
      }

      std::cout << num << std::endl;
      for (uint64_t i = 0; i < size_; ++i) {
        if (get(i)) {
          std::cout << i << " " << get(i) << std::endl;
        }
      }
    }

    void outputNonZeroToFile(FILE* fout, bool specify_rows = true) {
      uint64_t num = 0;
      for (uint64_t i = 0; i < size_; ++i) {
        if (get(i)) ++num;
      }

      if (specify_rows) fprintf(fout, "%lu\n", num);
      for (uint64_t i = 0; i < size_; ++i) {
        if (get(i)) {
          fprintf(fout, "%lu %lu\n", i, get(i));
        }
      }
    }

    uint64_t getSize() {
      return size_;
    }

    T getSum() {
      T sumValue = 0;
      for (uint64_t i = 0; i < size_; ++i) {
        sumValue += get(i);
      }
      return sumValue;
    }

};
