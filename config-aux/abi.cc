/* detect unsupported C++ compilers */
#if !defined(__GNUC__) || \
     defined(__PGI) || \
     defined(__xlC__)
  #error CXX compiler is not GNU g++!!!
#endif

#include <iostream>

int main() {
  if (sizeof(int) == 4 && sizeof(long) == 4 && sizeof(void *) == 4)
    std::cout << "ILP32" << std::endl;
  else if (sizeof(int) == 4 && sizeof(long) == 8 && sizeof(void *) == 8)
    std::cout << "LP64" << std::endl;
  else 
    std::cout << "OTHER" << std::endl;
  return 0;
}
