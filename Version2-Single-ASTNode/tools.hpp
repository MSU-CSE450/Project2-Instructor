#pragma once

#include <iostream>
// #include "/opt/local/include/gcc13/c++/iostream"

// Helper function that take a line number and any number of additional args
// that it uses to write an error message and terminate the program.
template <typename... Ts>
void Error(size_t line_num, Ts... message) {
  std::cout << "ERROR (line " << line_num <<  "): ";
  (std::cout << ... << std::forward<Ts>(message)) << std::endl;
  exit(1);
}

// Replace all instances in a string of one substring with another.
void ReplaceAll(std::string & str, std::string from, std::string to) {
  size_t pos = 0;
  while ((pos = str.find(from, pos)) != std::string::npos) {
    str.replace(pos, from.size(), to);
    pos += to.size(); // Move past the last replacement
  }
}
