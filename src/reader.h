#ifndef __RcppJaggerReader__INCLUDED__
#define __RcppJaggerReader__INCLUDED__
#include <Rcpp.h>
using namespace Rcpp;

class simple_string_reader {
  private:
    std::string _input;
    std::string _line;
    size_t _next_line_start;

  public:
    simple_string_reader(const std::string& input) : _input(input), _next_line_start(0) {}

    size_t get_line_len (char** line) {
      if (_next_line_start >= _input.size()) return 0;  // no more lines to read

      size_t line_end = _input.find('\n', _next_line_start);
      if (line_end == std::string::npos) line_end = _input.size();

      _line = _input.substr(_next_line_start, line_end - _next_line_start);
      _next_line_start = line_end + 1;

      *line = &_line[0];
      return _line.size();
    }
};

#endif