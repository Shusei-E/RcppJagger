#ifndef __RcppJaggerTokenizer__INCLUDED__
#define __RcppJaggerTokenizer__INCLUDED__

#include <Rcpp.h>
#include "reader.h"
using namespace Rcpp;

// Inherit jagger class for R
// It inherits the tagger class in jagger.cc
// We need to comment out the `main()` function in the `inst/include/` folder.
class RcppJaggerTokenizer : public jagger::tagger {
  public:
    // Constructor
    RcppJaggerTokenizer() : jagger::tagger() {};

    // POS tagger  (need to change `c2i` etc from private to protected)
    template <const int BUF_SIZE_, const bool POS_TAGGING>
    void pos(const std::string &inputstring, std::vector<std::string> &pos_vec, std::vector<std::string> &token_vec) {
      // Store results in R vectors

      simple_string_reader reader(inputstring);
      char* line = nullptr;

      while (const size_t len = reader.get_line_len(&line)) {
        int bytes (0), bytes_prev (0), id (0), ctype (0), ctype_prev (0);
        uint64_t offsets = c2i[CP_MAX + 1];
        bool bos (true), ret (line[len - 1] == '\n'), concat (false);
        for (const char *p (line), * const p_end (p + len - ret); p != p_end; bytes_prev = bytes, ctype_prev = ctype, offsets = p2f[static_cast <size_t> (id)], p += bytes) {
          const int r = da.longestPrefixSearchWithPOS (p, p_end, offsets & 0x3fff, &c2i[0]); // found word
          id    = r & 0xfffff;
          bytes = (r >> 23) ? (r >> 23) : u8_len (p);
          ctype = (r >> 20) & 0x7; // 0: num|unk / 1: alpha / 2: kana / 3: other
          if (! bos) { // word that may concat with the future context
            if (ctype_prev != ctype || // different character types
                ctype_prev == 3 ||     // seen words in non-num/alpha/kana
                (ctype_prev == 2 && bytes_prev + bytes >= 18)) {
              if (POS_TAGGING) {
                if (concat) {
                  store_result(&fs[(offsets >> 34)], (offsets >> MAX_KEY_BITS) & 0x7f, pos_vec);
                  store_result(",*,*,*\n", 7, pos_vec);
                } else {
                  store_result(&fs[(offsets >> 34)], (offsets >> (MAX_KEY_BITS + MAX_FEATURE_BITS)) & 0x3ff, pos_vec);
                }
                concat = false;
              } else {
                store_result(" ", 1, pos_vec);
              }
            } else {
              concat = true;
            }
          } else {
            bos = false;
          }
          store_token(p, static_cast <size_t> (bytes), token_vec, concat);
        }
        if (! bos) // output fs of last token
          if (POS_TAGGING) {
            if (concat) {
              store_result(&fs[(offsets >> 34)], (offsets >> MAX_KEY_BITS) & 0x7f, pos_vec);
              store_result(",*,*,*\n", 7, pos_vec);
            } else {
              store_result (&fs[(offsets >> 34)], (offsets >> (MAX_KEY_BITS + MAX_FEATURE_BITS)) & 0x3ff,  pos_vec);
            }
          }
    }
  }

  void store_result(const char* s, size_t len, std::vector<std::string> &pos_vec) {
    const char* end = s + len;
    s++;  // skipping the first character \t
    len--;

    // Find the position of the first comma
    const char* first_comma = std::find(s, end, ',');

    // Store the element
    std::string_view first_element(s, first_comma - s);
    if (first_element != "*") {
        pos_vec.emplace_back(first_element);
    }
  }

  void store_token(const char* s, size_t len, std::vector<std::string> &token_vec, bool concat) {
    if (!concat || token_vec.empty()) {
      token_vec.emplace_back(s, len);
    } else { // concat to the previous token
      token_vec.back().append(s, len);
    }
  }
};

#endif