#ifndef __RcppJaggerLemmatizer__INCLUDED__
#define __RcppJaggerLemmatizer__INCLUDED__
#include <Rcpp.h>
#include "reader.h"
using namespace Rcpp;

// Inherit jagger class for R
// It inherits the tagger class in jagger.cc
// We need to comment out the `main()` function in the `inst/include/` folder.
class RcppJaggerLemmatize : public jagger::tagger {
  public:
    // Constructor
    RcppJaggerLemmatize() : jagger::tagger() {};

    // POS tagger  (need to change `c2i` etc from private to protected)
    template <const int BUF_SIZE_, const bool POS_TAGGING>
    void pos(const std::string &inputstring, std::vector<std::string> &pos_vec, std::vector<std::string> &lemma_vec) {
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
                  store_result(&fs[(offsets >> 34)], (offsets >> MAX_KEY_BITS) & 0x7f, pos_vec, lemma_vec);
                  store_result(",*,*,*\n", 7, pos_vec, lemma_vec);
                } else {
                  store_result(&fs[(offsets >> 34)], (offsets >> (MAX_KEY_BITS + MAX_FEATURE_BITS)) & 0x3ff, pos_vec, lemma_vec);
                }
                concat = false;
              } else {
                store_result(" ", 1, pos_vec, lemma_vec);
              }
            } else {
              concat = true;
            }
          } else {
            bos = false;
          }
        }
        if (! bos) // output fs of last token
          if (POS_TAGGING) {
            if (concat) {
              store_result(&fs[(offsets >> 34)], (offsets >> MAX_KEY_BITS) & 0x7f, pos_vec, lemma_vec);
              store_result(",*,*,*\n", 7, pos_vec, lemma_vec);
            } else {
              store_result (&fs[(offsets >> 34)], (offsets >> (MAX_KEY_BITS + MAX_FEATURE_BITS)) & 0x3ff, pos_vec, lemma_vec);
            }
          }
    }
  }

  void store_result(const char* s, size_t len, std::vector<std::string> &pos_vec, std::vector<std::string> &lemma_vec) {
    std::string_view pos_info(s+1, len-1);  // skipping the first character

    // Declare a vector of string_view `parts` to hold the parts of the string `pos_info` split by commas.
    std::vector<std::string_view> parts;
    // Initialize `start` and `end` to point to the start of the string and the position of the first comma.
    size_t start = 0;
    size_t end = pos_info.find(',');

    // Loop through the string, finding each comma and splitting the string into parts.
    while (end != std::string_view::npos) {
        parts.emplace_back(pos_info.substr(start, end - start));
        start = end + 1;
        end = pos_info.find(',', start);
    }
    // Add the final part after the last comma to `parts`.
    parts.emplace_back(pos_info.substr(start));

    if (parts[0] != "*" && parts.size() >= 6) {  // first appearance of the token (i.e. not a concatenation)
      pos_vec.emplace_back(parts[0]);
      lemma_vec.emplace_back(parts[parts.size() - 3]);
    } else if (parts[0] != "*" && parts.size() == 4) {  // concatenation
      pos_vec.emplace_back(parts[0]);
      lemma_vec.emplace_back(parts[parts.size() - 1]);
    }
  }
};

#endif