#include <Rcpp.h>
#include "../inst/include/jagger.cc"
  // comment out the `main()` function in jagger.cc
  // Edit the `_err()` function in `ccedar_core.h` not to use `stderr` and `exit`
  // Edit the `read_model()` function in `jagger.cc` not to use `stderr`
  // Edit `jagger.h` to use `mman.h` on Windows and comment out `err.h`
  // Edit `errx` in `jagger.h` and `jagger.cc`
  // Edit `write_buffer` in `jagger.cc` to pass Devian check on CRAN
#include "reader.h"
#include "pos.h"
#include "pos_simple.h"
#include "lemmatizer.h"
#include "tokenizer.h"

using namespace Rcpp;

//' POS tagging in C++
//'
//' @keywords internal
// [[Rcpp::export]]
List pos_cpp_vec(StringVector &inputs, std::string model_path) {
  model_path += "/patterns";
  std::string model (model_path);

  RcppJaggerPOS jagger;
  jagger.read_model (model);

  int inputs_size = inputs.size();
  std::vector<List> result;
  result.reserve(inputs_size);

  for (int i = 0; i < inputs_size; i++) {
    std::vector<std::string> token_vec, pos_vec, subtype_vec, lemma_vec;

    jagger.pos <BUF_SIZE, true> (Rcpp::as< std::string >(inputs[i]), token_vec, pos_vec, subtype_vec, lemma_vec);

    List result_input;
    result_input["token"] = wrap(token_vec);
    result_input["pos"] = wrap(pos_vec);
    result_input["subtype"] = wrap(subtype_vec);
    result_input["lemma"] = wrap(lemma_vec);

    result.push_back(result_input);
  }

  return wrap(result);
}


//' POS tagging in C++ (only token and pos)
//'
//' @keywords internal
// [[Rcpp::export]]
List pos_simple_cpp_vec(StringVector &inputs, std::string model_path) {
  model_path += "/patterns";
  std::string model (model_path);

  RcppJaggerPOSSimple jagger;
  jagger.read_model (model);

  int inputs_size = inputs.size();
  std::vector<List> result;
  result.reserve(inputs_size);

  for (int i = 0; i < inputs_size; i++) {
    std::vector<std::string> token_vec, pos_vec;

    jagger.pos <BUF_SIZE, true> (Rcpp::as< std::string >(inputs[i]), token_vec, pos_vec);

    List result_input_list;
    result_input_list["token"] = wrap(token_vec);
    result_input_list["pos"] = wrap(pos_vec);

    result.push_back(result_input_list);
  }

  return wrap(result);
}


//' Tokenizer (a vector input)
//'
//' @keywords internal
// [[Rcpp::export]]
StringVector tokenize_cpp_vec(StringVector &inputs, std::string model_path, StringVector &keep_vec, bool keep_all) {
  model_path += "/patterns";
  std::string model (model_path);
  RcppJaggerTokenizer jagger;
  jagger.read_model (model);

  int keep_num = keep_vec.size();
  std::vector<std::string> keep;
  keep.reserve(keep_num);
  for (int i = 0; i < keep_num; i++) {
    keep[i] = keep_vec[i];
  }

  int inputs_size = inputs.size();
  StringVector result_vec(inputs_size);

  for (int i = 0; i < inputs_size; i++) {
    std::vector<std::string> pos_vec, token_vec;

    jagger.pos <BUF_SIZE, true> (Rcpp::as< std::string >(inputs[i]), pos_vec, token_vec);

    std::stringstream res_str;
    int token_size = token_vec.size();
    for (int j = 0; j < token_size; j++) {
      if (keep_all) {
        if (j != 0) {
          res_str << " ";
        }
        res_str << token_vec[j];
      } else {
        for (int k = 0; k < keep_num; k++) {
          if (pos_vec[j] == keep[k]) {
            if (!res_str.str().empty()) {
              res_str << " ";
            }
            res_str << token_vec[j];
            break;
          }
        }
      }
    }
    result_vec[i] = res_str.str();
  }

  return result_vec;
}


//' Lemmatize (a vector input)
//'
//' @keywords internal
// [[Rcpp::export]]
StringVector lemmatize_cpp_vec(StringVector &inputs, std::string model_path, StringVector &keep_vec, bool keep_all) {
  model_path += "/patterns";
  std::string model (model_path);
  RcppJaggerLemmatize jagger;
  jagger.read_model (model);

  int keep_num = keep_vec.size();
  std::vector<std::string> keep;
  keep.reserve(keep_num);
  for (int i = 0; i < keep_num; i++) {
    keep[i] = keep_vec[i];
  }

  int inputs_size = inputs.size();
  StringVector result_vec(inputs_size);

  for (int i = 0; i < inputs_size; i++) {
    std::vector<std::string> pos_vec, lemma_vec;

    jagger.pos <BUF_SIZE, true> (Rcpp::as< std::string >(inputs[i]), pos_vec, lemma_vec);

    std::stringstream res_str;
    int lemma_size = lemma_vec.size();
    for (int j = 0; j < lemma_size; j++) {
      if (keep_all) {
        if (j != 0) {
          res_str << " ";
        }
        res_str << lemma_vec[j];
      } else {
        for (int k = 0; k < keep_num; k++) {
          if (pos_vec[j] == keep[k]) {
            if (!res_str.str().empty()) {
              res_str << " ";
            }
            res_str << lemma_vec[j];
            break;
          }
        }
      }
    }
    result_vec[i] = res_str.str();
  }

  return result_vec;
}
