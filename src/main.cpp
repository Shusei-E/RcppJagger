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
List pos_cpp_vec(StringVector &inputs, std::string model_path, StringVector &keep_vec, bool keep_all) {
  model_path += "/patterns";
  std::string model (model_path);

  RcppJaggerPOS jagger;
  jagger.read_model (model);

  int inputs_size = inputs.size();
  std::vector<List> result;
  result.reserve(inputs_size);

  int keep_num = keep_vec.size();
  std::unordered_set<std::string> keep;
  for (int i = 0; i < keep_num; i++) {
    std::string keep_str = Rcpp::as< std::string >(keep_vec[i]);
    keep.insert(keep_str);
  }

  for (int i = 0; i < inputs_size; i++) {
    std::vector<std::string> token_vec, pos_vec, subtype_vec, lemma_vec;

    jagger.pos <BUF_SIZE, true> (Rcpp::as< std::string >(inputs[i]), token_vec, pos_vec, subtype_vec, lemma_vec);

    std::vector<std::string> kept_token_vec, kept_pos_vec, kept_subtype_vec, kept_lemma_vec;
    kept_token_vec.reserve(token_vec.size());
    kept_pos_vec.reserve(pos_vec.size());
    kept_subtype_vec.reserve(subtype_vec.size());
    kept_lemma_vec.reserve(lemma_vec.size());

    for (size_t j = 0; j < pos_vec.size(); ++j) {
      if (keep_all || keep.find(pos_vec[j]) != keep.end()) {
        kept_token_vec.push_back(token_vec[j]);
        kept_pos_vec.push_back(pos_vec[j]);
        kept_subtype_vec.push_back(subtype_vec[j]);
        kept_lemma_vec.push_back(lemma_vec[j]);
      }
    }

    List result_input;
    result_input["token"] = wrap(kept_token_vec);
    result_input["pos"] = wrap(kept_pos_vec);
    result_input["subtype"] = wrap(kept_subtype_vec);
    result_input["lemma"] = wrap(kept_lemma_vec);

    result.push_back(result_input);
  }

  return wrap(result);
}


//' POS tagging in C++ (only token and pos)
//'
//' @keywords internal
// [[Rcpp::export]]
List pos_simple_cpp_vec(StringVector &inputs, std::string model_path, StringVector &keep_vec, bool keep_all) {
  model_path += "/patterns";
  std::string model (model_path);

  RcppJaggerPOSSimple jagger;
  jagger.read_model (model);

  int inputs_size = inputs.size();
  std::vector<List> result;
  result.reserve(inputs_size);

  int keep_num = keep_vec.size();
  std::unordered_set<std::string> keep;
  for (int i = 0; i < keep_num; i++) {
    std::string keep_str = Rcpp::as< std::string >(keep_vec[i]);
    keep.insert(keep_str);
  }

  for (int i = 0; i < inputs_size; i++) {
    std::vector<std::string> token_vec, pos_vec;

    jagger.pos <BUF_SIZE, true> (Rcpp::as< std::string >(inputs[i]), token_vec, pos_vec);

    std::vector<std::string> kept_token_vec, kept_pos_vec;
    kept_token_vec.reserve(token_vec.size());
    kept_pos_vec.reserve(pos_vec.size());

    for (size_t j = 0; j < pos_vec.size(); ++j) {
      if (keep_all || keep.find(pos_vec[j]) != keep.end()) {
        kept_token_vec.push_back(token_vec[j]);
        kept_pos_vec.push_back(pos_vec[j]);
      }
    }

    List result_input;
    result_input["token"] = wrap(kept_token_vec);
    result_input["pos"] = wrap(kept_pos_vec);

    result.push_back(result_input);
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
  std::unordered_set<std::string> keep;
  for (int i = 0; i < keep_num; i++) {
    std::string keep_str = Rcpp::as< std::string >(keep_vec[i]);
    keep.insert(keep_str);
  }

  int inputs_size = inputs.size();
  StringVector result_vec(inputs_size);

  for (int i = 0; i < inputs_size; i++) {
    std::vector<std::string> pos_vec, token_vec;

    jagger.pos <BUF_SIZE, true> (Rcpp::as< std::string >(inputs[i]), pos_vec, token_vec);

    std::stringstream res_str;
    int token_size = token_vec.size();
    for (int j = 0; j < token_size; j++) {
      if (keep_all || keep.find(pos_vec[j]) != keep.end()) {
        if (!res_str.str().empty()) {
          res_str << " ";
        }
        res_str << token_vec[j];
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
  std::unordered_set<std::string> keep;
  for (int i = 0; i < keep_num; i++) {
    std::string keep_str = Rcpp::as< std::string >(keep_vec[i]);
    keep.insert(keep_str);
  }

  int inputs_size = inputs.size();
  StringVector result_vec(inputs_size);

  for (int i = 0; i < inputs_size; i++) {
    std::vector<std::string> pos_vec, lemma_vec;

    jagger.pos <BUF_SIZE, true> (Rcpp::as< std::string >(inputs[i]), pos_vec, lemma_vec);

    std::stringstream res_str;
    int lemma_size = lemma_vec.size();
    for (int j = 0; j < lemma_size; j++) {
      if (keep_all || keep.find(pos_vec[j]) != keep.end()) {
        if (!res_str.str().empty()) {
          res_str << " ";
        }
        res_str << lemma_vec[j];
      }
    }
    result_vec[i] = res_str.str();
  }

  return result_vec;
}
