test_that("tokenize", {
  skip_on_cran(); skip_on_os(c("windows", "linux", "sloaris"))
  sentence <- "日本語の文章の形態素解析を実験しています。\nこれが二文目です。"
  sentence_tbl <- tibble::tibble(
    sentence = c(sentence, sentence)
  )

  expect_no_error(tokenize(sentence, concat = TRUE))
  expect_no_error(tokenize(sentence, concat = FALSE))
  expect_no_error(tokenize(sentence_tbl$sentence, concat = FALSE))

  expect_error(tokenize_vec(sentence_tbl, keep = c("動詞", "名詞")))
})

test_that("tokenize_tbl", {
  skip_on_cran(); skip_on_os(c("windows", "linux", "sloaris"))
  sentence <- "日本語の文章の形態素解析を実験しています。\nこれが二文目です。"
  sentence_tbl <- tibble::tibble(
    sentence = c(sentence, sentence)
  )
  expect_no_error(tokenize_tbl(sentence_tbl, "sentence"))
  expect_no_error(tokenize_tbl(sentence_tbl, "sentence"))
  expect_no_error(tokenize_tbl(sentence_tbl, "sentence", keep = c("動詞", "名詞")))
  expect_error(tokenize_tbl(sentence_tbl, "no_exist", keep = c("動詞", "名詞")))
  expect_error(tokenize_tbl(sentence_tbl$sentence, keep = c("動詞", "名詞")))
})

test_that("tokenize_model_path", {
  skip_on_cran(); skip_on_os(c("windows", "linux", "sloaris"))
  sentence <- "日本語の文章の形態素解析を実験しています。\nこれが二文目です。"
  sentence_tbl <- tibble::tibble(
    sentence = c(sentence, sentence)
  )
  model_path <- "/no/exist/"
  expect_no_error(tokenize(sentence, model_path = model_path))
  expect_no_error(tokenize_tbl(sentence_tbl, "sentence", model_path = model_path))
})
