test_that("lemmatize", {
  skip_on_cran(); skip_on_os(c("windows", "linux", "sloaris"))
  sentence <- "日本語の文章の形態素解析を実験しています。\nこれが二文目です。"
  sentence_tbl <- tibble::tibble(
    sentence = c(sentence, sentence)
  )

  expect_no_error(lemmatize(sentence, concat = FALSE))
  expect_no_error(lemmatize(sentence, concat = TRUE))
  expect_no_error(lemmatize(sentence_tbl$sentence, keep = c("動詞", "名詞")))
})

test_that("lemmatize_tbl", {
  skip_on_cran(); skip_on_os(c("windows", "linux", "sloaris"))
  sentence <- "日本語の文章の形態素解析を実験しています。\nこれが二文目です。"
  sentence_tbl <- tibble::tibble(
    sentence = c(sentence, sentence)
  )
  expect_no_error(lemmatize_tbl(sentence_tbl, "sentence"))
  expect_no_error(lemmatize_tbl(sentence_tbl, "sentence", keep = c("動詞", "名詞")))
  expect_error(lemmatize_tbl(sentence_tbl, "no_exist", keep = c("動詞", "名詞")))
  expect_error(lemmatize_tbl(sentence_tbl$sentence, keep = c("動詞", "名詞")))
})
