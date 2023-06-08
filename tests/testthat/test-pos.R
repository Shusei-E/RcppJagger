test_that("pos", {
  skip_on_cran(); skip_on_os(c("windows", "linux", "sloaris"))
  sentence <- "日本語の文章の形態素解析を実験しています。\nこれが百二十五文目です。零時五十分予鈴。"
  expect_no_error(pos(sentence))
  expect_no_error(pos(sentence, format = "data.frame"))
  expect_error(pos(sentence, format = "tibble"))
  expect_no_error(pos(sentence, keep = c("動詞", "名詞")))
})

test_that("pos_sentences", {
  skip_on_cran(); skip_on_os(c("windows", "linux", "sloaris"))
  sentence <- "日本語の文章の形態素解析を実験しています。\nこれが二文目です。"
  sentence_tbl <- tibble::tibble(
    sentence = c(sentence, sentence)
  )
  expect_no_error(pos(sentence_tbl$sentence))
  expect_no_error(pos(sentence_tbl$sentence, keep = c("動詞", "名詞")))
  expect_error(pos(sentence_tbl, keep = c("動詞", "名詞")))
})

test_that("pos_simple", {
  skip_on_cran(); skip_on_os(c("windows", "linux", "sloaris"))
  sentence <- "日本語の文章の形態素解析を実験しています。\nこれが百二十五文目です。"
  expect_no_error(pos_simple(sentence))
  expect_no_error(pos_simple(sentence, format = "data.frame"))
  expect_error(pos_simple(sentence, format = "tibble"))
  expect_no_error(pos_simple(sentence, keep = c("動詞", "名詞")))
})


test_that("pos_simple_sentences", {
  skip_on_cran(); skip_on_os(c("windows", "linux", "sloaris"))
  sentence <- "日本語の文章の形態素解析を実験しています。\nこれが二文目です。"
  sentence_tbl <- tibble::tibble(
    sentence = c(sentence, sentence)
  )
  expect_no_error(pos_simple(sentence_tbl$sentence))
  expect_no_error(pos_simple(sentence_tbl$sentence, keep = c("動詞", "名詞")))
  expect_error(pos_simple(sentence_tbl, keep = c("動詞", "名詞")))
})

test_that("pos_model_path", {
  skip_on_cran(); skip_on_os(c("windows", "linux", "sloaris"))
  sentence <- "日本語の文章の形態素解析を実験しています。\nこれが二文目です。"
  model_path <- "/no/exist/"
  expect_no_error(pos(sentence, model_path = model_path))
  expect_no_error(pos_simple(sentence, model_path = model_path))
  model_path <- "/usr/lib/jagger/model/kwdlc"
  expect_no_error(pos(sentence, model_path = model_path))
  expect_no_error(pos_simple(sentence, model_path = model_path))
})
