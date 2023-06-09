# Sanity check

test_that("pos-pos_simple", {
  skip_on_cran(); skip_on_os(c("windows", "linux", "sloaris"))
  sentence <- "日本語の文章の形態素解析を実験しています。\nこれが百二十五文目です。零時五十分予鈴。"
  sentences <- c(sentence, "2つ目の文章を追加します。")

  expect_identical(pos(sentence)[[1]]$token, pos_simple(sentence)[[1]]$token)
  expect_identical(pos(sentences)[[2]]$token, pos_simple(sentences)[[2]]$token)
  expect_identical(pos(sentences, format = "data.frame")[[1]]$token, pos_simple(sentences, format = "data.frame")[[1]]$token)
  expect_identical(pos(sentences, format = "data.frame", keep = c("動詞", "名詞"))[[1]]$token, pos_simple(sentences, format = "data.frame", keep = c("動詞", "名詞"))[[1]]$token)
  expect_identical(pos(sentences, format = "data.frame")[[2]]$token, pos_simple(sentences, format = "data.frame")[[2]]$token)

  expect_identical(pos(sentence)[[1]]$pos, pos_simple(sentence)[[1]]$pos)
  expect_identical(pos(sentences)[[2]]$pos, pos_simple(sentences)[[2]]$pos)
})

test_that("pos-lemmatize", {
  skip_on_cran(); skip_on_os(c("windows", "linux", "sloaris"))
  sentence <- "日本語の文章の形態素解析を実験しています。\nこれが百二十五文目です。零時五十分予鈴。"
  sentences <- c(sentence, "2つ目の文章を追加します。")

  expect_identical(pos(sentence)[[1]]$lemma, lemmatize(sentence, concat = FALSE)[[1]])
  expect_identical(pos(sentence, keep = c("動詞", "名詞"))[[1]]$lemma, lemmatize(sentence, , keep = c("動詞", "名詞"), concat = FALSE)[[1]])
  expect_identical(pos(sentences)[[2]]$lemma, lemmatize(sentences, concat = FALSE)[[2]])
})

test_that("pos-tokenize", {
  skip_on_cran(); skip_on_os(c("windows", "linux", "sloaris"))
  sentence <- "日本語の文章の形態素解析を実験しています。\nこれが百二十五文目です。零時五十分予鈴。"
  sentences <- c(sentence, "12の文章を追加します。")

  expect_identical(pos(sentence)[[1]]$token, tokenize(sentence, concat = FALSE)[[1]])
  expect_identical(pos(sentences)[[2]]$token, tokenize(sentences, concat = FALSE)[[2]])
})

