context("Tokenlist")

test_that("multiplication works", {
  tokens <- list(purrr::set_names(c(intToUtf8(c(26519, 27278))),
                                  c(intToUtf8(c(21517, 35422)))))
  expect_true(is.list(tokens))
  expect_length(tokenlist_jp(tokens),
                length(list))
  expect_s3_class(tokenlist_jp(tokens),
                  "textrecipes_tokenlist")
})
