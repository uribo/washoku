#' Token list for Japanese character
#'
#' @param tokens list
#' @rdname tokenlist_jp
#' @export
tokenlist_jp <- function(tokens = list()) {
  pos <-
    tokens %>%
    purrr::map(names) %>%
    vctrs::vec_cast(list())
  tokens <-
    tokens %>%
    purrr::map(unname) %>%
    vctrs::vec_cast(list())
  unique_tokens <- unique(unlist(tokens))
  new_tokenlist(tokens = tokens,
                pos = pos,
                unique_tokens = unique_tokens %||% character())
}

new_tokenlist <- function(tokens = list(), pos = NULL, unique_tokens = character()) {
  vctrs::vec_assert(tokens, list())
  vctrs::vec_assert(unique_tokens, character())
  if (length(tokens) == 0) {
    return(vctrs::new_rcrd(list(tokens = tokens),
                           unique_tokens = unique_tokens,
                           class = "textrecipes_tokenlist"))
  }
  vctrs::new_rcrd(purrr::compact(list(tokens = tokens,
                                      pos = pos)),
                  unique_tokens = unique_tokens,
                  class = "textrecipes_tokenlist")
}
