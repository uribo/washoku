#' Tokenization of Japanese character variables
#'
#' [step_tokenize_jp()] creates a *specification* of a recipe step that
#'will convert a character predictor into a [tokenlist_jp].
#' @inheritParams textrecipes::step_tokenize
#' @param engine Implement token engine package. Defaults to 'sudachir'.
#' @param options list. path to engine's function.
#' @details
#' The following packages are available for the `engine`.
#' - sudachir (Sudachi)
#' - RcppMeCab (MeCab)
#' @export
step_tokenize_jp <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  engine = "sudachir",
  options = list(mode = "A",
                 type = "surface",
                 pos = TRUE),
  skip = FALSE,
  id = rand_id("tokenize_jp")) {
  recipes::recipes_pkg_check(required_pkgs.step_tokenize_jp())
  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_tokenize_jp_new(
      terms = terms,
      trained = trained,
      role = role,
      columns = columns,
      engine = engine,
      options = options,
      skip = skip,
      id = id
    )
  )
}

step_tokenize_jp_new <-
  function(terms, role, trained, columns, engine, options, skip, id) {
    rlang::arg_match(engine,
                     c("sudachir", "RcppMeCab"))
    step(
      subclass = "tokenize_jp",
      terms = terms,
      role = role,
      trained = trained,
      engine = engine,
      columns = columns,
      options = options,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tokenize_jp <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(terms = x$terms, info = info)
  step_tokenize_jp_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    columns = col_names,
    engine = x$engine,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_tokenize_jp <- function(object, new_data, ...) {
  col_names <- object$columns
  for (i in seq_along(col_names)) {
    new_data[, col_names[i]] <- tokenizer_fun(data = new_data[, col_names[i]],
                                              name = col_names[i],
                                              engine = object$engine,
                                              options = object$options)
  }
  tibble::as_tibble(new_data)
}

tokenizer_fun <- function(data, name, engine, options, ...) {
  recipes::check_type(data[, name], quant = FALSE)
  data <-
    factor_to_text(data, name)
  if (engine == "sudachir") {
    token_list <-
      rlang::exec("form",
                  x = data[, 1, drop = TRUE],
                  !!!options)
  } else if (engine == "RcppMeCab") {
    token_list <-
      rlang::exec("pos_purrr",
                  x = data[, 1, drop = TRUE],
                  format = "list",
                  join = FALSE,
                  !!!options)
  }
  out <-
    tibble::tibble(tokenlist_jp(token_list))
  names(out) <- name
  out
}

print.step_tokenize_jp <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Text tokenizing for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
}

#' S3 methods for tracking which additional packages are needed for steps.
#'
#' Recipe-adjacent packages always list themselves as a required package so that
#' the steps can function properly within parallel processing schemes.
#' @param x A recipe step
#' @return A character vector
#' @rdname required_pkgs.step
#' @keywords internal
#' @export
required_pkgs.step_tokenize_jp <- function(x, ...) {
  c("sudachir", "RcppMeCab", "textrecipes")
}

pos_purrr <- function(x, ...) {
  x %>%
    purrr::map(
      ~ unname(RcppMeCab::pos(sentence = .x, ...))
    ) %>%
    purrr::flatten()
}

factor_to_text <- function(data, names) {
  for (i in seq_along(names)) {
    if (is.factor(data[, names[i], drop = TRUE]))
      data[, names[i]] <-
        as.character.factor(data[, names[i], drop = TRUE])
  }
  data
}
