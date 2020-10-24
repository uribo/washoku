#' #' Tokenization of Japanese character variables
#'
#' [step_tokenize_jp()] creates a *specification* of a recipe step that
#'will convert a character predictor into a [tokenlist_jp].
#' @inheritParams textrecipes::step_tokenize
#' @param options list
#' @export
step_tokenize_jp <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  options = list(mode = "A",
                 type = "surface",
                 pos = TRUE),
  skip = FALSE,
  id = rand_id("tokenize_jp")
) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_tokenize_jp_new(
      terms = terms,
      trained = trained,
      role = role,
      columns = columns,
      options = options,
      skip = skip,
      id = id
    )
  )
}

step_tokenize_jp_new <-
  function(terms, role, trained, columns, options, skip, id) {
    step(
      subclass = "tokenize_jp",
      terms = terms,
      role = role,
      trained = trained,
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
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_tokenize_jp <- function(object, new_data, ...) {
  col_names <- object$columns

  for (i in seq_along(col_names)) {
    new_data[, col_names[i]] <- tokenizer_fun(new_data[, col_names[i]],
                                              col_names[i],
                                              options = object$options)
  }
  tibble::as_tibble(new_data)
}

tokenizer_fun <- function(data, name, options, ...) {
  recipes::check_type(data[, name], quant = FALSE)
  token_list <-
    rlang::exec("form", x = data[, 1, drop = TRUE], !!!options)

  #if (textrecipes:::is_tokenlist(token_list)) {
  #  out <- tibble::tibble(token_list)
  #} else {
    #out <- tibble::tibble(tokenlist_jp(tokens = token_list))
    out <- tibble::tibble(tokenlist_jp(token_list))
  #}
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
  c("sudachir", "textrecipes")
}
