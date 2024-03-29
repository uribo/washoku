% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenize.R
\name{step_tokenize_jp}
\alias{step_tokenize_jp}
\title{Tokenization of Japanese character variables}
\usage{
step_tokenize_jp(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  engine = "sudachir",
  options = list(mode = "A", type = "surface", pos = TRUE, instance = NULL),
  skip = FALSE,
  id = rand_id("tokenize_jp")
)
}
\arguments{
\item{recipe}{A \link{recipe} object. The step will be added to the
sequence of operations for this recipe.}

\item{...}{One or more selector functions to choose which
variables are affected by the step. See \code{\link[recipes:selections]{recipes::selections()}}
for more details.}

\item{role}{Not used by this step since no new variables are
created.}

\item{trained}{A logical to indicate if the quantities for
preprocessing have been estimated.}

\item{columns}{A character string of variable names that will
be populated (eventually) by the \code{terms} argument. This is \code{NULL}
until the step is trained by \code{\link[recipes:prep]{recipes::prep.recipe()}}.}

\item{engine}{Implement token engine package. Defaults to 'sudachir'.}

\item{options}{list. path to engine's function.}

\item{skip}{A logical. Should the step be skipped when the
recipe is baked by \code{\link[recipes:bake]{recipes::bake.recipe()}}? While all operations are baked
when \code{\link[recipes:prep]{recipes::prep.recipe()}} is run, some operations may not be able to be
conducted on new data (e.g. processing the outcome variable(s)).
Care should be taken when using \code{skip = FALSE}.}

\item{id}{A character string that is unique to this step to identify it.}
}
\description{
\code{\link[=step_tokenize_jp]{step_tokenize_jp()}} creates a \emph{specification} of a recipe step that
will convert a character predictor into a \link{tokenlist_jp}.
}
\details{
The following packages are available for the \code{engine}.
\itemize{
\item sudachir (Sudachi)
\item RcppMeCab (MeCab)
}
}
