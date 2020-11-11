
<!-- README.md is generated from README.Rmd. Please edit that file -->

# washoku <a href='https://https://uribo.github.io/washoku/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/washoku)](https://CRAN.R-project.org/package=washoku)
[![R build
status](https://github.com/uribo/washoku/workflows/R-CMD-check/badge.svg)](https://github.com/uribo/washoku/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of washoku is to …

## Installation

This package is not yet on CRAN, but can be installed from GitHub with:

    if (!requireNamespace("remotes"))
      install.packages("remotes")

    remotes::install_github("uribo/washoku")

## Usage

### recipe `step_*()`

-   `step_tokenize_jp()`
    -   Tokenizer (engine):
        [`Sudachi`](https://github.com/WorksApplications/Sudachi)
        ([sudachir](https://github.com/uribo/sudachir)),
        [`MeCab`](https://taku910.github.io/mecab/)
        ([RcppMeCab](https://github.com/junhewk/RcppMeCab))

<!-- -->

    library(washoku)
    library(sudachir)
    library(recipes)
    library(textrecipes)

    d <-
      tibble::tibble(
      id = c(1, 1, 2),
      txt = c("事実を読者の前に告白すると、去年の八月頃すでに自分の小説を紙上に連載すべきはずだったのである。",
              "吾輩は猫である。名前はまだ無い。",
              "国家公務員はかつ丼を食べたい。"))

**Sudachi**

    sudachir::install_sudachipy()

    reticulate::use_condaenv("r-sudachipy", required = TRUE)

    rec_sudachi <- 
      d %>% 
      recipe(id ~ txt) %>% 
      step_tokenize_jp(txt, 
                       engine = "sudachir", 
                       options = list(mode = "C", type = "surface", pos = TRUE))

    rec_sudachi <-
      rec_sudachi %>%
      textrecipes::step_pos_filter(txt, keep_tags = c("名詞", "動詞")) %>%
      textrecipes::step_untokenize(txt)

    bake(prep(rec_sudachi), new_data = NULL)
    #> Parsed to 32 tokens
    #> Parsed to 11 tokens
    #> Parsed to 7 tokens
    #> # A tibble: 3 x 2
    #>   txt                                                                      id
    #>   <fct>                                                                 <dbl>
    #> 1 事実 読者 前 告白 する 去年 八 月 頃 自分 小説 紙上 連載 す はず ある     1
    #> 2 猫 ある 名前                                                              1
    #> 3 国家公務員 かつ丼 食べ                                                    2

**MeCab**

    rec_mecab <- 
      d %>% 
      recipe(id ~ txt) %>% 
      step_tokenize_jp(txt, 
                       engine = "RcppMeCab",
                       options = NULL)

    rec_mecab <- 
      rec_mecab %>% 
      textrecipes::step_pos_filter(txt, keep_tags = c("名詞", "動詞")) %>%
      textrecipes::step_untokenize(txt)

    bake(prep(rec_mecab), new_data = NULL)
    #> # A tibble: 3 x 2
    #>   txt                                                                   id
    #>   <fct>                                                              <dbl>
    #> 1 事実 読者 前 告白 する 去年 八 月頃 自分 小説 紙上 連載 す はず の     1
    #> 2 吾輩 猫 名前                                                           1
    #> 3 国家 公務員 丼 食べ                                                    2
