## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
data.table::setDTthreads(1)

## ----setup--------------------------------------------------------------------
library(blocking)
library(data.table)
library(reclin2)

## -----------------------------------------------------------------------------
data(census)
data(cis)
setDT(census)
setDT(cis)
census[, x:=1:.N]
cis[, y:=1:.N]

## -----------------------------------------------------------------------------
pair_ann(x = census[1:1000], 
         y = cis[1:1000], 
         on = c("pername1", "pername2", "sex", "dob_day", "dob_mon", "dob_year", "enumcap", "enumpc"), 
         deduplication = FALSE) |>
  head()

## -----------------------------------------------------------------------------
pair_ann(x = census[1:1000], 
         y = cis[1:1000], 
         on = c("pername1", "pername2", "sex", "dob_day", "dob_mon", "dob_year", "enumcap", "enumpc"), 
         deduplication = FALSE,
         ann = "hnsw") |>
  compare_pairs(on = c("pername1", "pername2", "sex", "dob_day", "dob_mon", "dob_year", "enumcap", "enumpc"),
                comparators = list(cmp_jarowinkler())) |>
  score_simple("score",
               on = c("pername1", "pername2", "sex", "dob_day", "dob_mon", "dob_year", "enumcap", "enumpc")) |>
  select_threshold("threshold", score = "score", threshold = 6) |>
  link(selection = "threshold") |>
  head()

