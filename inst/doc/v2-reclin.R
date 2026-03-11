## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
data.table::setDTthreads(1)

## ----setup--------------------------------------------------------------------
library(blocking)
library(data.table)

## -----------------------------------------------------------------------------
data(census)
data(cis)
setDT(census)
setDT(cis)

## -----------------------------------------------------------------------------
head(census)

## -----------------------------------------------------------------------------
head(cis)

## -----------------------------------------------------------------------------
set.seed(2024)
census <- census[sample(nrow(census), floor(nrow(census) / 2)), ]
cis <- cis[sample(nrow(cis), floor(nrow(cis) / 2)), ]

## -----------------------------------------------------------------------------
census[, txt:=paste0(pername1, pername2, sex, dob_day, dob_mon, dob_year, enumcap, enumpc)]
cis[, txt:=paste0(pername1, pername2, sex, dob_day, dob_mon, dob_year, enumcap, enumpc)]

## -----------------------------------------------------------------------------
result1 <- blocking(x = census$txt, y = cis$txt, verbose = 1)

## -----------------------------------------------------------------------------
hist(result1$result$dist, main = "Distribution of distances between pairs", xlab = "Distances")

## -----------------------------------------------------------------------------
head(result1$result, n = 10)

## -----------------------------------------------------------------------------
cbind(t(census[1, c(1:7, 9:10)]), t(cis[12088, 1:9]))

## -----------------------------------------------------------------------------
matches <- merge(x = census[, .(x=1:.N, person_id)],
                 y = cis[, .(y = 1:.N, person_id)],
                 by = "person_id")
matches[, block:=1:.N]
head(matches)

## -----------------------------------------------------------------------------
result2 <- blocking(x = census$txt, y = cis$txt, verbose = 1,
                    true_blocks = matches[, .(x, y, block)])

## -----------------------------------------------------------------------------
result2

## -----------------------------------------------------------------------------
ann_control_pars <- controls_ann()
ann_control_pars$nnd$epsilon <- 0.2

result3 <- blocking(x = census$txt, y = cis$txt, verbose = 1, 
                    true_blocks = matches[, .(x, y, block)], 
                    control_ann = ann_control_pars)

## -----------------------------------------------------------------------------
result3

## -----------------------------------------------------------------------------
result4 <- blocking(x = census$txt, y = cis$txt, verbose = 1, 
                    true_blocks = matches[, .(x, y, block)], 
                    ann = "hnsw")

## -----------------------------------------------------------------------------
result4

## -----------------------------------------------------------------------------
c("no tuning" = mean(result2$result[order(y)]$x == result4$result[order(y)]$x)*100,
  "with tuning" = mean(result3$result[order(y)]$x == result4$result[order(y)]$x)*100)

