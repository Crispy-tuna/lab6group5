#' Knapsack objects with their weight(w) and value(v).
#'
#' A data set containing the item weight and item value of almost 2,000
#' knapsack objects.
#' @name knapsack_objects
#' @docType data
#' @format A data frame with 2000 rows and 2 variables:
NULL

RNGversion(min(as.character(getRversion()), "3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(w = sample(1:4000, size = n, replace = TRUE),
             v = runif(n = n, 0, 10000))
