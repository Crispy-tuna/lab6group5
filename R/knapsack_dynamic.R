#' @title Dynamic knapsack function.
#' @description Creating dynamic algorithm to solve knapsack problem.
#' @details Input takes a data.frame x with two variables v and w and returns the maximum knapsack value and which elements (rows in the data.frame).
#' @param x A a data.frame x with two variables v and w.
#' @param W The knapsack size.
#' @return A list with the maximum knapsack value and which elements.
#' @export

knapsack_dynamic <- function(x, W) {
  if (is.data.frame(x) == FALSE | ncol(x) != 2) {
    stop()
  }
  if (W < 0 | sum(x < 0) > 1) {
    stop()
  }
  x$w <- as.numeric(x$w)
  n <- nrow(x)
  m <- matrix(0, nrow = n + 1, ncol = W + 1)
  e <- matrix("", nrow = n + 1, ncol = W + 1)
  for (j in 2:(W + 1)) {
    for (i in 2:(n + 1)) {
      if (x$w[i - 1] > (j - 1)) {
        m[i, j] <- m[i - 1, j]
        e[i, j] <- e[i - 1, j]
      } else{
        if (m[i - 1, j] >= m[i - 1, j - x$w[i - 1]] + x$v[i - 1]) {
          m[i, j] <- m[i - 1, j]
          e[i, j] <- e[i - 1, j]
        } else{
          m[i, j] <- m[i - 1, j - x$w[i - 1]] + x$v[i - 1]
          e[i, j] <-
            paste(e[i - 1, j - x$w[i - 1]], i - 1, sep = " ")
        }
      }
      i <- i + 1
    }
    j <- j + 1
  }
  e <-
    as.numeric(unlist(strsplit(e[which(m == max(m))][1], split = " ")))
  l <-
    list(value = round(max(m)), elements = e[which(is.na(e) == FALSE)])
  return(l)
}
