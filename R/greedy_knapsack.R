#' @title Greedy knapsack function.
#' @description  Creating greedy algorithm to solve knapsack problem.
#' @details Input takes a data.frame x with two variables v and w and returns the maximum knapsack value and which elements (rows in the data.frame).
#' @param x A a data.frame x with two variables v and w.
#' @param W The knapsack size.
#' @return A list with the maximum knapsack value and which elements.
#' @export

greedy_knapsack <- function(x, W) {
  #check input
  if (is.data.frame(x) == FALSE | ncol(x) != 2) {
    stop()
  }
  if (W < 0 | sum(x < 0) > 1) {
    stop()
  }
  #initialization
  unit_value = matrix(0, NROW(x), 4)
  #unit_value
  for (i in 1:NROW(x)) {
    unit_value[i, 1] = x[i, 2] / x[i, 1]
    unit_value[i, 2] = x[i, 2] # value
    unit_value[i, 3] = x[i, 1]
    unit_value[i, 4] = i
  }
  unit_value = unit_value[order(unit_value[, 1]),]
  #result
  elements = c()# items that has been selected
  max_value = 0
  total_weight = 0
  for (i in NROW(unit_value):1) {
    max_value = max_value + unit_value[i, 2]
    total_weight = total_weight + unit_value[i, 3]
    elements = append(elements, unit_value[i, 4])
    if (total_weight + unit_value[i - 1, 3] > W) {
      break
    }
  }
  result = list(round(max_value), elements)
  names(result) <- c("value", "elements")
  return(result)
}
