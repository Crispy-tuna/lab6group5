#' @title Brute force search.
#' @description Creating Brute force search algorithm to solve knapsack problem.
#' @details Input takes a data.frame x with two variables v and w and returns the maximum knapsack value and which elements (rows in the data.frame).
#' @param x A a data.frame x with two variables v and w.
#' @param W The knapsack size.
#' @return A list with the maximum knapsack value and which elements.
#' @export

brute_force_knapsack <- function(x, W) {
  #check input
  if (is.data.frame(x) == FALSE | ncol(x) != 2) {
    stop()
  }
  if (W < 0 | sum(x < 0) > 1) {
    stop()
  }
  #all combinations
  choice_matrix = matrix(0, 2 ** NROW(x) - 1, NROW(x))
  for (i in 1:2 ** nrow(x) - 1) {
    a = intToBits(i)
    for (j in 1:NROW(x)) {
      choice_matrix[i, j] = as.integer(intToBits(i)[j])
    }
  }

  #calculate weight and value
  value_matrix = matrix(0, 2 ** NROW(x) - 1, 2)
  # total weight and total value of each combination
  for (i in 1:nrow(choice_matrix)) {
    # i is row, 1:255
    total_w = 0
    total_v = 0
    for (j in 1:ncol(choice_matrix)) {
      # j is column, 1:nrow(x)
      if (choice_matrix[i, j] == 1) {
        total_w = total_w + x[j, 1]
        total_v = total_v + x[j, 2]
      }
    }
    value_matrix[i, 1] = total_w
    value_matrix[i, 2] = total_v
  }

  # get the maximum value
  max_value = 0
  tag = 0 # the row of maximum value
  for (i in 1:nrow(value_matrix)) {
    if (value_matrix[i, 1] < W & value_matrix[i, 2] > max_value) {
      max_value = round(value_matrix[i, 2])
      tag = i
    }
  }

  #return a result list
  elements = c()
  for (i in 1:NCOL(choice_matrix)) {
    if (choice_matrix[tag, i] == 1) {
      elements = append(elements, i)
    }
  }
  result = list(max_value, elements)
  names(result) <- c("value", "elements")
  return(result)

}
