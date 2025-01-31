% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/greedy_knapsack.R
\name{greedy_knapsack}
\alias{greedy_knapsack}
\title{Greedy knapsack function.}
\usage{
greedy_knapsack(x, W)
}
\arguments{
\item{x}{A a data.frame x with two variables v and w.}

\item{W}{The knapsack size.}
}
\value{
A list with the maximum knapsack value and which elements.
}
\description{
Creating greedy algorithm to solve knapsack problem.
}
\details{
Input takes a data.frame x with two variables v and w and returns the maximum knapsack value and which elements (rows in the data.frame).
}
