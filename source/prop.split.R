#' Function to split data
#'
#' Split \code{N} points in proportions given by \code{p}
#'
#' @param N number of points
#' @param p vector of proportions. Must sum 1.
#' @return A list of \code{length(p)} vectors, with sample indexes from \code{1} to \code{N}.
prop.split = function(N, p){
  idx = sample(1:N)
  split.idx = c(0, ceiling(cumsum(p)*N))
  lapply(1:length(p), function(i){idx[(split.idx[i]+1):split.idx[i+1]]})
}
