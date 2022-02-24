`%|||%` <- function(x, otherwise) {
  if (is.null(x))
    return(otherwise)
  return(x)
}

is_empty <- Vectorize(function(x) {
  is.null(x) || length(x) == 0
})
