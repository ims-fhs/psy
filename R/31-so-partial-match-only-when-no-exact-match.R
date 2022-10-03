data <- c("a", "a b", "c d e", "c d", "lazy")

my_match <- function(data, pattern) {
  if (pattern %in% data) {
    return(which(pattern == data)[1])
  } else {
    return(grep(pattern, data, fixed = FALSE)[1])
  }
}

my_match <- function(data, pattern) {
 which.min(adist(pattern, data, partial=TRUE))
}


my_match <- function(data, pattern) {
  which.min(agrep(pattern, data))
}

my_match(data, "b")
my_match(data, "a")
my_match(data, "a b")
my_match(data, "c")
my_match(data, "e")
my_match(data, "c d")
my_match(data, "lasy")

my_match <- function(data, pattern) {
  i <- match(pattern, data)
  if(is.na(i)) {
    if(length(grep(pattern, data))>0) {
      which.min(adist(pattern, data, partial=TRUE))
    } else NA
  } else i
}

my_match <- function(data, pattern) ifelse(!is.na(match(pattern, data)), match(pattern, data)[1], which.min(adist(pattern, data, partial=TRUE)))
