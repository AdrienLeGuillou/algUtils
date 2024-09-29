#' Return merged sequences of begining and ends
#'
#' @param beg start of each sequence
#' @param end end of each sequence
#' @param na.rm boolean, do we remove sequences containing NAs?
#'
#' @return a list of 2, beg and end, the merged sequences
#'
#' @export
merge_sequences <- function(beg, end, na.rm = FALSE) {
  if (length(beg) != length(end)) stop("Not the same length")

  nas <- which(is.na(beg) | is.na(end))
  if (length(nas) > 0) {
    if (na.rm) {
      beg <- beg[-nas]
      end <- end[-nas]
    } else {
      stop("No NA allowed if `na.rm = FALSE`")
    }
  }

  for (i in seq_along(beg))
    if (beg[i] > end[i]) stop("Sequences must have `start` > `end`")


  n_intervals <- length(beg)
  ordering <- order(beg)
  beg <- beg[ordering]
  end <- end[ordering]

  new_beg <- numeric(n_intervals)
  new_ends <- numeric(n_intervals)
  n_new <- 1
  cur_int <- c(beg[1], end[1])

  i <- 2
  while (i <= n_intervals) {
    comp_int <- c(beg[i], end[i])
    if (cur_int[2] >= comp_int[1]) { # do they overlap?
      cur_int <- c(cur_int[1], max(cur_int[2], comp_int[2])) # merge
    } else { # else store the current
      new_beg[n_new] <- cur_int[1]
      new_ends[n_new] <- cur_int[2]
      n_new <- n_new + 1
      cur_int <- comp_int
    }
    i <- i + 1
  }

  new_beg[n_new] <- cur_int[1]
  new_ends[n_new] <- cur_int[2]
  list(
    beg = new_beg[1:n_new],
    end = new_ends[1:n_new]
  )
}
