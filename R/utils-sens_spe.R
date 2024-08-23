#' Make of formatted table of sens, spe, vpp, vpn
#'
#' @param e logical vector of the test result (test to eval)
#' @param m logical vector of the disease (ground truth)
#' @param prec the default precision for `label_percent`
#'
#' @return a formatted tibble
#' @export
senspe_fn <- function(e, m, prec = 0.1) {
  res <- dplyr::case_when(
    e & m ~ "TP",
    e & !m ~ "FP",
    !e & m ~ "FN",
    !e & !m ~ "TN",
    TRUE ~ NA
  )

  TP <- sum(res == "TP")
  FP <- sum(res == "FP")
  TN <- sum(res == "TN")
  FN <- sum(res == "FN")

  tibble::tibble(
    sens = ci_prop(TP, (TP + FN), prec),
    spe = ci_prop(TN, (TN + FP), prec),
    vpp = ci_prop(TP, (TP + FP), prec),
    vpn = ci_prop(TN, (TN + FN), prec)
  )
}

#' Calculate a proportion with CI95
#'
#' @param ts number of truth
#' @param n number of observation
#' @param prec the default precision for `label_percent`
#'
#' @return a formatted string of p% [CI95]
#' @export
ci_prop <- function(ts, n, prec) {
  fmtr <- scales::label_percent(prec)
  p <- ts / n
  zse <- 1.96 * sqrt(p * (1 - p) / n)

  paste0(fmtr(p), " [", fmtr(p - zse), " ,",  fmtr(p + zse), "]")
}
