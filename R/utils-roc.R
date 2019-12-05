#' Extend the ggroc function to add the Youden best cutoff
#'
#' @param roc_obj
#'
#' @return a ggplot graph
#'
#' @export
ggroc_youden <- function(roc_obj) {

  youden <- youden_j_max(roc_obj)

  ggroc(roc_obj) +
    geom_vline(
      xintercept = youden["specificity"],
      color = "grey",
      linetype = "dashed"
    ) +
    geom_hline(
      yintercept = youden["sensitivity"],
      color = "grey",
      linetype = "dashed"
    ) +
    geom_text(
      aes(
        x = youden["specificity"] - .05,
        y = youden["sensitivity"] - .05,
        label = paste0(
          "Spe: ", scales::number(youden["specificity"], accuracy = .01), ", ",
          "Sen: ", scales::number(youden["sensitivity"], accuracy = .01), ", ",
          "Cutoff: ", scales::number(youden["cutoff"], accuracy = .01)
        )),
      hjust = "left"
    )
}

#' Calculate the maximum Youden J index
#'
#' Calculate the Youden J index, outputs its value and the specificity and sensitivity corresponding
#' @param roc_obj A roc object from pROC::roc
#'
#' @return a vector containing the max Youden index and the specificity and sensitivity corresponding
#'
#' @export
youden_j_max <- function(roc_obj) {
  youden_j <- roc_obj$sensitivities + roc_obj$specificities - 1
  youden_max <- which.max(youden_j)

  c(
    max = youden_max,
    specificity = roc_obj$specificities[youden_max],
    sensitivity = roc_obj$sensitivities[youden_max],
    cutoff = roc_obj$thresholds[youden_max]
  )
}
