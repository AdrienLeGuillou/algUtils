`|>` <- dplyr::`|>`

kable_qc <- function(df) {
  df |>
    dplyr::mutate(
      n = scales::number(n, accuracy = 1),
      mean = frmt_mean_sd(mean, sd),
      median = frmt_median_iqr(median, q1, q3)) |>
    dplyr::mutate_at(
      dplyr::vars(param_p, nparam_p),
      frmt_pvalue
    ) |>
    dplyr::select(-c(explained, sd, q1, q3)) |>
    dplyr::select(explainatory, lvl_exry, dplyr::everything()) |>
    kableExtra::kable(
      col.names = c(
        "", "", "N", "Mean\u00B1SD", "p-value", "Median [IQR]", "p-value"
      ),
      align = c("r", "l", "c", "c", "l", "c", "l")
    ) |>
    kableExtra::collapse_rows(columns = 1) |>
    kableExtra::add_header_above(
      c(" " = 3, "Parametric" = 2, "Non Parametric" = 2)) |>
    kable_def_styles()
}


data("infert")

infert$case <- as.logical(infert$case)
infert$induced <- as.character(infert$induced)

et <- explain_test(infert, parity)

et$cat   |>
  kable_qc()




