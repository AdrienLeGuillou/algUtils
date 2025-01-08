pre_reg <- function(df, fmla) {
  dat <- model.frame(fmla, df)
  n_NA <- nrow(df) - nrow(dat)

  message(n_NA, " values removed due to missingness")

  dat
}

kable_reg <- function(df) {
    knitr::kable(df, format = "html") %>%
    kableExtra::kable_styling(bootstrap_options = "striped")
}

#' @export
alg_reg_logi <- function(df, fmla, accuracy = 0.001) {
  dat <- pre_reg(df, fmla)
  mod <- glm(family = binomial, fmla, data = dat)
  frmt_est <- scales::label_number(accuracy)

  mod %>%
    broom::tidy() %>%
    dplyr::mutate(
      IC_low = frmt_est(exp(estimate - 1.96 * std.error)),
      IC_hig = frmt_est(exp(estimate + 1.96 * std.error)),
      OR = frmt_est(exp(estimate)),
      OR_IC95 = glue::glue("{OR} [{IC_low}, {IC_hig}]"),
      p.value = frmt_pvalue(p.value)
    ) %>%
    dplyr::select(term, OR_IC95, p.value) %>%
    dplyr::filter(term != "(Intercept)") %>%
    kable_reg()
}

#' @export
alg_reg_lm <- function(df, fmla, accuracy = 0.001) {
  dat <- pre_reg(df, fmla)
  mod <- lm(fmla, data = dat)
  frmt_est <- scales::label_number(accuracy)

  mod %>%
    broom::tidy() %>%
    dplyr::mutate(
      IC_low = frmt_est(estimate - 1.96 * std.error),
      IC_hig = frmt_est(estimate + 1.96 * std.error),
      estimate = frmt_est(estimate),
      estimate_IC95 = glue::glue("{estimate} [{IC_low}, {IC_hig}]"),
      p.value = frmt_pvalue(p.value)
    ) %>%
    dplyr::select(term, estimate_IC95, p.value) %>%
    dplyr::filter(term != "(Intercept)") %>%
    kable_reg()
}

#' @export
alg_reg_auto <- function(df, fmla, accuracy = 0.001) {
  y <- as.character(fmla[[2]])

  if (is.numeric(df[[y]])) {
    alg_reg_lm(df, fmla, accuracy)
  } else {
    alg_reg_logi(df, fmla, accuracy)
  }
}
