# Stat tests ----

#' quanti - categorical statistical testing
#'
#' @param exed variable to explain
#' @param exry explainatory variable
#'
#' @return a data frame containing descriptions and p-values
qc_test <- function(df, exed, exry, na.rm = FALSE) {
  exed <- dplyr::enquo(exed)
  exry <- dplyr::enquo(exry)

  if (na.rm) df <- dplyr::select(df, !!exed, !!exry) %>% tidyr::drop_na()

  n_lvls <- df %>%
    dplyr::select(!!exed, !!exry) %>%
    tidyr::drop_na() %>%
    dplyr::pull(!!exry) %>%
    factor() %>%
    levels() %>%
    length()
  # n_lvls <- length(levels(factor(pull(df, !!exry))))
  fmla <- as.formula(paste(rlang::as_name(exed), "~", rlang::as_name(exry)))

  if (n_lvls == 2) {
    param_p <- tryCatch(
      t.test(fmla, data = df)$p.value,
      error = function(x) NA
    )
    nparam_p <- wilcox.test(fmla, data = df, correct = T, exact = F)$p.value
  } else if (n_lvls > 2) {
    param_p <- summary(aov(fmla, data = df))[[1]][["Pr(>F)"]][1]
    nparam_p <- kruskal.test(fmla, data = df)$p.value
  } else {
    param_p <- NA
    nparam_p <- NA
  }

  out <-
    df %>%
    dplyr::group_by(!!exry) %>%
    dplyr::summarise(
      "n" = n(),
      "mean" = mean(!!exed, na.rm = T),
      "sd" = sd(!!exed, na.rm = T),
      "median" = quantile(!!exed, probs = 0.5, na.rm = T),
      "q1" = quantile(!!exed, probs = 0.25, na.rm = T),
      "q3" = quantile(!!exed, probs = 0.75, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      explained = rlang::as_name(exed),
      explainatory = rlang::as_name(exry),
      !!exry := as.character(!!exry),
      param_p = param_p,
      nparam_p = nparam_p
    )

  dplyr::select(out, explained, explainatory, lvl_exry = !!exry,
         n, mean, sd, param_p, median, q1, q3, nparam_p)
}


#' quanti - quanti statistical testing
#'
#' @inheritParams qc_test
#' @inherit qc_test return
#'
qq_test <- function(df, exed, exry, na.rm = FALSE) {
  exed <- dplyr::enquo(exed)
  exry <- dplyr::enquo(exry)

  df <- dplyr::select(df, !!exed, !!exry) %>% tidyr::drop_na()

  out <- tibble::tibble(
    explained = rlang::as_name(exed),
    explainatory = rlang::as_name(exry),
    pearson = cor(df, method = "pearson")[1,2],
    pearson_p = tryCatch(
      cor.test(dplyr::pull(df, !!exed), dplyr::pull(df, !!exry),
                    method = "pearson")$p.value,
      error = function(x) NA
    ),
    spearman = cor(df, method = "spearman")[1,2],
    spearman_p = tryCatch(
      cor.test(dplyr::pull(df, !!exed), dplyr::pull(df, !!exry),
               method = "spearman")$p.value,
      error = function(x) NA
    )
  )

  out
}


#' categorical - quanti statistical testing
#'
#' Reorder the parameters and send to qc_test
#'
#' @inheritParams qc_test
#' @inherit qc_test return
#'
cq_test <- function(df, exed, exry, na.rm = FALSE) {
  exed <- dplyr::enquo(exed)
  exry <- dplyr::enquo(exry)

  out <- qc_test(df, !!exry, !!exed, na.rm)

  dplyr::select(out, explained = explainatory, lvl_exed = lvl_exry,
         explainatory = explained, dplyr::everything())
}


#' categorical - categorical statistical testing
#'
#' @inheritParams qc_test
#' @inherit qc_test return
#'
cc_test <- function(df, exed, exry, na.rm = FALSE) {
  exed <- dplyr::enquo(exed)
  exry <- dplyr::enquo(exry)

  if (na.rm) df <- dplyr::select(df, !!exed, !!exry) %>% tidyr::drop_na()

  cont_table <- table(dplyr::pull(df, !!exed), dplyr::pull(df, !!exry))
  param_p <- tryCatch(
    chisq.test(cont_table)$p.value,
    error = function(msg) NA
  )
  nparam_p <- tryCatch(
    fisher.test(cont_table)$p.value,
    error = function(msg) NA
  )

  out <-
    df %>%
    dplyr::group_by(!!exed, !!exry) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate("prop" = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      explained = rlang::as_name(exed),
      explainatory = rlang::as_name(exry),
      !!exed := as.character(!!exed),
      !!exry := as.character(!!exry),
      param_p = param_p,
      nparam_p = nparam_p
    )

  dplyr::select(out,
         explained, lvl_exed = !!exed,
         explainatory, lvl_exry = !!exry,
         n, prop, param_p, nparam_p)
}

# formatters ----

#' @export
frmt_pvalue <- function(x) {
  out <- scales::pvalue(x, add_p = T, accuracy = 0.01)

  ifelse(
    x < 0.001, paste0(out, "***"),
    ifelse(
      x < 0.01,  paste0(out, "**"),
      ifelse(
        x < 0.05, paste0(out, "*"),
        out
      )
    )
  )
}

#' @export
frmt_mean_sd <- function(mn, st, accuracy = 0.1) {
  paste0(
    scales::number(mn, accuracy = accuracy),
    "\u00B1",
    scales::number(st, accuracy = accuracy)
  )
}

#' @export
frmt_median_iqr <- function(md, q1, q3, accuracy = 0.1) {
  paste0(
    scales::number(md, accuracy = accuracy),
    " [",
    scales::number(q1, accuracy = accuracy),
    "-",
    scales::number(q3, accuracy = accuracy),
    "]"
  )
}

#' @export
frmt_n_prop <- function(n, prop, accuracy = 0.1) {
  paste0(
    n, " (",
    scales::percent(prop, accuracy = accuracy), ")"
  )
}

# tables ----

kable_def_styles <- function(k) {
  kableExtra::kable_styling(
    k,
    bootstrap_options = c("striped"),
    fixed_thead = T
  )
}


kable_cq <- function(df) {

  exed <- unique(df$explained)

  df %>%
    dplyr::mutate(
      n = scales::number(n, accuracy = 1),
      mean = frmt_mean_sd(mean, sd),
      median = frmt_median_iqr(median, q1, q3)) %>%
    dplyr::mutate_at(
      dplyr::vars(param_p, nparam_p),
      frmt_pvalue
    ) %>%
    dplyr::select(-c(explained, sd, q1, q3)) %>%
    dplyr::select(explainatory, lvl_exed, dplyr::everything()) %>%
    kableExtra::kable(
      col.names = c(
        "", exed, "N", "Mean\u00B1SD", "p-value", "Median [IQR]", "p-value"
      ),
      align = c("c", "c", "c", "c", "l", "c", "l")
    ) %>%
    kableExtra::collapse_rows(columns = 1) %>%
    kableExtra::add_header_above(
      c(" " = 3, "Parametric" = 2, "Non Parametric" = 2)) %>%
    kable_def_styles()
}

kable_cc <- function(df) {

  exed <- unique(df$explained)

  df <- df %>%
    dplyr::mutate(
      n = frmt_n_prop(n, prop)
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(param_p, nparam_p),
      frmt_pvalue
    ) %>%
    dplyr::select(-c(explained, prop)) %>%
    dplyr::select(lvl_exed, explainatory, lvl_exry, dplyr::everything()) %>%
    tidyr::pivot_wider(names_from = lvl_exed, values_from = n) %>%
    dplyr::select(-c(param_p, nparam_p), dplyr::everything())

  lvls <- setdiff(colnames(df), c("explainatory", "lvl_exry", "param_p", "nparam_p"))
  grps <- c(2, length(lvls),  1, 1)
  names(grps) <- c(" ", exed, "Parametric", "Non Parametric")


  kableExtra::kable(
    df,
    col.names = c(
      "", "", lvls,  "p-value", "p-value"
    ),
    align = c("r", "l", rep("c", ncol(df) - 2))
  ) %>%
    kableExtra::collapse_rows(columns = 1) %>%
    kableExtra::add_header_above(grps) %>%
    kable_def_styles()
}

kable_qc <- function(df) {
  df %>%
    dplyr::mutate(
      n = scales::number(n, accuracy = 1),
      mean = frmt_mean_sd(mean, sd),
      median = frmt_median_iqr(median, q1, q3)) %>%
    dplyr::mutate_at(
      dplyr::vars(param_p, nparam_p),
      frmt_pvalue
    ) %>%
    dplyr::select(-c(explained, sd, q1, q3)) %>%
    dplyr::select(explainatory, lvl_exry, dplyr::everything()) %>%
    kableExtra::kable(
      col.names = c(
        "", "", "N", "Mean\u00B1SD", "p-value", "Median [IQR]", "p-value"
      ),
      align = c("r", "l", "c", "c", "l", "c", "l")
    ) %>%
    kableExtra::collapse_rows(columns = 1) %>%
    kableExtra::add_header_above(
      c(" " = 3, "Parametric" = 2, "Non Parametric" = 2)) %>%
    kable_def_styles()
}

kable_qq <- function(df) {
  df %>%
    dplyr::select(-explained) %>%
    dplyr::mutate_at(
      dplyr::vars(pearson, spearman),
      scales::number_format(accuracy = 0.01)
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(pearson_p, spearman_p),
      frmt_pvalue
    ) %>%
    kableExtra::kable(
      col.names = c(" ", rep(c("Correlation", "p-value"), 2)),
      align = c("r", rep(c("c", "l"), 2))
    ) %>%
    kableExtra::add_header_above(
      c(" " = 1, "Pearson" = 2, "Spearman" = 2)) %>%
    kable_def_styles()
}


fmt_pvalue <- function(data, columns, rows = NULL) {

  # Capture expression in `rows`
  rows <- rlang::enquo(rows)

  # Pass `data`, `columns`, `rows`, and the formatting
  # functions as a function list to `fmt()`
  gt::fmt(
    data = data,
    columns = columns,
    rows = !!rows,
    fns = list(
      default = function(x) {

        x_str <-
          dplyr::case_when(
            x < 0.01 ~ "<0.01***",
            dplyr::between(round(x, 2), 0, 0.01) ~ paste0(round(x, 2), "**"),
            dplyr::between(round(x, 2), 0, 0.05) ~ paste0(round(x, 2), "*"),
            TRUE ~ as.character(round(x, 2))
          )
      }
    )
  )
}

# high levels


quant_quali <- function(x) {
  if (any(class(x) %in% c("numeric")))
    type <- "quant"
  else if (any(class(x) %in% c("character", "factor", "logical")))
    type <- "categ"
  else
    type <- "unknown"

  type
}

explain_test <- function(df, exed, na.rm = FALSE) {
  exed <- dplyr::enquo(exed)

  exed_type <- quant_quali(dplyr::pull(df, !!exed))

  if (exed_type == "quant") {
    quant_test <- qq_test
    categ_test <- qc_test
  } else if (exed_type == "categ") {
    quant_test <- cq_test
    categ_test <- cc_test
  } else {
    stop(glue::glue("Type of column {rlang::as_name(exed)} not supported" ))
  }

  cols_types <-  vapply(dplyr::select(df, - !!exed), quant_quali, "")
  cols_types <- cols_types[cols_types != "unknown"]

  quant <- purrr::map_dfr(
    names(cols_types[cols_types == "quant"]),
    function(x) quant_test(df, !!exed, !!dplyr::sym(x), na.rm = na.rm)
  )

  categ <- purrr::map_dfr(
    names(cols_types[cols_types == "categ"]),
    function(x) categ_test(df, !!exed, !!dplyr::sym(x), na.rm = na.rm)
  )

  list("quant" = quant, "categ" = categ, "exed_type" = exed_type)
}

#' explain
#'
#' @export
explain_rmd <- function(df, exed, msg = "", na.rm = FALSE) {
  exed <- rlang::enquo(exed)

  expl <- explain_test(df, !!exed, na.rm)

  cat(glue::glue(
    "
    ## Univariate explaining of variable `{rlang::as_label(exed)}` {msg}
    \n\n
    ")
  )


  if (expl$exed_type == "quant") {
    if (nrow(expl$quant) > 0) {
      cat(glue::glue(
        "
      ### Correlation of `{rlang::as_label(exed)}` \\
       with other quantitative variables
      \n\n
      "
      ))
      kable_qq(expl$quant) %>%
        cat()
      cat("\n\n")
    }

    if (nrow(expl$categ) > 0) {
      cat(glue::glue(
        "
      ### Value of `{rlang::as_label(exed)}` \\
      depending on other categorical variables
      \n\n
      "
      ))
      kable_qc(expl$categ) %>%
        cat()
      cat("\n\n")
    }
  }
  else if (expl$exed_type == "categ") {
    if (nrow(expl$quant) > 0) {
      cat(glue::glue(
        "
      ### Values of other quantitative variables \\
      depending on the level of `{rlang::as_label(exed)}`
      \n\n
      "
      ))
      kable_cq(expl$quant) %>%
        cat()
      cat("\n\n")
    }

    if (nrow(expl$categ) > 0) {
      cat(glue::glue(
        "
      ### Proportions of `{rlang::as_label(exed)}` \\
      depending on other categorical variables
      \n\n
      "
      ))
      kable_cc(expl$categ) %>%
        cat()
      cat("\n\n")
    }
  }
  else stop("unable to produce the explanation")
}
