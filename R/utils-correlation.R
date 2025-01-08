make_one_hot <- function(orig_var, name) {

  df <- tibble::tibble(orig_var)
  colnames(df) <- name

  df |>
    mutate(i = row_number(), y = T) |>
    spread(!! quo(name), y, fill = F, sep = "_") |>
    select(-i)
}

make_one_hot_lst <- function(df, orig_vars) {
  orig_vars <- enquos(orig_vars)

  df_one <- df |>
    select(!!! orig_vars)

  df_one <- purrr::map2_dfc(
    df_one, colnames(df_one),
    function(x, y) make_one_hot(x, y)
  )

  df |>
    select(-c(!!! orig_vars)) |>
    bind_cols(df_one)
}

make_one_hot_df <- function(df) {

  is_char_fac_lo <- function(x) {
    is.character(x) | is.factor(x) | is.logical(x)
  }

  orig_vars <- df |> select_if(is_char_fac_lo) |> colnames()

  df_one <- df |>
    select(!!! quos(orig_vars))

  df_one <- purrr::map2_dfc(
    df_one, orig_vars,
    function(x, y) make_one_hot(x, y)
  )

  df |>
    select(-c(!!! quos(orig_vars))) |>
    bind_cols(df_one)
}

by_freq_factor <- function(x) {
  x <- factor(x)
  factor(x, levels = names(sort(table(x), decreasing = T)))
}

is.factorable <- function(x) {
  is.character(x) | is.factor(x) | is.logical(x)
}
