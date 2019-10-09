#' Check if a vector can be coerced to logical without information loss
#'
#' @param x a vector
#'
#' @return TRUE if x can be losslessly coerced to logical
#' @export
alg_is_char_logical <- function(x) {
  lgl_values <- c("TRUE", "FALSE", "true", "false", "0", "1", NA)

  setequal( union(unique(x), lgl_values), lgl_values )
}

#' Coerce to logical a character vector which is "char_logical"
#'
#' @param x a vector
#'
#' @return a logical vector
#' @export
alg_as_logical_char_logical <- function(x) {
  x <- gsub("1", "TRUE", x)
  x <- gsub("0", "FALSE", x)

  as.logical(x)
}

#' Transform a character string into a clean name
#'
#' A clean name is defined here as a valid R name and:
#' * only ASCII characters
#' * underscore to separate words instead of dots
#' * only lowercase
#'
#' @param x the original character vector
#'
#' @return a vector of clean names
#' @export
alg_make_clean_names <- function(x) {

  name_cleaner <- function(x) {
    x <- make.names(x)
    x <- stringi::stri_trans_general(x, "ASCII")
    x <- gsub("\\.", "_", x)
    tolower(x)
  }

  x[!is.na(x)] <- name_cleaner(x[!is.na(x)])

  x
}

#' Clean all columns with a value map
#'
#' @param df the data frame to modify
#' @param value_maps the list of mapped values
#'
#' @return the df were columns were modified
alg_map_col_values <- function(df, value_maps) {

  # store the number of missing values of each "value map" column
  miss_pre <- vapply(df[names(value_maps)], function(x) sum(is.na(x)), 0)

  # do mapping all columns with a value map
  df[names(value_maps)] <- lapply(
    names(value_maps),
    function(n) {
      # convert NA in value maps to "__NA__" for the NA count
      vm <- value_maps[[n]]
      vm[is.na(vm)] <- "__NA__"
      # map the values
      vm[ df[[n]] ]
    }
  )

  # Check if more NA's were created
  miss_post <- vapply(df[names(value_maps)], function(x) sum(is.na(x)), 0)
  assertthat::assert_that(
    all(miss_pre == miss_post),
    msg = glue::glue(
      "New missing values created while mapping values with values maps.
      Problems with maps : {names(miss_pre)[(miss_pre != miss_post)]}"
    )
  )

  # replace "__NA__" with NA
  df <- dplyr::mutate_if(df, is.character, function(x) {
    x[x == "__NA__"] <- NA
    x
  })

  df
}
