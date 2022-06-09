#' col_check
#' @param .df A data frame object
#' @param ... Columns to check
#' @return A tibble with the percent of NA values in each column
#' @export
#' @examples
#' a <- c(1, 2, NA, NA)
#' b <- c(NA, NA, NA, 4)
#' c <- tibble(a, b)
#' c %>% col_check()
col_check <- function(.df, ...) {
  if (nargs() != 2) {
    if (nargs() == 1) {
      x <- colnames(.df)
      na_row <- .df %>%
        summarise(across(everything(), ~ mean(is.na(.))))
    } else {
      .df <- .df %>%
        select(...)

      na_row <- .df %>%
        summarise(across(everything(), ~ mean(is.na(.))))
    }
  } else {
    na_row <- .df %>%
      select(...) %>%
      summarise(across(everything(), ~ mean(is.na(.))))
  }

  final <- na_row %>%
    mutate(across(everything(), ~ as.character(.x))) %>%
    bind_rows(.df %>%  mutate(across(everything(), ~ as.character(.x)))) %>%
    slice(1)

  return(final)
}
