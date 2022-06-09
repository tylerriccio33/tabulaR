#' dupe
#' @description Adds a column that indicates the input value exists somewhere else in the same column
#' @param .df Data frame like object
#' @param ... Column or combination of columns to check
#' @export
#' @examples
#' a <- c(1, 2, 3, 3)
#' b <- c(1, 2, 3, 4)
#' c <- tibble::tibble(a, b)
#' c %>% dupe(a)
dupe <-
  function(.df, ...) {
    grouping <- quos(...)

    .df %>%
      group_by(!!!grouping) %>%
      mutate(dupe = n() > 1) %>%
      ungroup()
  }
