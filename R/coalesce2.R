#' coalesce2
#' @description Coalesce columns into a single one while dropping the used columns in the same function.
#' @param .df Dataframe object
#' @param into Column to coalesce into
#' @param ... Columns to coalesce
#' @param .suffix Coalesce columns with a common suffix
#' @return A dataframe without the coalesced columns
#' @export
#' @examples
#' a <- c(1, 2, NA, NA)
#' b <- c(NA, NA, 3, 4)
#' c <- tibble::tibble(a, b)
#' coalesce2(.df = c, into = new_col, a, b)
coalesce2 <- function(.df, into = NULL, ... = NULL, .suffix = F) {
  select2 <- function(.data, cols) {
    expr <- rlang::enquo(cols)
    pos <- eval_select(expr, data = .data)
    rlang::set_names(.data[pos], names(pos))
  }

  if (.suffix) {
    .into <- as.character(substitute(into))
    x1 <- colnames(.df) %>%
      as_tibble_col("col1") %>%
      mutate(col2 = str_extract(
        string = col1,
        pattern = "[^.]*"
      )) %>%
      group_by(col2) %>%
      filter(n() > 1) %>%
      ungroup() %>%
      filter(col2 == .into) %>%
      select(col1) %>%
      pull()

    into <- enquo(into)

    x1 <- syms(x1)

    .df <- .df

    .df <- .df %>%
      mutate(!!into := coalesce(!!!x1)) %>%
      select2(cols = !c(...))

    return(.df)
  }

  x <- enquos(...)

  into <- enquo(into)

  .df <- .df

  .df <- .df %>%
    mutate(!!into := coalesce(!!!x)) %>%
    select2(cols = !c(...))


  return(.df)
}
