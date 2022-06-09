#' match_cols
#' @description Coalesce columns into a single one while dropping the used columns in the same function.
#' @param df1 A dataframe object you want to reorder
#' @param df2 A data frame object containing the ideal order
#' @return A data frame object with the data from input one in the column order of input 2
#' @export
#' @examples
#' ideal_order <- tibble::tibble(col_b = NA, col_a = NA)
#' tibble::tibble(col_a = c(1, 2, 3, 4), col_b = c(5, 6, 7, 8)) %>%
#'   match_cols(ideal_order)
match_cols <- function(df1, df2) {
  empty <- df2 %>%
    slice(0)

  df2 <- empty %>%
    bind_rows(df1)

  return(df2)
}
