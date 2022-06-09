#' transmutate
#' @description Mutate columns while dropping the used ones
#' @param .df A data frame object you want to reorder
#' @param ... Parameters to pass to \code{dplyr::mutate}
#' @return A data frame object with the new column in the first position
#' @export
#' @examples
#' dplyr::starwars %>%
#'   transmutate(new_col = height + mass)
transmutate <- function(.df, ...) {
  x <- enquos(...)

  .df %>%
    mutate(!!!x, .keep = "unused")

}





