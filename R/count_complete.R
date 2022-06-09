#' count_complete
#' @description Count the columns per row that contain .df
#' @param .df .df frame like object
#' @param ... Columns to count
#' @param boolean Option to count Boolean fields with a \code{FALSE} value as NA
#' @param name Option to name the new variable
#' @export
#' @examples
#'\dontrun{
#' mpg %>%
#' group_by(model) %>%
#'  mutate(dummy = floor(runif(1, min=min(cyl), max=max(cyl))),
#'         cyl = ifelse(cyl == dummy, NA_real_, cyl)) %>%
#'  ungroup() %>%
#'  select(-dummy) %>%
#' count_complete(starts_with('c'), name = 'hello')
#' }
count_complete <- function(.df,
                           ...,
                           boolean = F,
                           name = 'completeFields') {
  # Need to convert ... to enquos in order to count the length
  x <- enquos(...)

  # Checks whether to count FALSE as complete
  if (boolean) {
    evaluated <- .df %>%
      select(...) %>%
      transmute(name :=  rowSums(across(everything(), ~ .x == T)))
  }
  # If Boolean is FALSE (default) then don't bother with FALSE logical variables
  else {
    # If ... is it'll catch NA's across everything
    if (length(x) == 0) {
      evaluated <- .df %>%
        transmute({{name}} := rowSums(!is.na(.)))
    } else {
      # If ... isn't empty it should become tidyselect syntax
      evaluated <- .df %>%
        select(...) %>%
        transmute({{name}} := rowSums(!is.na(.)))
    }
  }

  .df <- .df %>%
    bind_cols(evaluated)

  return(.df)
}
