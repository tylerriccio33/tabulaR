#' winning_record
#' @description Arranges and chooses the first row of a group based on a sorting parameter
#' @param .df A data frame object you want to reorder
#' @param group Grouping parameter
#' @param arr Column to use as the sorting parameter
#' @param by Sorting function
#' @return A data frame object with a single row per group
#' @export
#' @examples
#' mpg %>% winning_record(group = model, arr = cyl)
winning_record <- function(.df,
                           group,
                           arr = NULL,
                           by = desc) {
  x <- missing(arr)
  if(x){
      .df %>%
      group_by({{group}}) %>%
      slice(1)%>%
      ungroup()
     return(.df)
  } else {
    .df %>%
      group_by({{group}}) %>%
      arrange(by({{arr}})) %>%
      slice(1) %>%
      ungroup()
  }
}
