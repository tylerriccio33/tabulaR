#' listJoin
#' @description Left join function that takes a string and searches for a match within a list.
#' @param x Left data frame object
#' @param y Right data frame object
#' @param .by Joining column
#' @return Joined table abiding by left join principles.
#' @export
#' @examples
#' \dontrun{
#' firstList <- starwars %>%
#'  select(films) %>%
#'  mutate(films = map(films, ~ .[1]) %>% unlist())
#'
#'firstList %>%
#'  listJoin(starwars %>%
#'             mutate(rowN = row_number())%>%
#'             select(films,rowN) , .by = 'films') %>% view()
#' }
listJoin <- function(x, y, .by) {

  leftVals <- x %>%
    pull(.by)

  rightVals <- y %>%
    pull(.by)

  idTibble <- tibble(id1 = NA_real_,
                     id2 = NA_real_)

  for (i in seq(1:length(leftVals))) {

    # Left Table can't be a list

    searchValue <- leftVals[i]


    for (l in seq(1:length(rightVals))) {
      evaluationVector <- rightVals[l] %>% unlist()
      if (searchValue %in% evaluationVector) {
        # Yes there's a match
        match <- l
        break
        # After it finds a match, break
      } else {
        match <- NA_real_
      }
    }

    idTibble <- idTibble %>%
      add_row(tibble(id1 = i,
                     id2 = match))

  }

  x %>%
    mutate(id1 = row_number()) %>%
    power_left_join(idTibble,
                    by = 'id1',
                    na_matches = 'never',
                    conflict = coalesce_xy) %>%
    power_left_join(y %>% mutate(id2 = row_number()),
                    by = 'id2',
                    na_matches = 'never') %>%
    select(-id1, -id2)
}
