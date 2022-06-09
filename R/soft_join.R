#' soft_join
#' @description Case insensitive join
#' @param df1 A data frame object
#' @param df2 A data frame to join
#' @param .by A string or character vector delimited with \code{=} as the key(s)
#' @param .fun Function to call
#' @param .keep Option to keep the joined columns
#' @return A data frame object 1 joined with data frame object 2
#' @export
#' @examples
#' a <- starwars %>%
#'   select(1:5)
#' b <- starwars %>%
#'   select(1:5) %>%
#'   mutate(name = str_to_upper(name)) %>%
#'   mutate(new_column = "hello")
#'
#' t <- a %>%
#'   multi_join(b, .by = "name")
soft_join <- function(df1,
                      df2,
                      .by,
                      .fun = dplyr::left_join,
                      .keep = F) {
  stored_by <- .by

  .by <- ifelse(is.null(.by %>% names()), .by, (paste0(names(.by), ",", .by))) %>%
    str_split(",", n = 2) %>%
    unlist()

  if (length(.by) == 1) {
    var <- enquo(stored_by)

    var2 <- sym(stored_by)
  } else {
    first_var <- .by[1]
    second_var <- .by[2]

    second_var <- enquo(second_var)

    stored_by <- .by[1]

    var <- enquo(first_var)

    var2 <- sym(first_var)

    df2 <- df2 %>% rename(!!var := !!second_var)
  }

  vec_1 <- df1 %>%
    mutate(index = row_number()) %>%
    select(all_of(!!var), index)

  df2_low <- df2 %>%
    mutate(!!var := str_to_lower(!!var2))

  empty_df1 <- df1 %>%
    slice(0)

  df1 <- df1 %>%
    mutate(!!var := str_to_lower(!!var2)) %>%
    mutate(index = row_number()) %>%
    match.fun(.fun)(df2_low, stored_by) %>%
    group_by(!!var2) %>%
    fill(index, .direction = "down") %>%
    ungroup() %>%
    select(-all_of(!!var)) %>%
    left_join(vec_1, by = "index") %>%
    select(-index)



  if (.keep == T) {
    vec_2 <- df2 %>%
      select(!!var) %>%
      pull() %>%
      as_tibble_col("vec_2")

    df1 <- df1 %>%
      bind_cols(vec_2)
  }

  df1 <- empty_df1 %>%
    bind_rows(df1)
}
