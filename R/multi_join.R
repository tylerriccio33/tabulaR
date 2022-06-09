#' multi_join
#' @description Use \code{dplyr::join} to join with additional parameters like case insensitivity and coalesce
#' @importFrom tidyr fill
#' @param df1 A data frame object
#' @param df2 A data frame to join
#' @param .by A string or character vector delimited with \code{=} as the key(s)
#' @param .fun Function to call
#' @param .soft Case insensitive function
#' @param .coalesce Coalesce like columns
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
multi_join <- function(df1,
                       df2,
                       .by = NULL,
                       .fun = dplyr::left_join,
                       .soft = T,
                       .coalesce = T) {
  empty_df1 <- df1 %>%
    slice(0)

  pre_join_df1 <- df1

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

  if (.soft) {
    vec_1 <- df1 %>%
      mutate(index = row_number()) %>%
      select(all_of(!!var), index)

    df2_low <- df2 %>%
      mutate(!!var := str_to_lower(!!var2))

    df1 <- df1 %>%
      mutate(!!var := str_to_lower(!!var2)) %>%
      mutate(index = row_number()) %>%
      match.fun(.fun)(df2_low, stored_by, na_matches = "never") %>%
      group_by(!!var2) %>%
      fill(index, .direction = "down") %>%
      ungroup() %>%
      select(-all_of(!!var)) %>%
      left_join(vec_1, by = "index") %>%
      select(-index)
  } else {
    df1 <- df1 %>%
      match.fun(.fun)(df2, stored_by)
  }


  if (.coalesce) {
    if (ncol(pre_join_df1) > ncol(df2)) {
      df1_cols <- colnames(pre_join_df1) %>%
        as_tibble_col("col1")
      df2_cols <- colnames(df2) %>%
        as_tibble_col("col2")

      for (i in rep(NA_character_, times = ncol(pre_join_df1) - ncol(df2))) {
        df2_cols <- df2_cols %>%
          rbind(rep(NA_character_, times = ncol(pre_join_df1) - ncol(df2)))
      }

      matches <- bind_cols(df1_cols, df2_cols)
    } else {
      df1_cols <- colnames(pre_join_df1) %>%
        as_tibble_col("col1")
      df2_cols <- colnames(df2) %>%
        as_tibble_col("col2")

      for (i in rep(NA_character_, times = ncol(df2) - ncol(pre_join_df1))) {
        df1_cols <- df1_cols %>%
          rbind(rep(NA_character_, times = ncol(df2) - ncol(pre_join_df1)))
      }
      matches <- bind_cols(df1_cols, df2_cols)
    }


    matches <- matches %>%
      mutate(
        col1 =
          str_extract(
            string = col1,
            pattern = "[^.]*"
          )
      ) %>%
      mutate(
        col2 =
          str_extract(
            string = col2,
            pattern = "[^.]*"
          )
      ) %>%
      mutate(col1 = ifelse(col1 %in% col2, col1, NA_character_)) %>%
      select(col1) %>%
      filter(col1 != stored_by) %>%
      fill(col1, .direction = "downup") %>%
      unique() %>%
      pull()

    for (i in matches) {
      var1 <- paste0(i, ".x")
      var1 <- ensym(var1)
      var2 <- paste0(i, ".y")
      var2 <- ensym(var2)
      i <- enquo(i)
      df1 <- df1 %>%
        mutate(!!i := coalesce(!!var1, !!var2)) %>%
        select(-!!var1) %>%
        select(-!!var2)
    }
  }

  df1 <- empty_df1 %>%
    bind_rows(df1)

  return(df1)
}
