#' uid
#' @description Generate unique IDs
#' @param .df A data frame object
#' @param times Number of times you've called the function so that the seed can be reset
#' @param n Length of vector
#' @param len length of ID generated
#' @param prefix Prefix to add to ID string
#' @param suffix Suffix to add to ID string
#' @return A vector or column of unique IDs
#' @export
#' @examples
#' tibble::tibble(a = c(1, 2, 3, 4)) %>%
#'   uid()
uid <- function(.df = NULL, times = 1, n = NULL, len = 12, prefix = NULL, suffix = NULL) {
  set.seed(times)

  pool <- c(letters, LETTERS, 0:9)

  stored_n <- n

  if (!is.null(n)) {
    n <- n
  } else if (is.null(nrow(.df))) {
    n <- length(.df)
  } else {
    n <- nrow(.df)
  }

  res <- n # pre-allocating vector is much faster than growing it

  for (i in seq(n)) {
    this_res <- paste0(sample(pool, len, replace = TRUE), collapse = "")
    while (this_res %in% res) {
      # if there was a duplicate, redo
      this_res <-
        paste0(sample(pool, len, replace = TRUE), collapse = "")
    }
    res[i] <- this_res
  }
  if (!is.null(stored_n)) {
    return(res)
  }

  if (!(is_tibble(.df) || is.data.frame(.df))) {
    uid <- res
    .df <- cbind(.df, uid)
  } else {
    res <- res %>% as_tibble_col()
    .df <- .df %>%
      bind_cols(res) %>%
      rename(uid = ncol(.))
  }

  if (!is.null(prefix) || !is.null(suffix)) {
    .df <- .df %>%
      mutate(uid = paste0(ifelse(!is.na(prefix), paste0(prefix, "_"), ""), uid, ifelse(!is.na(suffix),
        paste0("_", suffix), ""
      )))
  }

  return(.df)
}
