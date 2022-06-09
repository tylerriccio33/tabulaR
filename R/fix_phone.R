#' fix_phone
#' @description Formats phone number column or columns
#' @importFrom purrr map_chr
#' @param .df Data frame like object
#' @param ... Column(s) or vector of strings
#' @param strip Number of digits to strip down
#' @param .format Optional argument specifying the format
#' @export
#' @examples
#' p <- c("111-111-1111", "2-(222)-222-2222")
#' p2 <- c("111-111-1111", "2-222-222-2222")
#' p3 <- tibble::tibble(p, p2)
#' p3 %>% fix_phone(p2, .format = "US")
fix_phone <- function(.df,
                      ...,
                      strip = 11,
                      .format = NULL) {
  stripped <- function(b) {
    x <- str_replace_all(
      string = b,
      pattern = "([^\\d])",
      replacement = ""
    )
    str_sub(
      string = x,
      start = -strip,
      end = -1
    )
  }

  .df <- .df

  is_.df <- (is_tibble(.df) || is.data.frame(.df))

  if (is_.df) {
    x <- enquos(...)
  } else {
    x <- .df
  }

  if (!is.null(strip)) {
    if (is_.df) {
      .df <- .df %>%
        mutate(across(c(...), ~ stripped(b = .x)))
    } else {
      .df <- map_chr(x, stripped)
    }
  }

  strip <- case_when(
    .format == "us" ~ 11,
    .format == "domestic" ~ 10,
    TRUE ~ strip
  )

  y <- function(a) {
    x <- stripped(b = a)

    case_when(
      nchar(x) == 10 ~
        paste0(
          str_sub(x, 1, 3),
          "-",
          str_sub(x, 4, 6),
          "-",
          str_sub(x, 7, nchar(x))
        ),
      nchar(x) == 11 ~
        paste0(
          str_sub(x, 1, 1),
          "-",
          str_sub(x, 2, 4),
          "-",
          str_sub(x, 5, 7),
          "-",
          str_sub(x, 8, nchar(x))
        ),
      nchar(x) == 7 ~
        paste0(
          str_sub(x, 1, 3),
          "-",
          str_sub(x, 4, 7)
        )
    )
  }

  if (!is.null(.format)) {
    if (is_.df) {
      .df <- .df %>%
        mutate(across(c(...), ~ y(a = .x)))
    } else {
      .df <- map_chr(x, y)
    }
  }

  return(.df)
}
