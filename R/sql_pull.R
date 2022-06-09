#' sql_pull
#' @description Pull, quote and copy column as vector to clipboard
#' @importFrom utils writeClipboard
#' @param .df A .df frame object
#' @param x Column to copy
#' @param na.rm Option to exclude NA values
#' @param quote Double or single quote to wrap around each observation
#' @param comma Option to add a comma after each observation
#' @return A .df frame object with the new column in the first position
#' @export
#' @examples
#' dplyr::starwars %>%
#'   sql_pull(homeworld)
sql_pull <-
  function(.df,
           x,
           na.rm = T,
           quote = "single",
           comma = T) {
    comma <- ifelse(comma, ",", "")

    if (quote == "single") {
      wrap_text <- function(y, quote) {
        paste0("'", y, "',")
      }
    } else if (quote == "double") {
      wrap_text <- function(y, quote) {
        paste0(dQuote(y, q = F), comma)
      }
    }

    if (!is_tibble(.df) || !is.data.frame(.df)) {
      stored_.df <- .df

      .df <- map_chr(.x = .df, .f = wrap_text)

      writeClipboard(.df)

      return(stored_.df)
    } else {
      x <- enquo(x)

      if (na.rm == T) {
        d <- .df %>%
          filter(!is.na(!!x)) %>%
          mutate(!!x := wrap_text(y = !!x)) %>%
          pull(var = !!x)
      } else {
        d <- .df %>%
          mutate(!!x := wrap_text(y = !!x)) %>%
          pull(var = !!x)
      }

      writeClipboard(d)

      return(.df)
    }
  }
