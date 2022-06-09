#' cleaner
#' @description Sets implied NA values with explicit ones, removes rows or columns containing all NA values, dedups, and converts the column names to a more workable syntax.
#' @param .data Data frame object to clean
#' @export
#' @examples
#' a <- c(1, NA, NA, 4)
#' b <- c(NA, NA, NA, NA)
#' c <- c(1, NA, 3, 4)
#' d <- tibble(a, b, `bad syntax ` = c)
cleaner <- function(.data,
                    .name_fix = T,
                    .nullify = T,
                    .rm_na = T,
                    .dedup = T) {
  nullify <- function(x) {
    x %>%
      str_replace_all(pattern = "\\b(NULL|null|na|Null)+", replacement = NA_character_) %>%
      str_replace_all(pattern = '^(\\s*)$',replacement = NA_character_)
  }

  rm_na <- function(.df) {
    .df %>%
      filter(rowSums(is.na(.)) != ncol(.)) %>%
      select(where(function(x) {
        sum(is.na(x)) / length(x) != 1
      }))
  }

  name_fix <- function(x) {
    x %>%
      str_to_lower() %>%
      str_replace_all(pattern = "[^\\w+|\\s]", replacement = "") %>%
      str_trim() %>%
      str_replace_all(pattern = "\\s+", replacement = "_")
  }
  if (.name_fix) {
    .data <- rename_with(.data, ~ name_fix(.))
  }
  if (.nullify) {
    .data <- .data %>%
      mutate(across(where(is.character), ~ nullify(.)))
  }

  if (.rm_na) {
    .data <- rm_na(.df = .data)
  }
  if (.dedup) {
    .data <- unique(x = .data)
  }
  return(.data)
}

