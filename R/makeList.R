#' makeList
#' @description Vectorized function that turns a string into a delimited list.
#' @param data A data frame object
#' @param x Variable
#' @param sep String delimeter if applicable
#' @return A tibble with the supplied variable in list format
#' @export
#' @examples
#' \dontrun{
#' mpg %>%
#' mutate(manufacturer = paste0(manufacturer, ', something')) %>%
#' makeList(manufacturer, sep = ',')
#' }
makeList <- function(data, x, sep = ' | ') {

  varName <- rlang::as_label(enquo(x))

  vector <- c()

  for (i in data[[varName]]) {
    trimmedSep <- str_trim(sep)
    newVal <- str_split(i, sep) %>%
      unlist() %>%
      keep(~ .x != trimmedSep) %>% list()
    vector <- c(vector, newVal)
  }

  return(data %>%
           select(-{{x}}) %>%
           bind_cols(tibble("{varName}" := vector)) %>%
           relocate({{x}}))

}

