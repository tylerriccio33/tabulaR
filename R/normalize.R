#' normalize
#' @description Handles skew by square route and
#' @param .data Data frame object
#' @param ... Variables to normalize using tidyselect syntax
#' @export
#' @examples
#' dplyr::starwars %>% normalize(where(is.numeric))

normalize <- function(.data, ...){

  rightSkew <- .data %>%
    select(...) %>%
    summarise(across(everything(), ~ skewness(.x))) %>%
    pivot_longer(cols = everything(), names_to = 'var') %>%
    filter(value > 1) %>%
    pull(var)

  leftSkew <- .data %>%
    select(...) %>%
    summarise(across(everything(), ~ skewness(.x))) %>%
    pivot_longer(cols = everything(), names_to = 'var') %>%
    filter(value < 1) %>%
    pull(var)

  # Manipulating data beforehand
  .data <- .data %>%
    mutate(across(c(...), ~ rescale(.x)))

  export <- recipe(~ ., data = .data %>% select(...)) %>%
    # For negative skew
    step_mutate(across(all_of(leftSkew), ~ .x ^ 2)) %>%
    # For positive skew
    step_log(all_of(rightSkew),offset = 1) %>%
    # For centering and scaling
    step_normalize(...) %>%
    prep() %>%
    bake(.data) %>%
    mutate(across(c(...), ~ rescale(.x)))

    return(export)

}
