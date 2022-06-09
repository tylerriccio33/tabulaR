#' in_file
#' @description Uploads, parses, cleans and assigns in one function
#' @importFrom purrr map
#' @param x String or vector of file names
#' @param clean Cleans names, removes empty rows, removes constant rows and dedups
#' @param choose \code{file.choose()} option
#' @param .assign Option to manually declare the variable in the form of a list of tibbles
#' @export
#' @examples
#' \dontrun{
#' in_file(c("example_filename", "filename2"))
#' }
in_file <- function(x = NULL,
                    clean = T,
                    choose = F,
                    .assign = F) {

  ## global variables

  # global functions --------------------------------------------------------

  master_helper <- function(.x) {
    file_type <- str_match(
      string = list.files(getwd()),
      pattern = "(.+?)(\\.[^.]*$|$)"
    ) %>%
      as_tibble() %>%
      filter(V2 == .x) %>%
      pull(V3)

    fun <- case_when(
      file_type == ".txt" ~ as.character("read_tsv"),
      file_type == ".csv" ~ as.character("read_csv"),
      file_type == ".xls" ~ as.character("read_xls"),
      file_type == ".xlsx" ~ as.character("read_xlsx")
    )

    val <- match.fun(fun)(paste0(
      getwd(),
      "/",
      sym(.x),
      file_type
    ))

    if (clean) {
      val <- val %>% cleaner()
    }

    return(val)
  }

  get_file_type <- function(.x) {
    str_match(
      string = list.files(getwd()),
      pattern = "(.+?)(\\.[^.]*$|$)"
    ) %>%
      as_tibble() %>%
      filter(V2 == .x) %>%
      pull(V3)
  }

  fun_controller <- function(.file_type) {
    case_when(
      .file_type == ".txt" ~ as.character("read_tsv"),
      .file_type == ".csv" ~ as.character("read_csv"),
      .file_type == ".xls" ~ as.character("read_xls"),
      .file_type == ".xlsx" ~ as.character("read_xlsx")
    )
  }

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

  .value <- function(.i, .fun, .file_type, .clean) {
    val <- match.fun(.fun)(paste0(
      getwd(),
      "/",
      sym(.i),
      .file_type
    ))
    if (.clean) {
      val <- val %>% cleaner()
    }
    return(val)
  }

  namer <- function(.x) {
    .x %>%
      str_to_lower() %>%
      str_trim() %>%
      str_replace_all(pattern = "\\s", replacement = "_")
  }

  # loops -------------------------------------------------------------------

  if (choose) {
    x <- str_match(string = file.choose(), pattern = "[^\\\\{2}]+(?=\\.)")

    x <- x %>%
      as.list() %>%
      set_names(namer)

    nested_vals <- map(x, master_helper)
  } else {
    x <- x %>%
      as.list() %>%
      set_names(namer)

    nested_vals <- map(x, master_helper)
  }

  if (.assign) {
    # If it's being assigned and length > 1, it'll hold the tibbles in a list
    # If it's being assigned and length == 1, it'll return the tibble
    if(length(nested_vals) > 1){
      return(nested_vals)
    } else {
return(nested_vals[[1]])
    }
  } else {
    list2env(nested_vals, envir = .GlobalEnv)
  }
}












