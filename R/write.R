#' write
#' @description Function that writes a data frame object to the wd
#' @param x A data frame object
#' @param path Location to write the file to
#' @param type Type of file
#' @return A data frame object with the new column in the first position
#' @export
#' @examples
#' .x <- tibble::tibble(a = NA, b = NA)
#' \dontrun{
#' write(.x)
#' }
write <- function(x, path = wd, type = "csv") {
  .fun <- case_when(
    type == "csv" ~ "write_csv",
    type == "xlsx" ~ "write_xlsx",
    type == "tsv" ~ "write_tsv"
  )

  y <- as_label(rlang::enquo(x))

  match.fun(FUN = .fun)(x, paste0(path, "/", y, ".", type), na = "")
}
