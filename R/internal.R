#' "Mixed Case" Capitalizing - toupper( every first letter of a word )
#' @description A function that turns every first letter to upper case
#' @details Slightly modified function from R Documentation "Character
#' Translation and Casefolding". Turns first character in a list of names to
#' Upper Case, and other letters to lower case. Needs lapply for lists.
#'
#' @examples
#' # Turku
#' .simpleCap("turku")
#' # Kaarina
#' .simpleCap("kAaRiNa")
#'
#' @noRd
#' @keywords internal
#' @export
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
}


#' Check if connection is available
#' @description A simple function to check if connection is available
#' @details To avoid repetition in data retrieval functions, this code snippet
#' is now here
#'
#' @return TRUE or FALSE
#'
#'
#' @noRd
#' @keywords internal
#' @export
check_connection <- function(api_url) {
  conn<-url(api_url)
  doesnotexist<-inherits(try(suppressWarnings(readLines(conn)),silent=TRUE),"try-error")
  close(conn)
  if (doesnotexist) {
    warning(paste("Sorry! API", api_url, "not available! Returning NULL"))
    return(FALSE)
  }
  return(TRUE)
}
