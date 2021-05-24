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
