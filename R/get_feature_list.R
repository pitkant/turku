#' @title Print all available Features
#'
#' @description Basically a neat wrapper for "request=GetCapabilities".
#'
#' @details Lists all <FeatureType> nodes.
#'
#' @seealso Use \code{\link{get_feature}} to download feature,
#' \code{\link{select_feature}} for menu-driven listing and downloading
#'
#' @param base.url a WFS url, for example "https://opaskartta.turku.fi/TeklaOGCWeb/WFS.ashx"
#'
#' @return data frame
#'
#' @import dplyr
#' @importFrom purrr flatten_dfc
#' @importFrom xml2 as_list xml_find_all xml_ns_strip
#'
#' @author Pyry Kantanen <pyry.kantanen@@gmail.com>
#'
#' @examples
#' \dontrun{
#' dat <- get_feature_list(base.url = "https://opaskartta.turku.fi/TeklaOGCWeb/WFS.ashx")
#' }
#'
#' @export
get_feature_list <- function(base.url = NULL) {

  if (is.null(base.url)) {
    message("base.url = NULL. Using https://opaskartta.turku.fi/TeklaOGCWeb/WFS.ashx")
    base.url <- "https://opaskartta.turku.fi/TeklaOGCWeb/WFS.ashx"
  }

  resp <- wfs_api(base.url = base.url, queries = "request=GetCapabilities")
  content <- resp$content

  # For some reason this seems to be a necessary step
  # for xml_find_all to function
  content_ns_strip <- xml_ns_strip(content)

  # All "<FeatureType>" nodes
  all_features <- xml_find_all(x = content_ns_strip, xpath = "//FeatureType ")

  df <- data.frame(matrix(NA, nrow = length(all_features), ncol = 2))
  names(df) <- c("Name", "Title")

  for (i in seq_len(length(all_features))) {
    all_features_list <- as_list(all_features[[i]])
    df[i,] <- flatten_dfc(all_features_list[c("Name", "Title")])
  }

  df$Namespace <- gsub(":.*", "", df$Name)

  # Without using flatten_dfc
  #for (i in 1:length(kaikki)) {
  #  kaikki_list <- xml2::as_list(kaikki[[i]])
  #  flatten_list <- purrr::flatten(kaikki_list)
  #  df[i,] <- as.data.frame(flatten_list[c("Name", "Title")])
  # }

  df
}

#' @title Interactively browse and select features
#'
#' @description Use an interactive menu to select and download a feature
#' for use in other functions
#'
#' @seealso \code{\link{get_feature}}, \code{\link{get_feature_list}}
#'
#' @return feature Title (character) or feature object (sf), if \code{get} parameter is TRUE
#'
#' @param base.url WFS url, for example "https://opaskartta.turku.fi/TeklaOGCWeb/WFS.ashx"
#' @param get Should the selected feature be downloaded? Default is \code{FALSE}
#'
#' @author Pyry Kantanen <pyry.kantanen@@gmail.com>
#'
#' @examples
#' \dontrun{
#' url <- "https://opaskartta.turku.fi/TeklaOGCWeb/WFS.ashx"
#' selection <- select_feature(base.url = url)
#' feature <- get_feature(base.url = url, type_name = selected)
#' ggplot(feature) +
#'   geom_sf()
#' }
#'
#' @importFrom utils select.list
#'
#' @export
select_feature <- function(base.url = NULL, get = FALSE) {
  df <- get_feature_list(base.url = base.url)
  unique_namespace <- unique(df$Namespace)
  selected_namespace <- select.list(choices = unique_namespace,
                                    title = "From which namespace?",
                                    graphics = FALSE)

  df2 <- df[which(df$Namespace == selected_namespace),]

  selected_title <- select.list(choices = df2$Title,
                                  title = "Which dataset?",
                                  graphics = FALSE)

  # Name = Namespace:Title
  selected_row <- df[which(df$Title == selected_title),]
  selected_name <- selected_row$Name

  if (get == TRUE) {
    object <- get_feature(base.url = base.url, typename = selected_name)
    return(object)
  } else {
    return(selected_name)
  }
}
