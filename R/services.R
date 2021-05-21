#' @title Turku Air Quality Monitoring
#'
#' @description Access data from Turku Region Air Quality Monitoring Stations
#'
#' @param station.id NULL (default) returns information from all stations, inputing
#' one or more ID's returns information for given stations. Valid stations are:
#' "kauppatori", "ruissalo", "raisio", "naantali", "kaarina", "kaanaa" and "parainen".
#' @param to.sf FALSE (default) returns a data.table, TRUE returns an sf object
#' for easy visualization on map
#' @param timeseries FALSE (default) returns the last observation for station(s),
#' TRUE returns historical data
#'
#' @return a data.frame or an sf object
#'
#' @author Pyry Kantanen
#' @examples
#' kauppatori <- get_airmonitoring(station.id="kauppatori")
#'
#' @source API Location: https://api.turku.fi/airmonitoring/v1/
#'
#' API documentation: https://api.turku.fi/airmonitoring/v1/
#'
#' @importFrom httr parse_url build_url
#' @importFrom jsonlite fromJSON
#' @importFrom geojsonsf geojson_sf
#' @importFrom utils read.csv
#'
#' @export

get_airmonitoring <- function(station.id = NULL, to.sf = FALSE, timeseries = FALSE) {

  if (to.sf == TRUE && timeseries == TRUE) {
    stop("Cannot get timeseries data if to.sf is TRUE")
  }

  if (is.null(station.id) == TRUE && timeseries == TRUE) {
    stop("Select a single feature to download history data from")
  }

  api_url <- "https://api.turku.fi/airmonitoring/v1/stations"
  if (is.null(station.id) == TRUE || length(station.id) == 1){
    query_url <- paste(api_url, station.id, sep = "/")
  } else if (length(station.id) > 1) {
    query_url <- paste(api_url, NULL, sep = "/")
  }

  url <- parse_url(query_url)
  # url$query <- list(...)
  url <- build_url(url)

  # Check whether API url available
  conn<-url(api_url)
  doesnotexist<-inherits(try(suppressWarnings(readLines(conn)),silent=TRUE),"try-error")
  close(conn)
  if (doesnotexist) {
    warning(paste("Sorry! API", api_url, "not available! Returning NULL"))
    return(NULL)
  }

  message(
    "All content is available under CC BY 4.0, except where otherwise stated.
The City of Helsinki logo is a registered trademark. The Helsinki Grotesk
Typeface is a proprietary typeface licensed by Camelot Typefaces.
CC BY 4.0: <https://creativecommons.org/licenses/by/4.0/>")

  if (to.sf == TRUE) {
    res_list <- suppressWarnings(geojson_sf(url))
    return(res_list)
  } else if (to.sf == FALSE){
    res_list <- fromJSON(url, flatten = TRUE)
  }

  if (is.null(station.id) == FALSE && length(station.id) == 1){
    res_list <- unlist(res_list)
    res_list <- as.list(res_list)
    res_list <- as.data.frame(res_list)
  } else if (is.null(station.id) == FALSE && length(station.id) > 1){
    res_list <- res_list$features
    res_list <- res_list[which(res_list$id %in% station.id),]
  }

  if (timeseries == TRUE) {
    historydata_urls <- res_list$properties.historydata
    res <- lapply(historydata_urls, read.csv)
    res <- do.call(rbind.data.frame, res)
    return(res)
  }

  res_list
}



#' @title Access Turku Region Linked Events API
#'
#' @description Easy access to Turku Region Linked Events API
#'
#' @source See \href{https://www.avoindata.fi/data/fi/dataset/turku-energian-ymparistotaideteokset}{avoindata.fi}
#' for additional information.
#'
#' @param data.url Dataset url
#'
#' @return sf object
#'
#' @author Pyry Kantanen
#'
#' @examples
#' art_sf <- get_turku_energia_art()
#'
#' @importFrom utils read.csv2
#' @importFrom sf st_as_sf st_sf st_crs
#'
#' @export

get_turku_energia_art <- function(data.url = "https://dev.turku.fi/datasets/turku-energia-ymparistotaideteokset.csv") {

  # Initial settings
  data_url <- data.url
  crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # Check whether API url available
  conn<-url(data_url)
  doesnotexist<-inherits(try(suppressWarnings(readLines(conn)),silent=TRUE),"try-error")
  close(conn)
  if (doesnotexist) {
    warning(paste("Sorry! API", data_url, "not available! Returning NULL"))
    return(NULL)
  }

  data <- read.csv2(data_url, header = TRUE, dec = ".", fileEncoding = "latin1")
  sf_objekti <- st_as_sf(x = data, coords = c(3, 4), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  sf_objekti
}

#' @title List Turku regoion addresses
#'
#' @description Download and list addresses in Turku region
#'
#' @details Dataset contains addresses for Turku, Kaarina, Aura, Lieto,
#' Marttila, Paimio, Sauvo, Rusko, Raisio, Masku, Nousiainen, Mynämäki and Naantali.
#'
#' Coordinates are given in EPSG:3877 (ETRS-GK23) format
#'
#' @source See \href{https://www.avoindata.fi/data/fi/dataset/turun-seudun-osoitteet}{avoindata.fi}
#' for additional information.
#'
#' @param city Choose a city. Default is NULL, returning whole dataset
#' @param to.sf Turn list into an sf object. Default is FALSE
#'
#' @return list or sf object
#'
#' @author Pyry Kantanen
#'
#' @examples
#' addresses <- get_turku_addresses()
#'
#' @importFrom utils read.csv2
#' @importFrom sf st_as_sf st_sf st_crs
#'
#' @export
get_turku_addresses <- function(city = NULL, to.sf = FALSE) {
  url <- "https://api.turku.fi/addresses.csv"
  data_object <- read.csv2(file = url, header = FALSE, fileEncoding = "latin1")
  names(data_object) <- c("city", "street", "number", "lon", "lat")

  # Some municipalities have made errors in data input, here is a temp fix
  rows_to_correct <- which(is.na(data_object$lat))
  latitudes <- data_object$lon[rows_to_correct]
  longitudes <- data_object$number[rows_to_correct]
  data_object$lat[rows_to_correct] <- latitudes
  data_object$lon[rows_to_correct] <- longitudes
  data_object$number[rows_to_correct] <- NA_integer_

  if (!is.null(city)) {
    data_object <- data_object[which(data_object$city %in% city),]
  }

  if (to.sf == TRUE) {
    sf_object <- st_as_sf(x = data_object, coords = c("lon", "lat"))
    st_crs(sf_object) <- "EPSG:3877"
    return(sf_object)
  } else {
    return(data_object)
  }
}
