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

  api_url <- "https://api.turku.fi/airmonitoring/v1/stations"

  if (to.sf == TRUE && timeseries == TRUE) {
    stop("Cannot get timeseries data if to.sf is TRUE")
  }

  if (is.null(station.id) == TRUE && timeseries == TRUE) {
    stop("Select a single feature to download history data from")
  }

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
#' @description Easy access to Turku Region Linked Events API
#' @source See \href{https://www.avoindata.fi/data/fi/dataset/turku-energian-ymparistotaideteokset}{avoindata.fi}
#' for additional information.
#' @param data.url Dataset url
#' @param to.sf Turn list into an sf object. Default is FALSE
#' @return data.frame or sf object
#' @author Pyry Kantanen
#' @examples
#' art_sf <- get_turku_energia_art()
#' @importFrom utils read.csv2
#' @importFrom sf st_as_sf st_sf st_crs
#' @export

get_turku_energia_art <- function(data.url = "https://dev.turku.fi/datasets/turku-energia-ymparistotaideteokset.csv",
                                  to.sf = FALSE) {

  # Initial settings
  data_url <- data.url
  data_crs <- "EPSG:4326"
  # WGS84 "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # Check whether API url available
  conn<-url(data_url)
  doesnotexist<-inherits(try(suppressWarnings(readLines(conn)),silent=TRUE),"try-error")
  close(conn)
  if (doesnotexist) {
    warning(paste("Sorry! API", data_url, "not available! Returning NULL"))
    return(NULL)
  }

  data <- read.csv2(data_url, header = TRUE, dec = ".", fileEncoding = "latin1")
  if (to.sf == TRUE) {
    sf_object <- st_as_sf(x = data, coords = c(3, 4), crs = data_crs)
    return(sf_object)
  }
  data
}

#' @title List Turku region addresses
#' @description Download and list addresses in Turku region
#' @details Dataset contains addresses for Turku, Kaarina, Aura, Lieto,
#' Marttila, Paimio, Sauvo, Rusko, Raisio, Masku, Nousiainen, Mynämäki and Naantali.
#'
#' Coordinates are given in EPSG:3877 (ETRS-GK23) format
#' @source See \href{https://www.avoindata.fi/data/fi/dataset/turun-seudun-osoitteet}{avoindata.fi}
#' for additional information.
#' @param city Choose 1 or more cities. Default is NULL, returning the whole
#' dataset
#' @param to.sf Turn list into an sf object. Default is FALSE
#' @param fix.data Quick and dirty fix for erroneous input in data. Default is
#' TRUE.
#' @return list or sf object
#' @author Pyry Kantanen
#' @examples
#' addresses <- get_turku_addresses(city = c("Turku", "Kaarina"), to.sf = FALSE)
#' @importFrom utils read.csv2
#' @importFrom sf st_as_sf st_sf st_crs
#' @export
get_turku_addresses <- function(city = NULL, to.sf = FALSE, fix.data = TRUE) {
  data_crs <- "EPSG:3877"
  url <- "https://api.turku.fi/addresses.csv"
  city <- lapply(city, .simpleCap)

  data_object <- read.csv2(file = url, header = FALSE, fileEncoding = "latin1")
  names(data_object) <- c("city", "street", "number", "lon", "lat")

  if (fix.data == TRUE) {
    # Some municipalities have made errors in data input, here is a quick fix
    rows_to_correct <- which(is.na(data_object$lat))
    latitudes <- data_object$lon[rows_to_correct]
    longitudes <- data_object$number[rows_to_correct]
    data_object$lat[rows_to_correct] <- latitudes
    data_object$lon[rows_to_correct] <- longitudes
    data_object$number[rows_to_correct] <- NA_integer_
  }

  if (!is.null(city)) {
    data_object <- data_object[which(data_object$city %in% city),]
  }

  if (to.sf == TRUE && fix.data == TRUE) {
    # fix.data must be true as data cannot contain rows without geo data
    sf_object <- st_as_sf(x = data_object, coords = c("lon", "lat"))
    sf::st_crs(sf_object) <- data_crs
    return(sf_object)
  } else {
    return(data_object)
  }
}

#' @title Turku street maintenance API
#' @description Download and list where maintenance vehicles are located and
#'   what they are doing.
#' @details Possible events for vehicles: \itemize{ \item{"kv"} {Work done on
#'   bicycle and pedestrian lanes} \item{"au"} {Snow removal (auraus)}
#'   \item{"su"} {De-icing with salt (suolaus)} \item{"hi"} {Spreading sand
#'   (hiekoitus)} \item{"nt"} {Mowing (niitto)} \item{"ln"} {Levelling down high
#'   spots (Lanaus)} \item{"hs"} {Planing (höyläys)} \item{"pe"} {Street washing
#'   (kadunpesu)} \item{"ps"} {Dust binding (Pölynsidonta)} \item{"hn"} {Sand
#'   removal (hiekannosto)} \item{"hj"} {Brushing (harjaus)} \item{"pn"}
#'   {Coating (pinnoitus, liittyy kesähoitoreitteihin} }
#' @source See
#'   \href{https://www.avoindata.fi/data/fi/dataset/turun-kaupungin-katujen-kunnossapitorajapinta}{avoindata.fi}
#'    for additional information. See
#'   \href{https://github.com/City-of-Helsinki/aura/wiki/API}{City of Helsinki documentation}
#'   for more information on the API.
#' @param ... List queries, for example vehicle id as integer,
#' \code{history} (integer), \code{since} (integer), \code{limit} (integer),
#' \code{temporal_resolution} (integer)
#' @param id Vehicle ID as integer
#' @return data.frame
#' @author Pyry Kantanen
#' @examples
#' ## vehicle 8, 10 last instances
#' vehicles <- get_maintenance_locations(id = 8, history = 10)
#' @importFrom jsonlite fromJSON
#' @export
get_maintenance_locations <- function(id = NULL, ...) {
  api_url <- "https://api.turku.fi/street-maintenance/v1/vehicles"

  if (!is.null(id)) {
    paste(api_url, id, sep = "/")
  }

  url <- parse_url(api_url)
  url$query <- list(...)
  url <- build_url(url)

  object <- jsonlite::fromJSON(txt = api_url, simplifyVector = TRUE)
  object
}
