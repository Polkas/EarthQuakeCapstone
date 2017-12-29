
#' @title Function for plotting a leaflet map of earthquake occurences.
#'
#' @param df data.frame - cleaned earthquake data
#' @param label character - Column in the tbl_df object to be used for annotation.
#'
#' @return leaflet map - interactive map of significant earthquakes
#'
#' @references \url{http://rstudio.github.io/leaflet/}
#'
#' @seealso \code{\link{eq_CreateLabel}}
#'
#' @importFrom 	magrittr %>%
#'
#' @examples
#'
#' df = eq_ReadQuake()
#'
#' library(dplyr)
#'
#' eq_CleanData(df) %>%
#' eq_GenLocationName() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 1980) %>%
#' eq_Map(label = "DATE")
#'
#' @export
eq_Map <- function(df, label="DATE"){

  stopifnot(all(c('DATE','LATITUDE','LONGITUDE','MAGNITUDE') %in%  colnames(df)))

  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = df,
                              lng = ~ LONGITUDE,
                              lat = ~ LATITUDE,
                              radius = ~ MAGNITUDE,
                              weight = 1,
                              fillOpacity = 0.1,
                              popup = ~paste(get(label)))

}


#' @title Supporting function for \code{eq_Map}. It should be used for genereting more informative labels.
#
#' @param df data.frame - cleaned earthquake data
#'
#' @return data.frame - earthquake data with an addditional event description variable in a html format.
#'
#' @seealso \code{\link{eq_Map}}
#'
#' @importFrom 	magrittr %>%
#'
#' @examples
#'
#' df = eq_ReadQuake()
#'
#' library(dplyr)
#'
#' eq_CleanData(df) %>%
#' eq_GenLocationName() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 1980) %>%
#' eq_CreateLabel() %>%
#' eq_Map(label = "popup_text")
#'
#' @export

eq_CreateLabel <- function(df) {

  .data=NULL

  stopifnot(all(c('MAGNITUDE',"DEATHS","LOCATION") %in%  colnames(df)))

  popup_text =  df %>%
    dplyr::mutate(popup_text = paste(paste0("<b>Location:</b> ",.data$LOCATION),
         paste0("<b>Magnitude:</b> ", .data$MAGNITUDE),
         paste0("<b>Total Deaths:</b> ", .data$DEATHS),sep ='<br>'))

  return(popup_text)
}
