#' @title Checks if the file exists and then load it as data.frame.
#'
#' @description  Default arg value provides pre-loaded database.
#'
#' @param filename character - a path to the file or the file name.
#'
#' @return data in a data.frame format
#'
#' @seealso \code{\link{eq_CleanData}} \code{\link{eq_GenLocationName}} \code{\link{geom_TimeLine}} \code{\link{geom_TimeLineLable}}
#'
#' @source National Geophysical Data Center / World Data Service NGDC/WDS: Significant Earthquake Database. National Geophysical Data Center, NOAA. doi:10.7289/V5TD9V7K
#'
#' @examples
#'
#' eq_ReadQuake()
#'
#' @export


eq_ReadQuake = function(filename=""){

  if(filename==""){

  path_package = system.file("exdata","EarthQuakeData.txt", package = "EarthQuakeCapstone")

    data <- suppressMessages({
    res_raw = readr::read_delim(path_package,
                                delim = "\t",
                                na = c("-99","NA",""))  })
  return(dplyr::tbl_df(data))

  } else{

    if(!file.exists(filename)){stop("file ", filename, " does not exist. Firstly, check out the path.")}
    data <- suppressMessages({
      res_raw = readr::read_delim(filename,
                                  delim = "\t",
                                  na = c("-99","NA",""))  })
    return(dplyr::tbl_df(data))
  }
}



#' @title clean/format existing variables for earthquake data.
#'
#' @description Moreover it generates additinal variables - MAGNITUDE and DATE which is important at vizualization step.
#'
#' @param df data.frame - earthquake data
#'
#' @return data.frame - cleaned earthquake data
#'
#' @seealso \code{\link{eq_ReadQuake}} \code{\link{eq_GenLocationName}} \code{\link{geom_TimeLine}} \code{\link{geom_TimeLineLable}}
#'
#' @examples
#'
#' df = eq_ReadQuake()
#' eq_CleanData(df)
#'
#' @importFrom 	magrittr %>%
#'
#' @export


eq_CleanData = function(df){

  .=AVG_MAGNITUDE=NULL

  df = df %>%
    dplyr::mutate_if(is.character,trimws) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("EQ")),dplyr::funs(as.numeric)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("ITUDE")),dplyr::funs(as.numeric)) %>%
    dplyr::mutate_at("DEATHS",dplyr::funs(as.numeric)) %>%
    dplyr::mutate_at(c("LOCATION_NAME"),dplyr::funs(stringr::str_to_title))

  df = eq_GetPosixDate(df)

  df$MAGNITUDE <- df %>%
    dplyr::select(dplyr::contains("EQ")) %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::mutate(AVG_MAGNITUDE=rowMeans(.,na.rm=T)) %>%
    dplyr::select(AVG_MAGNITUDE) %>%
    unlist()

  df = df %>% dplyr::filter_at(c("MAGNITUDE","DATE"),dplyr::all_vars(!is.na(.)))

  return(df)

}


#' @title  Function is provided to process LOCATION_NAME variable in such way to subset only a region name.
#'
#' @description If event occur at few locations the function return region names for all places.
#'
#' @param df character - a path to the file or the file name
#' @param to_name character - a path to the file or the file name
#'
#' @return data.frame - data expanded by LOCATION variable
#'
#' @seealso \code{\link{eq_ReadQuake}} \code{\link{eq_ReadQuake}} \code{\link{geom_TimeLine}} \code{\link{geom_TimeLineLable}}
#'
#' @examples
#'
#'  df = eq_ReadQuake()
#'  eq_GenLocationName(eq_CleanData(df))
#'
#' @export

eq_GenLocationName <- function(df,to_name="LOCATION"){

  condit1 = !is.data.frame(df)

  condit2 = !all(c("COUNTRY","LOCATION_NAME") %in% colnames(df))

  if(condit1 || condit2) stop("is not a data.frame or do not contain COUNTRY or LOCATION_NAME variables")

  str_out = c(unique(df$COUNTRY),
              "SW",":","SUR","W","-")

  loc = df$LOCATION_NAME

  for(i in seq_along(str_out)) loc = stringr::str_replace_all(loc,stringr::fixed(str_out[i]),"")

  loc = stringr::str_replace_all(loc,stringr::fixed(";"),"  ")

  loc = unlist(lapply(loc,trimws))

  df[to_name] = stringr::str_to_title(loc)

  return(df)

}

#' Component of \code{eq_CleanData} - genereting POSIX Time variable
#'
#' @format NULL
#' @usage NULL
#' @importFrom 	magrittr %>%
#' @export

eq_GetPosixDate = function(df){

  .data=.=NULL

  stopifnot(is.data.frame(df),
            all( c("MONTH", "DAY","HOUR", "SECOND") %in% colnames(df) )
  )
  df$SECOND = as.integer(trimws(df$SECOND))

  df = df %>% dplyr::mutate_at(c("MONTH","DAY","HOUR","MINUTE","SECOND"),dplyr::funs(formatC(ifelse(is.na(.),1,.),width=2,flag=0)))

  res = dplyr::filter(df,.data$YEAR>0)

  res$year = formatC(res$YEAR,width=4,flag=0)

  res = res %>%
    dplyr::mutate(Date_Time = paste(paste(.data$year,.data$MONTH,.data$DAY,sep=":"),paste(.data$HOUR,.data$MINUTE,.data$SECOND,sep=":"))) %>%
    dplyr::mutate(DATE=lubridate::ymd_hms(.data$Date_Time,tz="UTC")) %>%
    dplyr::select(-dplyr::one_of(c("Date_Time","year","YEAR","MONTH","DAY","HOUR","MINUTE","SECOND")))

  res2 = df %>% dplyr::filter(.data$YEAR<0)

  addit = cumsum(sapply(1:2500,function(i) ifelse((i%%4==0&&i%%100!=0)|i%%400==0,1,0)))

  res2 = res2 %>%
    dplyr::mutate(DATE = as.POSIXct(as.POSIXct("0000-01-01 01:01:01","UTC") - 60*60*24*365*abs(.data$YEAR) - 60*60*24*addit[abs(.data$YEAR)],"UTC")) %>%
    dplyr::select(-dplyr::one_of(c("YEAR","MONTH","DAY","HOUR","MINUTE","SECOND")))

  res_final = dplyr::bind_rows(res2,res)

  return(res_final)

}
