#' Clean raw data
#'
#' function to read in raw data and clean up the following columns:
#'     - DATE
#'     - Lat & Long
#'     - Location
#'
#' @param raw_data_loc
#'
#' @return a DT
#' @export
#'
#' @import data.table
#'
#' @examples \dontrun{db <- clean_raw_data()}
clean_raw_data <- function(
    raw_data_loc = system.file("extdata", "signif.txt", package = "noaaQuake")
    ){

    raw_data <- data.table::fread(raw_data_loc)

    raw_data[is.na(MONTH), MONTH := 1]
    raw_data[is.na(DAY), DAY := 1]

    raw_data[,DATE:=as.Date(paste(YEAR, MONTH, DAY, sep="-"),"%Y-%m-%d")]
    raw_data[is.na(DATE),DATE:=as.Date("0000-01-01")+365*(YEAR-1)]
    raw_data[,.(YEAR,MONTH,DAY,DATE)]

    # fread automatically changes long and lat to numeric
    # str(raw_data[,LONGITUDE])

    raw_data <- eq_location_clean(raw_data)

    return(raw_data)

}
#' eq_location_clean
#'
#' function to clean up the location column
#'
#' @param raw_data
#'
#' @return a DT
#' @export
#'
#' @import data.table
#'
#' @examples \dontrun{db <- eq_location_clean(db)}
eq_location_clean<-function(raw_data){

    raw_data[,LOCATION_NAME := gsub(".*:","",LOCATION_NAME)]
    raw_data[,LOCATION_NAME := trimws(LOCATION_NAME)]
    raw_data[,LOCATION_NAME := tolower(LOCATION_NAME)]
    raw_data[,LOCATION_NAME := gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", LOCATION_NAME, perl=TRUE)]
    raw_data[,.(LOCATION_NAME)]

    return(raw_data)


}
