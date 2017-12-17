#' Plot quakes on leaflet map
#'
#' @param data a DT
#' @param annot_col string with name of annotation column
#'
#' @return a leaflet map
#' @export
#'
#' @import leaflet
#'
#' @examples \dontrun{
#' eq_map(db[COUNTRY %in%c("GREECE") & DATE>"2000-01-01"],
#'     annot_col = "DATE")
#' }
eq_map <- function(data, annot_col){

    if(annot_col == "popup_text"){data<-eq_create_label(data)}

    leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addProviderTiles("Stamen.TonerHybrid")%>%
        leaflet::addCircleMarkers(data = data,
                                  radius = ~ EQ_PRIMARY,
                                  lng = ~ LONGITUDE,
                                  lat = ~ LATITUDE,
                                  weight = 2,
                                  popup = data[[annot_col]])

}


#' function to add popup text to db
#'
#' @param data a DT
#'
#' @return a DT
#' @export
#'
#' @import data.table
#'
#' @examples \dontrun{
#' eq_map(db[COUNTRY %in%c("GREECE") & DATE>"2000-01-01"],
#'     annot_col = "popup_text")
#' }
eq_create_label <- function(data){

    data[,row1:=""]
    data[,row2:=""]
    data[,row3:=""]

    data[!is.na(LOCATION_NAME),row1:=paste("<b>Location: </b>", LOCATION_NAME, "<br/>",sep="")]
    data[!is.na(EQ_PRIMARY),row2:=paste("<b>Magnitude: </b>", EQ_PRIMARY, "<br/>",sep="")]
    data[!is.na(TOTAL_DEATHS),row3:=paste("<b>Total deaths: </b>", TOTAL_DEATHS, "<br/>",sep="")]

    data[,popup_text:=paste(row1, row2, row3, sep="")]

    data[,row1:=NULL]
    data[,row2:=NULL]
    data[,row3:=NULL]

    return(data)
}
