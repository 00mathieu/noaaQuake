#
#
# library(ggplot2)
# library(dplyr)
# db <- clean_raw_data()
# ggplot(db[COUNTRY %in%c("CHINA","GREECE")],
#        aes (x = as.Date(DATE), y = COUNTRY, color = as.numeric(DEATHS)/1000, size = as.numeric(EQ_PRIMARY))) +
#     geom_timeline(aes(xmin = as.Date("2000-01-01"), xmax = as.Date("2018-01-01"))) +
#     geom_timeline_label(n_max = 15, aes(label = LOCATION_NAME, xmin = as.Date("2000-01-01"),
#                             xmax = as.Date("2018-01-01")))+
#     xlab("Date") + ylab("") + ggtitle("Timeline of Earthquakes") +
#     labs(size = "Richter scale", color = "# deaths (k)" )+
#     theme_bw() +
#     theme(legend.position="bottom")
#
#
#
# library(leaflet)
#
# eq_map(db[COUNTRY %in%c("GREECE") & DATE>"2000-01-01"],annot_col = "DATE")
# eq_map(db[COUNTRY %in%c("GREECE") & DATE>"2000-01-01"], annot_col = "popup_text")
