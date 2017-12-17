library(ggplot2)
library(dplyr)
db <- clean_raw_data()
p <- ggplot(db[COUNTRY %in%c("CHINA","GREECE")],
       aes (x = as.Date(DATE), y = COUNTRY, color = as.numeric(DEATHS)/1000, size = as.numeric(EQ_PRIMARY))) +
    geom_timeline(aes(xmin = as.Date("2000-01-01"), xmax = as.Date("2018-01-01"))) +
    geom_timeline_label(n_max = 15, aes(label = LOCATION_NAME, xmin = as.Date("2000-01-01"),
                            xmax = as.Date("2018-01-01")))+
    xlab("Date") + ylab("") + ggtitle("Timeline of Earthquakes") +
    labs(size = "Richter scale", color = "# deaths (k)" )+
    theme_bw() +
    theme(legend.position="bottom")

test_that("geom timeline works",{

    expect_is(p, "ggplot")
    expect_equal(min(year(as.Date(layer_data(p)$x,origin="1970-01-01"))), 2000)

})


