
library(leaflet)

db <- clean_raw_data()
p <- eq_map(db[COUNTRY %in%c("GREECE") & DATE>"2000-01-01"],annot_col = "DATE")

test_that("leaflet functions work",{
    expect_is(p, "leaflet")
})
