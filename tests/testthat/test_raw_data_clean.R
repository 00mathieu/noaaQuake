

test_that("raw data is pulled in and cleaned",{
    db <- clean_raw_data()
    expect_is(db, "data.table")
    expect_is(db[,DATE],"Date")
    expect_is(db[,LATITUDE],"numeric")
    expect_is(db[,LONGITUDE],"numeric")
})


