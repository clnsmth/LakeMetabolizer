
context("sun.rise.set()")

library(LakeMetabolizer)

test_that("POSIXct tzone attributes are used", {
  
  # Parameterize --------------------------------------------------------------
  
  # Chicago
  time_zone <- "America/Chicago"
  time_zone <- "Etc/GMT+5"
  latitude <- 41.8781
  longitude <- -87.6298
  
  # daylight to standard time transition
  dates <- c("2012-11-01", 
             "2012-11-02",
             "2012-11-03",
             "2012-11-04",
             "2012-11-05",
             "2012-11-06")
  
  # Target behavior -----------------------------------------------------------
  
  dplyr::select(
    suncalc::getSunlightTimes(
      date = as.Date(dates),
      lat = latitude,
      lon = longitude, 
      tz = time_zone),
    sunrise, 
    sunset)$sunrise
  
  # Proposed implementation ---------------------------------------------------
  
  LakeMetabolizer::sun.rise.set(
    # as.POSIXct(dates),
    as.POSIXct(dates, tz = time_zone),
    lat = latitude, 
    with.sys.timezone = FALSE)$rise
  
  # Current implementation ----------------------------------------------------
  
  riseset_lm <- LakeMetabolizer::sun.rise.set(
    as.POSIXct(dates, tz = time_zone),
    lat = latitude, 
    with.sys.timezone = FALSE)
  
  
})
