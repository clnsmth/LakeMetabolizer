
context("sun.rise.set()")

library(LakeMetabolizer)

test_that("POSIXct tzone attributes are used", {
  
  # Current implementation ----------------------------------------------------
  
  # Value returned in time zone of Sys.timezone() (may be an hour off in in daylight savings time)
  sun.rise.set(lat = 40.75, datetimes = as.POSIXlt('2013-03-31')) # Date is CDT but output is CST
  
  # Specifying a time zone and setting with.sys.tzone = TRUE results in Value returned in 
  # time zone of Sys.timezone() with respect to the tzone attribute of the POSIXct POSIXlt
  # object (and may be an hour off in in daylight savings time) but note the
  sun.rise.set(lat = 40.75, datetimes = as.POSIXlt('2013-03-31', tz = "Etc/GMT+5")) # Date is CDT but output is CST
  
  # POSIXct inputs produce same results as POSIXlt
  sun.rise.set(lat = 40.75, datetimes = as.POSIXct('2013-03-31'))
  sun.rise.set(lat = 40.75, datetimes = as.POSIXct('2013-03-31', tz = "Etc/GMT+5"))
  
  # Proposed implementation ---------------------------------------------------
  
  r <- sun.rise.set(lat = 37.880280, datetimes = as.POSIXlt('2019-08-17', tz = "Etc/GMT+5"))
  class(r$rise)
  class(r$set)
  attr(r$rise, "tzone")
  attr(r$set, "tzone")
  lubridate::with_tz(r$rise, tzone = "Etc/GMT+7") # In PDT
  
})
