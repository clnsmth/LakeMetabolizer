
context("sun.rise.set()")

library(LakeMetabolizer)

test_that("POSIXct tzone attributes are used", {
  
  # Current implementation ----------------------------------------------------
  
  # Sys.timezone() == "America/Los_Angeles"
  sun.rise.set(lat = 37.880280, datetimes = as.POSIXlt('2019-08-17'))
  # Returns a rise time of "05:17:25", which is ~ 1 hr and 10 min earlier than the actual rise
  # time.
  
  # Sys.timezone() == "America/Los_Angeles" and tzone attribute is "Etc/GMT+5" (CDT)
  sun.rise.set(lat = 37.880280, datetimes = as.POSIXlt('2019-08-17', tz = "Etc/GMT+5"))
  # Returns a rise time of "2019-08-17 03:16:21 PDT" which is ~ 1 hr and 10 min earlier than the 
  # actual rise time (with some precision issues, i.e. should match PDT minute resolution).
  
  # Proposed implementation ---------------------------------------------------
  
  r <- sun.rise.set(lat = 37.880280, datetimes = as.POSIXlt('2019-08-17', tz = "Etc/GMT+5"))
  class(r$rise)
  class(r$set)
  attr(r$rise, "tzone")
  attr(r$set, "tzone")
  lubridate::with_tz(r$rise, tzone = "Etc/GMT+7") # In PDT
  
})
