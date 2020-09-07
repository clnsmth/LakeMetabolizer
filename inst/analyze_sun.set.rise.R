# Load dependencies
library(LakeMetabolizer)
library(suncalc)
library(data.table)
library(lubridate)
library(RCurl)
library(stringr)
library(ggplot2)

# Set system time zone for LakeMetabolizer. Without doing this, returned values
# will be rise/set times in Chicago converted to your local systems time zone.
Sys.setenv(TZ = "America/Chicago")

# Create a function to compile test data composed of:
# 1.) NOAA sun rise/set times for the years 2011 and 2012, and for the location
# Chicago IL in the America/Chicago time zone obtained from
# https://www.esrl.noaa.gov/gmd/grad/solcalc/
# 2.) LakeMetabolizer calculated sun rise/set times for dates included in #1 
# above
# 3.) suncalc calculated sun rise/set times for dates included in #1 
# above
#
create_test_data <- function() {
  
  # Set parameters
  latitude <- 41.8781
  longitude <- -87.6298
  time_zone <- "America/Chicago"
  files <- c(
    "https://raw.githubusercontent.com/clnsmth/LakeMetabolizer/noaa_data/data/sunrise_2011_america_chicago.csv",
    "https://raw.githubusercontent.com/clnsmth/LakeMetabolizer/noaa_data/data/sunset_2011_america_chicago.csv",
    "https://raw.githubusercontent.com/clnsmth/LakeMetabolizer/noaa_data/data/sunrise_2012_america_chicago.csv",
    "https://raw.githubusercontent.com/clnsmth/LakeMetabolizer/noaa_data/data/sunset_2012_america_chicago.csv")
  
  # Read and format NOAA data
  d <- lapply(files, data.table::fread)
  names(d) <- tools::file_path_sans_ext(basename(files))
  datetimes <- lapply(
    names(d),
    function(file) {
      month <- colnames(d[[file]])
      month <- month[month != "Day"]
      datetime_strings <- lapply(
        month,
        function(m) {
          paste(
            d[[file]]$Day,
            m, 
            stringr::str_extract(file, "[:digit:]+"),
            d[[file]][[m]])
        })
      suppressWarnings(
        lubridate::dmy_hm(unlist(datetime_strings), tz = time_zone))
    })
  names(datetimes) <- stringr::str_remove(names(d), "(?<=[:digit:]{4}).+")
  d <- data.frame(
    noaa_rise = c(datetimes$sunrise_2011, datetimes$sunrise_2012),
    noaa_set = c(datetimes$sunset_2011, datetimes$sunset_2012))
  d$date <- lubridate::date(d$noaa_rise)
  d <- d[complete.cases(d), ]
  
  # Calculate and add sun rise/set using LakeMetabolizer
  lm_output <- LakeMetabolizer::sun.rise.set(
    datetimes = as.POSIXlt(as.character(d$date), tz = time_zone),
    lat = latitude)
  d$lm_rise <- lm_output[, 1]
  d$lm_set <- lm_output[, 2]
  
  # Calculate and add sun rise/set using suncalc
  sc_output <- suncalc::getSunlightTimes(
    date = d$date,
    lat = latitude,
    lon = longitude, 
    tz = time_zone)
  d$sc_rise <- sc_output$sunrise
  d$sc_set <- sc_output$sunset
  
  # Mark dates observing daylight savings time
  d$dst <- lubridate::dst(d$noaa_rise)
  
  # Reorder columns for readability
  d <- dplyr::select(
    d, date, dst, noaa_rise, lm_rise, sc_rise, noaa_set, lm_set, sc_set)
  
  # Remove outliers - During transitions between standard time and daylight 
  # time, LakeMetabolizer correctly adjusts to the change but for only 1 day 
  # and then reverts back to standard time.
  dates_to_remove <- as.Date(
    c("2011-03-13", "2011-11-06", "2012-03-11", "2012-11-04"))
  d <- d[!(d$date %in% dates_to_remove), ]
  
  # Return
  d
  
}




# Create a function to compare sun/rise set values calculated from 
# LakeMetabolizer and suncalc to NOAA values
#
compare_to_noaa <- function(data, source, daylight.savings) {
  
  # Filter daylight savings time or standard time
  if (isTRUE(daylight.savings)) {
    data <- dplyr::filter(data, dst)
  } else if (!isTRUE(daylight.savings)) {
    data <- dplyr::filter(data, !dst)
  }
  
  # Compare source to NOAA
  if (source == "LakeMetabolizer") {
    rise <- data %>% dplyr::mutate(dif = noaa_rise - lm_rise)
    rise <- rise$dif
    units(rise) <- "mins"
    set <- data %>% dplyr::mutate(dif = noaa_set - lm_set)
    set <- set$dif
    units(set) <- "mins"
  } else if (source == "suncalc") {
    rise <- data %>% dplyr::mutate(dif = noaa_rise - sc_rise)
    rise <- rise$dif
    units(rise) <- "mins"
    set <- data %>% dplyr::mutate(dif = noaa_set - sc_set)
    set <- set$dif
    units(set) <- "mins"
  }
  
  # Return messages
  message("Sunrise difference:\n", 
          "  min: ", round(min(rise), digits = 2), "\n",
          "  max: ", round(max(rise), digits = 2), "\n",
          "  avg: ", round(mean(rise), digits = 2), "\n",
          "Sunset difference:\n", 
          "  min: ", round(min(set), digits = 2), "\n",
          "  max: ", round(max(set), digits = 2), "\n",
          "  avg: ", round(mean(set), digits = 2), "\n")
  
}

# Create test data (noaa = NOAA, lm = LakeMetabolizer, sc = suncalc)
d <- create_test_data()
head(d)

# Difference LakeMetabolizer and suncalc to NOAA (Rise/set times later than 
# NOAA result in negative values. Units = minutes.):

# LakeMetabolizer during standard time
compare_to_noaa(
  data = d, 
  source = "LakeMetabolizer", 
  daylight.savings = FALSE)

# LakeMetabolizer during daylight savings
compare_to_noaa(
  data = d, 
  source = "LakeMetabolizer", 
  daylight.savings = TRUE)

# suncalc during standard time
compare_to_noaa(
  data = d, 
  source = "suncalc", 
  daylight.savings = FALSE)

# suncalc during daylight savings
compare_to_noaa(
  data = d, 
  source = "suncalc", 
  daylight.savings = TRUE)

# Plot sun rise
rise <- dplyr::bind_rows(
  dplyr::rename(
    dplyr::mutate(
      dplyr::select(d, date, noaa_rise), 
      source = "NOAA"), 
    rise = "noaa_rise"),
  dplyr::rename(
    dplyr::mutate(
      dplyr::select(d, date, lm_rise), 
      source = "LakeMetabolizer"), 
    rise = "lm_rise"),
  dplyr::rename(dplyr::mutate(
    dplyr::select(
      d, date, sc_rise), 
    source = "suncalc"), 
    rise = "sc_rise"))
ggplot(rise, aes(x = date, y = hms::as.hms(rise), color = source)) + 
  geom_point() +
  scale_x_date(date_breaks = "months" , date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Sun rise in Chicago IL, tzone = America/Chicago")

# Plot sun set
set <- dplyr::bind_rows(
  dplyr::rename(
    dplyr::mutate(
      dplyr::select(d, date, noaa_set), 
      source = "NOAA"), 
    set = "noaa_set"),
  dplyr::rename(
    dplyr::mutate(
      dplyr::select(d, date, lm_set), 
      source = "LakeMetabolizer"), 
    set = "lm_set"),
  dplyr::rename(dplyr::mutate(
    dplyr::select(
      d, date, sc_set), 
    source = "suncalc"), 
    set = "sc_set"))
ggplot(set, aes(x = date, y = hms::as.hms(set), color = source)) + 
  geom_point() +
  scale_x_date(date_breaks = "months" , date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Sun set in Chicago IL, tzone = America/Chicago")
