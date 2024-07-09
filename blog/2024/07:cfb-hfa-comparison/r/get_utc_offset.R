#get_utc_offset.R
get_utc_offset <- function(time_input, timezone_input){
  utc_time <- as.POSIXct(format(time_input, tz = "UTC"))
  comparison_time <- as.POSIXct(format(time_input, tz = timezone_input))
  timezone_offset <- as.numeric(difftime(comparison_time, utc_time, units = "hours"))
  return(timezone_offset)
}

get_utc_offset <- Vectorize(get_utc_offset)