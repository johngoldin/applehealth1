

#' Title
#'
#' @param d
#' @param timezone the timezone that d was expressed in originally
#'
#' @return
#' @export
#'
#' @examples
double_to_datetime <- function(d) {
  if (class(d) != "numeric") stop("input to d is not numeric")
  as.POSIXct(d, origin = lubridate::origin, tz = "UTC")
}

local_to_sys_time <- function(datetime, timezone) {
  # what it would be if saved as system timezone (which for me, is New York)
  tz(datetime) <- timezone
  xt <- with_tz(datetime, "UTC")
  xt <- with_tz(xt, Sys.timezone())
  tz(xt) <- "UTC"
  xt
}

local_to_utc <- function(dt, timezone) {
  # UTC of this local time. Note: not vectorized
  if (is.character(dt)) dt <- as_datetime(dt)
  tz(dt) <- timezone
  xt <- with_tz(dt, "UTC")
  tz(xt) <- "UTC"
  return(xt)
}

# For example, flight to Paris on 2014-10-08 leaves at
# 20:45 NY time, duration of flight is 6 hours 40 minutes. Therefore
# arrival is scheduled as 03:25 the next morning NY time which is
# 09:25 Paris time. 09:25 Paris time is  07:25 UTC.
# If Export times are all converted from UTC to
# eastern time, then the plane's arrival would appear to be
# 03:25 (same as above). So if I see a time in the data that
# is after 03:25 and (before the return to New York) it needs
# to be adjusted to local Paris time rather than to New York time.

# I will be applying this function to nearly a million times so it's important
# that it be vectorized and compiled. Vectorized is what really counts.

utc_dt_to_local <- function(dt, time_zone) {
  # Adjust a vector of datetime from UTC
  # to a particular time_zone that that applies to the whole vector.
  tz(dt) <- "UTC"
  local <- with_tz(dt, time_zone) # now adjust utc to the time zone I want
  tz(local) <- "UTC"
  # I mark the vector as UTC because I will be row_bind-ing vectors
  # together and all need to have the same time zone attribute.
  # Although the vector is marked as UTC,
  # in the end I will treat the hour as being whatever the local
  # time was that I experienced then.
  return(local)
}



raw_to_local <- function(datetime, timezone) {
  if (is.character((datetime))) datetime = ymd_hms(datetime)
  tz(datetime) <- Sys.timezone()
  # convert to UTC
  xt <-force_tz(datetime, "UTC", roll= TRUE)
  # convert to then local timezone
  xt <- force_tz(xt, timezone, roll = TRUE)
}

find_timezone <- function(time, arrivals) {
  arrivals <- arrivals %>% filter(timezone != Sys.timezone())
  tz <- rep(Sys.timezone(), length(time))

}

get_my_time_zone <- function(dt) {
  # What I'm going for is the time zone used by my watch.
  # I'm assuming my watch got the local clock time about the
  # same time as the scheduled arrival for my flight.
  time_zone <- case_when(
    (dt >= as_datetime("2018-01-31 16:00:00")) & # trip to RStudio conference
      (dt <= as_datetime("2018-02-07 13:01:00")) ~ "America/Los_Angeles",
    (dt >= as_datetime("2018-04-18 08:00:00")) & # trip to Amsterdam
      (dt <= as_datetime("2018-04-20 13:50:00")) ~  "Europe/Amsterdam",
    (dt >= as_datetime("2018-04-20 13:50:00")) & # trip to Athens
      (dt <= as_datetime("2018-04-30 15:52:00")) ~  "Europe/Athens",
    (dt >= as_datetime("2019-06-21 03:45:00")) & # trip to SW England
      (dt <= as_datetime("2019-07-05 13:25:00")) ~  "Europe/London",
    (dt >= as_datetime("2019-08-28 06:30:00")) & # trip to Manchester
      (dt <= as_datetime("2019-09-10 12:40:00")) ~  "Europe/London",
    (dt >= as_datetime("2020-01-28 20:52:00")) & # trip to RStudio in SF
      (dt <= as_datetime("2020-02-04 18:17:00")) ~  "America/San_Francisco",
    TRUE ~ "America/New_York" # good old Eastern time, home sweet home
  )
  return(time_zone)
}
get_my_time_zone <- compiler::cmpfun(get_my_time_zone)

