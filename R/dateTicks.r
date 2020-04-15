#calc nice tick locations for date axis


#' Get step sizes that are  a nice multiple of 1, 2, 5, or 10
#' range is max - min
#' numSteps is the approximate number of steps desired in range
#' @export
nice_step_size <- function(range, numSteps){
  rough_step <- range/numSteps
  mag <- floor(log10(rough_step))
  scaled_step <-  ceiling(rough_step/(10^mag)) # in range 1:10
  if (scaled_step > 5){
    nice_step <- 10
  } else if (scaled_step > 2) {
    nice_step <- 5
  } else if (scaled_step >1){
    nice_step <- 2
  } else {
    nice_step <- 1
  }

  nice_step <- nice_step *(10^mag)
  return (nice_step)
}

# Get nice (multiples of 6,4,3,2,or 1 month) step sizes
nice_monthly_step_size <- function(months, numSteps){

  rough_step = ceiling(months/numSteps)
  if (rough_step > 5){
    nice_step <- ceiling(6 * nice_step_size(months/6, numSteps))
  } else if (rough_step > 4 ){
    nice_step <- 6
  } else {
    nice_step <- ceiling(rough_step)
  }
  return (nice_step)
}

#' Calculate daily tick marks
#' x, the data range to consider
#' numIntervals, the approximate number of intervals between tick marks
#' @export
daily_ticks <- function(x, numIntervals = 3 ){


  #get the start and end dayOfWeeks
  minDate <- as.Date(min(x, na.rm = T), tz="")

  maxDate <- as.Date(max(x, na.rm = T), tz="")
  maxDate <- maxDate + lubridate::days(1)

  days  <- as.numeric(difftime(maxDate, minDate, units="days"))
  if (days < 1){
    maxDate <- minDate + lubridate::days(1)
    days <- 1
  }

  numIntervals <- max(numIntervals, 2)

  nice_step = ceiling(nice_step_size(days, numIntervals))
  ticks <- seq(minDate, maxDate, by = nice_step)
  return(ticks)
}



#' Calculate weekly tick marks
#' x, the data range to consider
#' dayOfWeek, the day to tick on (0 = sunday)
#' numIntervals, the approximate number of intervals between tick marks
#' @export
weekly_ticks <- function(x, numIntervals = 3, dayOfWeek=0 ){


  #get the start and end dayOfWeeks
  minDate <- as.Date(min(x, na.rm = T), tz="")
  leftMargin <- (as.POSIXlt(minDate)$wday - dayOfWeek)%%7
  minDate <- minDate - lubridate::days(leftMargin)

  maxDate <- as.Date(max(x, na.rm = T), tz="")
  rightMargin <- (dayOfWeek - as.POSIXlt(maxDate)$wday)%%7
  maxDate <- maxDate + lubridate::days(rightMargin)

  weeks <- as.numeric(difftime(maxDate, minDate, units="weeks"))
  if (weeks < 1){
    maxDate <- minDate + lubridate::days(7)
    weeks <- 1
  }

  numIntervals <- max(numIntervals, 2)

  nice_step = ceiling(nice_step_size(weeks, numIntervals))
  ticks <- seq(minDate, maxDate, by = nice_step * 7)
  return(ticks)
}

#' Calculate monthly tick marks
#' x, the data range to consider
#' numIntervals, the approximate number of intervals between tick marks
#' @export
monthly_ticks <- function(x, numIntervals = 3){
  #x <-seq(ISOdate(2020, 4,1), by="day", length.out=5)
  #numIntervals <- 10

  numIntervals = max(1, numIntervals)

  minDate <- as.Date(min(x, na.rm = T), tz="")
  y = as.POSIXlt(minDate)$year + 1900
  m = as.POSIXlt(minDate)$mon + 1
  minDate <- ISOdate(y,m,1, tz="")

  maxDate <- as.Date(max(x, na.rm = T), tz="")
  y = as.POSIXlt(maxDate)$year + 1900
  m = as.POSIXlt(maxDate)$mon + 1
  #need to handle through end of day on maxDate
  if (m==12){
    y <- y+1
    m <- 1
  } else {
    m <- m+1
  }
  maxDate <- ISOdate(y,m,1, tz="")

  approx_months <- ceiling(as.numeric(maxDate-minDate)/30)

  nice_steps <- nice_monthly_step_size(approx_months, numIntervals)
  ticks <- seq(minDate, maxDate, by=sprintf("%d months", nice_steps))

  return (ticks)
}

#' Calculate tick marks for a date axis
#' x, the data range to consider
#' dayOfWeek, the day to tick on (0 = sunday) (only applies if weekly ticks)
#' numIntervals, the approximate number of intervals between tick marks
#' @export
date_ticks <- function(x, numIntervals = 3, weekStartDay = 0){
  range <- as.numeric(max(x, na.rm = T) - min(x, na.rm = T))
  rough_tick <- range/numIntervals #days
  if (rough_tick < 4){
    ticks <- daily_ticks(x,numIntervals)
  } else if (rough_tick < 40){
    ticks <- weekly_ticks(x, numIntervals,weekStartDay)
  } else {
    ticks <- monthly_ticks(x, numIntervals)
  }

  #try to return the same class as supplied since differences confuse xyplot
  if (class(x)[1] == "POSIXct"){
    return (as.POSIXct(ticks, tz=""))
  } else if (class(x)[1] == "Date"){
    return (as.Date(ticks))
  } else {
    return (ticks)
  }
}


test <- function(){


  print(nice_step_size(3,3))
  print(nice_step_size(3,30))

  print(nice_monthly_step_size(6,3))
  print(nice_monthly_step_size(0.2,3))
  print(nice_monthly_step_size(1300,12))


  s1 <- seq(ISOdate(2020, 4,1), by="hour", length.out=10)
  print(paste(min(s1), "...", max(s1)))
  print(monthly_ticks(s1,3))
  print(weekly_ticks(s1, 3,0))
  print(weekly_ticks(s1, 3,3))
  print(date_ticks(s1,3,0))
  print(date_ticks(s1,3,1))

  s1 <- seq(ISOdate(2019, 12,1), by="day", length.out=30)
  print(paste(min(s1), "...", max(s1)))
  print(monthly_ticks(s1,3))
  print(weekly_ticks(s1, 3,0))
  print(weekly_ticks(s1, 3,3))
  print(date_ticks(s1,3,0))
  print(date_ticks(s1,3,1))



  s1 <- seq(ISOdate(2020, 4,1), by="month", length.out=160)
  print(paste(min(s1), "...", max(s1)))
  print(monthly_ticks(s1,3))
  print(weekly_ticks(s1, 3,0))
  print(weekly_ticks(s1, 3,3))
  print(date_ticks(s1,3,0))
  print(date_ticks(s1,3,1))

  s1 <- seq(ISOdate(2020, 4,1), by="hour", length.out=10)
  weekly_ticks(s1, 3)
  monthly_ticks(s1,3)
  date_ticks(s1,3,0)


}



