


#Get major and minor tick marks based on weeks
#major tick marks will be on weekday weekStart (0 = sunday)
#range will at least include first_weekday <= min(x) <= _last weekday

#' Calculate weekly tick marks
#' @export
weekly_ticks <- function(x, dayOfWeek, numMajorTicks = 3){


  #get the start and end dayOfWeeks
  minDate <- as.Date(min(x))
  leftMargin <- (as.POSIXlt(minDate)$wday - dayOfWeek)%%7
  minDate <- minDate - lubridate::days(leftMargin)

  maxDate <- as.Date(max(x))
  rightMargin <- (dayOfWeek - as.POSIXlt(maxDate)$wday)%%7
  maxDate <- maxDate + lubridate::days(rightMargin)

  weeks <- as.numeric(difftime(maxDate, minDate, units="weeks"))
  if (weeks < 1){
    minDate <- minDate - lubridate::days(y)
    maxDate <- maxDate + lubridate::days(y)
    weeks <- 2
  }

  numMajorTicks <- max(numMajorTicks, 2)
  rough_step <- max(1, weeks/numMajorTicks)
  magnitude <-floor(log10(rough_step))
  rough_step <- rough_step/(10^magnitude) #this is in range 1:10
  pretty_step <- 1
  if (rough_step > 5){
    pretty_step <- 10
  } else if (rough_step > 1){
    pretty_step = 2
  }
  pretty_step = pretty_step*(10^magnitude)
  majors <- seq(minDate, maxDate, by = pretty_step * 7)
  #print(majors)

  minor_step <- if (pretty_step < 2) 1 else 7
  minors <- seq(minDate, maxDate + lubridate::days(1), by= minor_step  )
  minors <- minors[!minors%in%majors]
  #print(minors)

  return (list(majors=majors, minors=minors))


}

test <- function(){

  s1 <- seq(ISOdate(2020, 4,1), by="hour", length.out=160)
  print(weekly_ticks(s1, 0,3)$majors)
  print(weekly_ticks(s1, 3,3)$majors)

  s1 <- seq(ISOdate(2019, 12,1), by="day", length.out=300)
  print(weekly_ticks(s1, 0,3)$majors)
  print(weekly_ticks(s1, 3,3)$majors)


  s1 <- seq(ISOdate(2020, 4,1), by="month", length.out=160)
  print(weekly_ticks(s1, 0,3)$majors)
  print(weekly_ticks(s1, 0,10)$majors)

}
