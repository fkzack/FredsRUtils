#An example of creating xyplots with grid aligned with tick marks
#Works for log10 plots, date plots with "at" explicitly set in scales, and with ordinary plots


#library(lattice)
#library(latticeExtra)
#library(lubridate)

#rm(list=ls())

#' Calculate tick locations that will work nicely with log 10 grids
#' returns a list of major tick locations and minor tick locations,
#' $majors and $minors
log_ticks_old <- function(log_range){

  #cat("log_ticks_old from", log_range[1], "to", log_range[2], "\n")

  #locate major ticks as range of integer powers of 10 inside data limits
  lowest <- ceiling(log_range[1])
  highest <- floor(log_range[2])
  step <- 1

  #cat("lowest", lowest, ", highest", highest, ", step", step, "\n")

  #majors are the log value of the major ticks
  majors <- seq(lowest, highest, step)
  #cat("majors (actual plotted value):", majors, "\n")
  #cat("10^majors (value as labeled on axis):", 10^majors, "\n")

  #minor ticks can go outside data limits, but no further than next decade
  lowest <- floor(log_range[1])
  highest <- ceiling(log_range[2])
  minor_limits <- seq(lowest, highest, step)
  #cat("minor limits (actual plotted value):", minor_limits,"\n")

  #minor grid lines are at 2x, 3x,.. 9x each power of 10
  minors <- log10(seq(2,9) %o% 10^minor_limits)
  #cat("minors:", minors, "\n")
  #cat("10^minors  (value as labeled on axis):", 10^minors, "\n")
  return (list(majors=majors, minors=minors))
}


#' add grid(s) to xyplots
#' Treats date axis and log10 axis as special cases
#' For log10 axis:
#'   Major grids are placed at integral powers of 10 inside the data range
#'   Minor grids are placed at integral multiples of each power of 10, e.g. 1,2,...9,10,20, 30, ....
#'   Minor grids may extend past end of data
#' For date axis
#'   Grids added at each of the "at" points for each scale.
#'   This requires that the orignal plot be created with explicit data ticks, e.g.
#'   scales = list(x=list(at=as.Date(pretty_dates(df$date, 5)), rot=45))
#'
#' Always draws major grid lines
#' Draws minor grid lines if MinorLines is TRUE
#' @export
addGrid <- function (p, MinorLines=TRUE) {
  p1 <- update (
    p,
    panel = function(x, y, ...) {
      #data limits in log coordinates
      #cat("limits", p$y.limits, "\n")
      #cat("x", x, "\n")
      #cat("y", y, "\n")

      if (p$x.scales$log == 10) {
        ticks <- log_ticks_old(p$x.limits)
        lattice::panel.abline(v = ticks$majors, alpha = 0.15)
        lattice::panel.grid(v=0)
        if (MinorLines) {
          lattice::panel.abline(v = ticks$minors, alpha = 0.08)
        }
      } else if (p$x.scales$at[1])   {
        lattice::panel.abline(v = p$x.scales$at, alpha = 0.15)
      } else {
        #print("using default grid x")
        lattice::panel.grid(v = -1)
      }

      if (p$y.scales$log == 10) {
        ticks <- log_ticks_old(p$y.limits)
        #print("ticks y:")
        #print(ticks)
        lattice::panel.abline(h = ticks$majors, alpha = 0.15)
        lattice::panel.grid(h=0, v=0)
        if (MinorLines){
          lattice::panel.abline(h = ticks$minors, alpha = 0.08)
        }
      } else if (p$y.scales$at[1]) {
        #print(p$y.scales$at)
        lattice::panel.abline(h = p$y.scales$at, alpha = 0.15)
      } else {
        #print("using default grid y")
        lattice::panel.grid(h = -1)
      }
      lattice::panel.xyplot(x, y,  ...)
    }
  )
  return(p1)
}

# addGrid <- function (p) {
#     #data limits in log coordinates
#     cat("limits", p$y.limits, "\n")
#     #cat("x", x, "\n")
#     #cat("y", y, "\n")
#
#     if (p$x.scales$log == 10) {
#       ticks <- log_ticks_old(p$x.limits)
#       p <- p +
#         panel.abline(v = ticks$majors, alpha = 0.15)
#         panel.abline(v = ticks$minors, alpha = 0.08)
#     } else if (p$x.scales$at[1])   {
#       p <- p + panel.abline(v = p$x.scales$at, alpha = 0.15)
#     } else {
#       print("using default grid x")
#       p <- p + panel.grid(v = -1)
#     }
#
#     if (p$y.scales$log == 10) {
#       ticks <- log_ticks_old(p$y.limits)
#       p <- p +
#         panel.abline(h = ticks$majors, alpha = 0.15)
#         panel.abline(h = ticks$minors, alpha = 0.08)
#     } else if (p$y.scales$at[1]) {
#       print(p$y.scales$at)
#       p <- p + panel.abline(h = p$y.scales$at, alpha = 0.15)
#     } else {
#       print("using default grid y")
#       p <- p + panel.grid(h = -1)
#     }
#
#
# return(p)
# }


#' plot a bunch of test plots with grids
#' @export
testLogPlotGrids <- function(){

  #generate some data for testing

  tens <- 10 ^ seq(-1, 2)
  df <- data.frame(sort(c(tens %o% seq(1:9))))
  names(df) <- 'x1'
  df$y2 <- df$x1 ^ 2
  df$date <- seq(as.Date("2001-01-07"),
                 by = "week",
                 along.with = df$x1)
  df$date2 <- seq(ISOdate(2020, 01, 07),
                  by = "hour",
                  along.with = df$x1)

  #test plots without grid
  px <- lattice::xyplot(
    y2 ~ x1,
    data = df,
    grid = FALSE,
    type = 'p',
    main = "logpower x",
    scales = list(x = list(log = 10)),
    xscale.components = latticeExtra::xscale.components.logpower,
  )
  print(px)

  py <- lattice::xyplot(
    y2 ~ x1,
    data = df,
    type = 'p',
    grid = FALSE,
    main = "log10ticks y",
    scales = list(y = list(log = 10)),
    yscale.components = latticeExtra::yscale.components.log10ticks
  )
  print(py)

  pxy <- lattice::xyplot(
    y2 ~ x1,
    data = df,
    grid = FALSE,
    type = 'p',
    main = "logpower x log10ticks y",
    scales = list(x = list(log = 10), y = list(log = 10)),
    xscale.components = latticeExtra::xscale.components.logpower,
    yscale.components = latticeExtra::yscale.components.log10ticks
  )
  print (pxy)

  pxy2 <- lattice::xyplot(
    y2 ~ x1,
    data = df,
    type = 'p',
    grid = FALSE,
    main = "xy no scale comps",
    scales = list(x = list(log = 10), y = list(log = 10)),
  )
  print (pxy)

  #XXXXXXX add a date tick method to get nice major and minor ticks for a date axis
  pdate1 <- lattice::xyplot(
    y2 ~ date,
    data = df,
    type = 'p',
    grid = FALSE,
    main = "date x log10ticks y explicit date at",
    scales = list(y = list(log = 10),
                  x = list(
                    at = as.Date(lubridate::pretty_dates(df$date, 5)), rot = 45
                  )),
    yscale.components = latticeExtra::yscale.components.log10ticks
  )
  print(pdate1)

  pdate2 <- lattice::xyplot(
    y2 ~ date,
    data = df,
    type = 'p',
    grid = FALSE,
    main = "date x log10ticks y, default date at",
    scales = list(y = list(log = 10)),
    yscale.components = latticeExtra::yscale.components.log10ticks
  )
  print(pdate2)

  pdate3 <- lattice::xyplot(date2 ~ date, data = df,
                   main = "date x date y, default date at")
  print(pdate3)


  #xyplot handles dates badly by default, so set them manually before
  #adding grids
  pdate4 <- lattice::xyplot(
    date2 ~ date,
    data = df,
    type = 'p',
    main = "date x date y, explicit date at",
    scales = list(
      y = list(at = lubridate::pretty_dates(df$date2, 5), rot = 45),
      x = list(at = as.Date(lubridate::pretty_dates(df$date, 5)), rot =
                 45)
    )
  )
  print(pdate4)

  print(addGrid(pdate1,F))
  print(addGrid(pdate2, F))
  #print(addGrid(pdate3))
  #print(addGrid(pdate4))

  #print(addGrid(px))
  #print(addGrid(py))
  #print(addGrid(pxy))
  #print(addGrid(pxy2))

}

# testLogPlotGrids()
