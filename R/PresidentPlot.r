# color the background of a plot to show the party of the president in office at that time

#library(lattice)
#library(latticeExtra)
#library(tidyverse)
#source("R/Presidents.R")
#' @export
PresidentPlot <- function(formula, data, inagurations = NULL, ...){
  if (is.null(inagurations)){
    inagurations <- Presidents()
  }
  p <- lattice::xyplot(as.formula(formula), data = data,
              panel=function(x,y, ...) {
                latticeExtra::panel.xblocks(x, getBackground(inagurations, x), alpha=0.2)
                #panel.text(presidents$InagurationDate, 80, presidents$President)
                lattice::panel.xyplot(x, y, ...)
              },
              ...
  )
  return (p)
}


#' draw a test plot
#' @export
testPresidentPlot <- function(){
  df <- data.frame("date" = seq(ISOdate(1850,1,1), ISOdate(2000,2,1), "months"))
  df$value <- sin(as.numeric(df$date)/1000000000)
  p<- PresidentPlot(value~date, df, main="test", xlab='date', ylab="abc")
  print(p)
}

#' draw a test plot
#' @export
testPresidentLogPlot <- function(){
  df <- data.frame("date" = seq(ISOdate(1850,1,1), ISOdate(2000,2,1), "months"))
  df$value <- sin(as.numeric(df$date)/1000000000) + 1.1
  p<- PresidentPlot(value~date, df, main="test", xlab='date'
                    ,scales=list(y=list(log=10))
                    ,yscale.components = latticeExtra::yscale.components.log10ticks)
  print(p)
}



