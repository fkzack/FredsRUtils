#library(methods)
#'a simple S4 class containing presidential inaguration dates, party, party color, and a few getters
#' @export
Presidents <- setClass("Presidents", slot= c(inagurationDates = "data.frame"))


Presidents <- function() {
  #init with data manually copied from a wikipedia table
  inagurations <- tibble::tribble(
    ~InagurationDate,~President,~PoliticalParty,
    "1789-04-30","George Washington", "None",
    "1797-03-04","John Adams", "Federalist",
    "1801-03-04","Thomas Jefferson", "Democratic-Republican",
    "1809-03-04","James Madison", "Democratic-Republican",
    "1817-03-04","James Monroe", "Democratic-Republican",
    "1825-03-04","John Quincy Adams", "Democratic-Republican",
    "1829-03-04","Andrew Jackson", "Democrat",
    "1837-03-04","Martin Van Buren", "Democrat",
    "1841-03-04","William Henry Harrison", "Whig",
    "1841-04-06","John Tyler", "Whig",
    "1845-03-04","James K. Polk", "Democrat",
    "1849-03-05","Zachary Taylor", "Whig",
    "1850-07-10","Millard Fillmore", "Whig",
    "1853-03-04","Franklin Pierce", "Democrat",
    "1857-03-04","James Buchanan", "Democrat",
    "1861-03-04","Abraham Lincoln", "Republican",
    "1865-04-15","Andrew Johnson", "National Union",
    "1869-03-04","Ulysses S. Grant", "Republican",
    "1877-03-05","Rutherford B. Hayes", "Republican",
    "1881-03-04","James A. Garfield", "Republican",
    "1881-09-20","Chester A. Arthur", "Republican",
    "1885-03-04","Grover Cleveland", "Democrat",
    "1889-03-04","Benjamin Harrison", "Republican",
    "1893-03-04","Grover Cleveland", "Democrat",
    "1897-03-04","William McKinley", "Republican",
    "1901-09-14","Theodore Roosevelt", "Republican",
    "1909-03-04","William Howard Taft", "Republican",
    "1913-03-04","Woodrow Wilson", "Democrat",
    "1921-03-04","Warren G. Harding", "Republican",
    "1923-08-03","Calvin Coolidge", "Republican",
    "1929-03-04","Herbert Hoover", "Republican",
    "1933-03-04","Franklin Roosevelt", "Democrat",
    "1945-04-12","Harry S. Truman", "Democrat",
    "1953-01-20","Dwight D. Eisenhower", "Republican",
    "1961-01-20","John F. Kennedy", "Democrat",
    "1963-11-22","Lyndon B. Johnson", "Democrat",
    "1969-01-20","Richard M. Nixon", "Republican",
    "1974-08-09","Gerald Ford", "Republican",
    "1977-01-20","Jimmy Carter", "Democrat",
    "1981-01-20","Ronald Reagan", "Republican",
    "1989-01-20","George Bush", "Republican",
    "1993-01-20","Bill Clinton", "Democrat",
    "2001-01-20","George W. Bush", "Republican",
    "2009-01-20","Barack Obama", "Democrat",
    "2017-01-20","Donald Trump", "Republican",
  )
  inagurations$InagurationDate <- as.POSIXct(inagurations$InagurationDate)
  #add party colors
  inagurations$BackColor = "green"
  inagurations$BackColor = ifelse(inagurations$PoliticalParty=="Democrat", 'blue', inagurations$BackColor)
  inagurations$BackColor = ifelse(inagurations$PoliticalParty=="Republican", 'red', inagurations$BackColor)
  new("Presidents", inagurationDates = as.data.frame(inagurations))
}

#Get the background color to use for presidential party on date
setGeneric(name="getBackground", def=function(theObject, date){standardGeneric("getBackground")})
setMethod(f="getBackground", signature = "Presidents", definition = function(theObject, date)
{
  col = 'yellow'
  interval <- findInterval(as.POSIXct(date), theObject@inagurationDates$InagurationDate)
  if (interval[1] > 0){
    col = theObject@inagurationDates$BackColor [interval]
  }
  return (col)
}
)

#Get the president's name  on date
setGeneric(name="getName", def=function(theObject, date){standardGeneric("getName")})
setMethod(f="getName", signature = "Presidents", definition = function(theObject, date)
{
  col <- theObject@inagurationDates$President [findInterval(as.POSIXct(date), theObject@inagurationDates$InagurationDate)]
  return (col)
}
)

#Get the president's party  on date
setGeneric(name="getParty", def=function(theObject, date){standardGeneric("getParty")})
setMethod(f="getParty", signature = "Presidents", definition = function(theObject, date)
{
  col <- theObject@inagurationDates$PoliticalParty [findInterval(as.POSIXct(date), theObject@inagurationDates$InagurationDate)]
  return (col)
}
)

