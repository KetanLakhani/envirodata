library(XML)
library(methods)

##Enter dates and station ID here
ndates <- seq.Date(as.Date('1955-01-01'),as.Date('2012-01-01'),by = "month")
id <- 2205
norows <- length(ndates)*24*31

##Build up URLS for extraction
toNumerics <- function(Date) {
  stopifnot(inherits(Date, c("Date", "POSIXt")))
  day <- as.numeric(strftime(Date, format = "%d"))
  month <- as.numeric(strftime(Date, format = "%m"))
  year <- as.numeric(strftime(Date, format = "%Y"))
  list(year = year, month = month, day = day)
}

kl <- data.frame(toNumerics(ndates))

URLS <- paste0("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=xml&stationID=",
               id,
               "&Year=",
               kl$year,
               "&Month=",
               kl$month,
               "&Day=",
               kl$day,
               "&timeframe=1&submit=Download+Data")

URLS <- data.frame(URLS)
#URLxml <- "http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=xml&stationID=2205&Year=1955&Month=1&Day=1&timeframe=1&submit=Download+Data"
            

noURLS <- nrow(URLS)

###This fucntion changes rbind to combine columns even when there is missing data
rbind.all.columns <- function(x, y) {     
  x.diff <- setdiff(colnames(x), colnames(y))    
  y.diff <- setdiff(colnames(y), colnames(x))     
  x[, c(as.character(y.diff))] <- NA     
  y[, c(as.character(x.diff))] <- NA     
  return(rbind(x, y))}




#Create a matrix for input
master <- matrix(nrow = norows, ncol = 17)
master <- data.frame(master)

#j is the file number (also the URL number)
#Cycle through all URL's and get data
# Data will go into
# a for dates for each hour
# b for other data like temperature pressure etc for each hour
# ab to combine the 2 above
# master for all hours in a month
# Master for all Months in the timeframe

for(j in 1:noURLS) {
    #j <- 2
    download.file(as.character(URLS[j,]), destfile = "Test1.xml")
    library(XML)
    doc <- xmlTreeParse("Test1.xml")
    top <- xmlRoot(doc)
    result <- xmlParse(file = "Test1.xml")

    # Exract the root node form the xml file.
    rootnode <- xmlRoot(result)
    
    xml_data <- xmlToList(doc)
    
    
    xml_size <- xmlSize(rootnode)
    
          # i is the size of the file  
          for(i in 4:xml_size) {
            #i = 5
            a <- as.data.frame(t(xml_data[[i]]$.attrs))
            b <- as.data.frame(t(xmlSApply(rootnode[[i]], xmlValue)))
            ab <- cbind(a,b)
            ifelse(i==4 ,master <- rbind(ab), master <- rbind.all.columns(master, ab))
            }
    
    ifelse(j==1 , Master <- rbind(master), Master <- rbind(Master, master))
}

write.csv(Master, "DatafileWeather.csv")

