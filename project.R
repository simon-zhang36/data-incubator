filename <- './2014-Q2-Trips-History-Data2.csv'
data <- read.csv(filename,stringsAsFactors = FALSE)
data1 <- as.POSIXct(strptime(data$Start.date,"%m/%d/%Y %H:%M"),tz="","%Y-%m-%d")
