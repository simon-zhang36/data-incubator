library(ggplot2)
filename <- './2014-Q2-Trips-History-Data2.csv'
data <- read.csv(filename,stringsAsFactors = FALSE)
data1 <- as.Date(strptime(data$Start.date,"%m/%d/%Y %H:%M"))

data1 <- as.data.frame(data1)
names(data1) <- c("date")

data1$weekday = weekdays(data1$date)
data1$month <- months(data1$date)

par(mfrow = c(1,3))
hist(data1$date,breaks = 90,col = 'green')

plot(table(data1$weekday),ylab = "count")
plot(table(data1$month),ylab = "count")



data3 <- as.data.frame(table(data$Start.Station))
names(data3) <- c("Start.Station","count")
data3 <- subset(data3,count > 5000)

ggplot(data3,aes(x=Start.Station,y=count))+ geom_bar(stat = "identity") + xlab("location")
