require(sqldf)
filename1 <- "./trip_fare_3.csv"
filename2 <- "./trip_data_3.csv"
# 1
sql1 = 'select  payment_type, total_amount from file where  total_amount < 5' 
df1 = read.csv.sql(filename1, sql, sep=',',header = TRUE)

nrow(d1f[df1$payment_type == "CRD",])/nrow(df1)
# 0.08878

# 2
sql2 = 'select  payment_type, total_amount from file where total_amount > 50' 
df2 = read.csv.sql(filename1, sql2, sep=',',header = TRUE)

nrow(df2[df2$payment_type == "CRD",])/nrow(df2)
# 0.6816

# 3 

sql3_1 = 'select trip_time_in_secs from file'
df3_1 <- read.csv.sql(filename2, sql3_1, sep=',',header = TRUE)
sql3_2 = 'select total_amount from file'
df3_2 <- read.csv.sql(filename1, sql3_2, sep=',',header = TRUE)
index <- df3_1$trip_time_in_secs != 0
mean(df3_2$total_amount[index] * 60 / df3_1$trip_time_in_secs[index])

#1.712565

# 4
sql4 = 'select trip_distance from file'
df4 <- read.csv.sql(filename2, sql4, sep=',',header = TRUE)
index <- df4$trip_distance != 0
median(df3_2$total_amount[index] / df4$trip_distance[index] )
## 6

# 5
sql5 = 'select trip_distance,trip_time_in_secs from file'
df5<- read.csv.sql(filename2, sql5, sep=',',header = TRUE)
quantile(df5$trip_distance/df5$trip_time_in_secs * 3600, .95,na.rm = T)
## 26.67984 

# 6
sql6 = 'select trip_distance,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude from file'
df6 <- read.csv.sql(filename2, sql6, sep=',',header = TRUE)

index <- df6$trip_distance != 0 & 
        (df6$pickup_longitude < -71 & df6$pickup_longitude > -75) &
        (df6$dropoff_longitude < -71 & df6$dropoff_longitude > -75) &
        (df6$pickup_latitude < 42 & df6$pickup_latitude > 39) &
        (df6$dropoff_latitude < 42 & df6$dropoff_latitude > 39)
df7 <- df6[index,]

earth.dist <- function (long1, lat1, long2, lat2){
        rad <- pi/180
        a1 <- lat1 * rad
        a2 <- long1 * rad
        b1 <- lat2 * rad
        b2 <- long2 * rad
        dlon <- b2 - a2
        dlat <- b1 - a1
        a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
        c <- 2 * atan2(sqrt(a), sqrt(1 - a))
        R <- 6378.145
        d <- R * c * 0.621371192
        return(d)
}
dist <- earth.dist(df7$pickup_longitude,df7$pickup_latitude,df7$dropoff_longitude,df7$dropoff_latitude)
mean(dist/df7$trip_distance)

## 0.8119574
# 7 

sql7_1 <- 'select tip_amount from file'
sql7_2 <- 'select pickup_longitude,pickup_latitude from file'
df7_1 <- read.csv.sql(filename1, sql7_1, sep=',',header = TRUE)
df7_2 <- read.csv.sql(filename2, sql7_2, sep=',',header = TRUE)
df7 <- df7_1[df7_2$pickup_longitude > (-73.78 - .05) & df7_2$pickup_longitude < (-73.78 + .05) &
             df7_2$pickup_latitude > (40.65 - 0.05) & df7_2$pickup_latitude < (40.65 + 0.05),]
mean(df7)
## 4.475571

# 8
sql8_1 <- 'select hack_license,fare_amount, tip_amount from file'
df8_1 <- read.csv.sql(filename1,sql8_1,sep = ',',header = TRUE)
df8_1$revenue <- df8_1$fare_amount + df8_1$tip_amount
df8 <- tapply(df8_1$revenue, as.factor(df8_1$hack_license),sum)
median(df8)  
## 6708.19