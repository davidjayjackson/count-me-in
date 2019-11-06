library(tidyverse) 
library(data.table)
library(xts)

NOAA <- read.csv("https://www.aavso.org/sites/default/files/solar/NOAAfiles/daily%20%281%29.csv")
NOAA$Ymd <- as.Date(paste(NOAA$Year, NOAA$Month, NOAA$Day, sep = "-"))
NOAA <- NOAA %>% filter(Year >=1945) %>% select(Ymd,Ra)
summary(NOAA)
## Basic Plot
# ggplot(data=NOAA,aes(x=Ymd,y=Ra)) + geom_col()
A <- as.data.frame(lowess(NOAA$Ra,f=.2))
B <-cbind(NOAA,A)

ggplot(data=B,aes(x=Ymd,y=Ra,col="Ra")) + geom_line() +
  geom_line(data=B,aes(x=Ymd,y=y,col="Y"))
  B$Vote <- ifelse(B$Ra ==0,0,1)
B <- B %>% filter(B$Vote==1)
#
# Create monthly summary field with XTS
isn.xts <- xts(x = NOAA$Vote, order.by = NOAA$Ymd)
isn.monthly <- apply.monthly(isn.xts, sum)
isn <-as.data.table(isn.monthly)
D <- NOAA %>% filter(Ymd >="2005-01-01" & Ymd <="2014-01-01" )
ggplot(data=D,aes(x=Ymd,y=Ra)) +geom_line() +geom_smooth(method="loess")

isn.xts <- xts(x = NOAA$Vote, order.by = NOAA$Ymd)
isn.monthly <- apply.monthly(isn.xts, sum)
isn <-as.data.table(isn.monthly)
D <- NOAA %>% filter(Ymd >="2005-01-01" & Ymd <="2014-01-01" )
ggplot(data=D,aes(x=Ymd,y=Ra)) +geom_line() +geom_smooth(method="loess")
