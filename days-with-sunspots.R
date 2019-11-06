library(tidyverse) 
library(data.table)
library(xts)

## Basic Plot
# ggplot(data=NOAA,aes(x=Ymd,y=Ra)) + geom_col()
A <- as.data.frame(lowess(NOAA$Ra,f=.2))
B <-cbind(NOAA,A)
###
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
##
## Begin SIDC daily numbers stuff
##
sidc <-fread("http://sidc.be/silso/DATA/SN_d_tot_V2.0.csv",sep = ';')
colnames(sidc) <- c("Year","Month","Day", "Fdate","Spots", "Sd","Obs" ,"Defin"  )
sidc$Ymd <- as.Date(paste(sidc$Year, sidc$Month, sidc$Day, sep = "-"))
sidc1<-sidc[Ymd>="1850-01-01",.(Ymd,Spots),]
sidc1$Vote <- ifelse(B$Ra ==0,0,1)
A <- as.data.frame(lowess(sidc1$Spots,f=.2))
B <-cbind(sidc1,A)
##
## Create monthly summary field with XTS
isn.xts <- xts(x = sidc1$Vote, order.by = sidc1$Ymd)
isn.monthly <- apply.monthly(isn.xts, sum)
isn <-as.data.table(isn.monthly)
D <- sidc1 %>% filter(Ymd >="2014-01-01")
ggplot(data=D,aes(x=Ymd,y=Ra)) +geom_line() +geom_smooth(method="loess")
