library(tidyverse)
library(data.table)
library(xts)
library(prophet)
rm(list=ls())
##
## Begin SIDC daily numbers stuff
##
sidc <-fread("http://sidc.be/silso/DATA/SN_d_tot_V2.0.csv",sep = ';')
colnames(sidc) <- c("Year","Month","Day", "Fdate","Spots", "Sd","Obs" ,"Defin"  )
sidc$Ymd <- as.Date(paste(sidc$Year, sidc$Month, sidc$Day, sep = "-"))
sidc1<-sidc[Ymd>="1850-01-01",.(Ymd,Spots),]
sidc1$Vote <- ifelse(sidc1$Spots ==0,0,1)
Fit <- as.data.frame(lowess(sidc1$Spots,f=0.3))
sidc1 <-cbind(sidc1,Fit$y)
colnames(sidc1) <-c("Ymd","Spots","Vote","Loess")
str(sidc1)
##
## Quick PLot of  Mens and Leoss for 1850 to 2019
ggplot(data=sidc1,aes(x=Ymd,y=Spots,col="Sidc")) + geom_line() +
  geom_line(data=sidc1,aes(x=Ymd,y=Loess,col="Loess")) + ggtitle("SIDC Daily Mean: 1850 - 2019")
##
## Current Solar Miniumn: 2014 - Oct. 2019
##
sidc2 <-  sidc1 %>% filter(Ymd >="2014-01-01") 
ggplot(data=sidc2,aes(x=Ymd,y=Spots,col="Sidc")) + geom_line() + 
  geom_line(data=sidc2,aes(x=Ymd,y=Loess,col="Loess")) + ggtitle("Current Solar Min. 2014-2019")
##
## Prophet prediction based on Daily Vote Field
##
df <- sidc1 %>% select(Ymd,Vote)
colnames(df) <- c("ds","y")
m <- prophet(seasonality.mode="multiplicative")
m <- add_seasonality(m, name="cycle_11year", period=364.25 * 11,fourier.order=5)
m <- fit.prophet(m, df)
future <- make_future_dataframe(m,periods=4000,freq="day")
forecast <- predict(m, future)
plot(m, forecast) +ggtitle("SIDC Monthy Votes: Jan. 1850 - Oct. 2019") +ylab("Predicted Mean Wolf(Ra)") +
  xlab("Years" )
##
S2 <- forecast %>% select(ds,yhat,yhat_lower,yhat_upper) %>% filter(ds >="2019-01-01")
ggplot(data=S2,aes(x=ds,y=yhat)) +geom_line()
##
##
## Create monthly summary field with XTS
##
isn.xts <- xts(x = sidc1$Vote, order.by = sidc1$Ymd)
isn.monthly <- apply.monthly(isn.xts, sum)
isn <-as.data.table(isn.monthly)
colnames(isn) <- c("Ymd","Days")
S3 <- isn %>% filter(Ymd >="1850-01-01")
ggplot(data=S3,aes(x=Ymd,y=Days)) +geom_line() +geom_smooth(method="loess",col="blue") + 
  ggtitle(" XTS: Monthy Days with Spots: 1850 - 2019")
##
## Prophet prediction based on Daily Vote Field
##
df <- S3 %>% select(Ymd,Days)
colnames(df) <- c("ds","y")
m <- prophet(seasonality.mode="multiplicative")
m <- add_seasonality(m, name="cycle_11year", period=364.25 * 11,fourier.order=5)
m <- fit.prophet(m, df)
future <- make_future_dataframe(m,periods=8000,freq="day")
forecast <- predict(m, future)
plot(m, forecast) +ggtitle("SIDC Monthy Votes: Jan. 1850 - Oct. 2019") +ylab("Predicted Days w/ Spots") +
  xlab("Years" )
##
S4 <- forecast %>% filter(ds >="2019-01-01") 
ggplot(data=S4,aes(x=ds,y=yhat)) +geom_col()