library(tidyverse)
library(data.table)
library(xts)
<<<<<<< HEAD
library(prophet)
=======
>>>>>>> 1f5e476b3457ae9f7febd0e6613f40166b6129b7
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
## Create monthly summary field with XTS
##
isn.xts <- xts(x = sidc1$Vote, order.by = sidc1$Ymd)
isn.monthly <- apply.monthly(isn.xts, sum)
isn <-as.data.table(isn.monthly)
colnames(isn) <- c("Ymd","Days")
D <- isn %>% filter(Ymd >="2014-01-01")
ggplot(data=D,aes(x=Ymd,y=Days)) +geom_line() +geom_smooth(method="loess",col="blue") +
    ggtitle("Monthly Mean Days with Sunspots: 2014-2019")
##
## Create monthly mean Sunpots
##
isn.xts <- xts(x = sidc1$Spots, order.by = sidc1$Ymd)
isn.monthly <- apply.monthly(isn.xts, mean)
monthly_mean <-as.data.table(isn.monthly)
colnames(monthly_mean) <- c("Ymd","Spots")
E <- monthly_mean %>% filter(Ymd >="2014-01-01")
ggplot(data=E,aes(x=Ymd,y=Spots)) +geom_line() +geom_smooth(method="loess",col="blue") +
    ggtitle("Monthly Rolling Average( Sunspots): 2014-2019")

<<<<<<< HEAD
##
## Prediction based on number of days per month
isn.xts <- xts(x = sidc1$Vote, order.by = sidc1$Ymd)
isn.monthly <- apply.monthly(isn.xts, sum)
isn <-as.data.table(isn.monthly)
colnames(isn) <- c("ds","y")
##
m <- prophet(seasonality.mode="multiplicative")
m <- add_seasonality(m, name="cycle_11year", period=365.25 * 11,fourier.order=5)
m <- fit.prophet(m, isn)
future <- make_future_dataframe(m,periods=8000,freq="day")
forecast <- predict(m, future)
plot(m,forecast) +ggtitle("SIDC: Predict  Days with Sunspots per Month")
##
## Current min. 2014

current_min <- forecast %>% filter(ds >="2014-01-01")
ggplot(data=current_min,aes(x=ds,y=yhat)) +geom_line() + 
  geom_line(data=current_min,aes(x=ds,y=yhat_lower,col="Lower")) +
  geom_line(data=current_min,aes(x=ds,y=yhat_upper,col="Upper")) 
  
=======
>>>>>>> 1f5e476b3457ae9f7febd0e6613f40166b6129b7
