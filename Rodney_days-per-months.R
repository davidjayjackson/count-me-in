library(tidyverse)
library(data.table)
library(xts)
library(prophet)
library(plotly)
library(lubridate)
rm(list=ls())
##
## Begin SIDC daily numbers stuff
##
sidc <-fread("http://sidc.be/silso/DATA/SN_d_tot_V2.0.csv",sep = ';')
colnames(sidc) <- c("Year","Month","Day", "Fdate","Spots", "Sd","Obs" ,"Defin"  )
sidc$Ymd <- as.Date(paste(sidc$Year, sidc$Month, sidc$Day, sep = "-"))
sidc1<-sidc[Ymd>="1850-01-01",.(Ymd,Spots),]
sidc1$Year <- year(sidc1$Ymd)
sidc1$Month <- month(sidc1$Ymd)
sidc1$Vote <- ifelse(sidc1$Spots ==0,0,1)
Fit <- as.data.frame(lowess(sidc1$Spots,f=0.3))
sidc1 <-cbind(sidc1,Fit$y)
colnames(sidc1) <-c("Ymd","Spots","Vote","Loess")
str(sidc1)
##
## Create monthly summary (Vote) field with XTS
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
##s
## Select date range for plots and tables
##
S4 <- filter(forecast,ds >="2019-01-01" & ds <="2026-12-31")
S4 <- as.data.table(S4)
##
dcast.data.table(S4,Year~Month,value.var = "yhat")
##
ggplot(data=S4,aes(x=ds,y=yhat_upper,col="Upper")) +geom_col() +
  geom_col(data=S4,aes(x=ds,y=yhat,col="Predict")) +
  geom_col(data=S4,aes(x=ds,y=yhat_lower,col="Lower")) +
  xlab("Date: Month and Year") + ylab("Days Per Month") + 
  geom_smooth(data=S4,aes(x=ds,y=yhat,col="Loess"))
##
# Create a interactive plot.
# plot_ly(data=S4,x=~ds,y=~yhat,mode="lines")
##
## Create CSV file
forecast$Year <- year(forecast$ds)
forecast$Month <- month(forecast$ds)
S5 <- select(forecast,Year,Month,ds,yhat_upper,yhat,yhat_lower)
write.csv(S5,file="dayPerMonth.csv",row.names = F)


## Create "pivot table"
##
S3$Year <-year(S3$Ymd)
S3$Month <- month(S3$Ymd)
S3  %>% select(Year,Month,Days) %>% filter(Year >=2014) %>%
  pivot_wider(names_from = Month,values_from = Days)

##
## Pivot table for Sidc votes
sidc1 %>% select(Year,Month,Vote) %>% filter(Year >=2000 & Vote==1) %>% 
  pivot_wider(names_from=Month,values_from = Vote,values_fn = list(Vote = sum))
