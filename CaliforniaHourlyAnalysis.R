#CA Hourly Study
rm(list = ls())

library(readxl)
library(ggplot2)
library(stats)
library(dplyr)
library(corrplot)
library(quantreg)
library(binhf)
library(dyn)
library(car)
library(tidyr)
library(MASS)


setwd("C:/Users/dholder/Desktop/Practice")
#load("Cal hourly full.Rda")


#the na.lomf function is pretty badass - it replaces an NA value with the last observed value from the column
na.lomf <- function(x) {
  
  na.lomf.0 <- function(x) {
    non.na.idx <- which(!is.na(x))
    if (is.na(x[1L])) {
      non.na.idx <- c(1L, non.na.idx)
    }
    rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
  }
  
  dim.len <- length(dim(x))
  
  if (dim.len == 0L) {
    na.lomf.0(x)
  } else {
    apply(x, dim.len, na.lomf.0)
  }
}

wkdy <- function(x){
  if(x=="Monday"|x=="Tuesday"|x=="Wednesday"|x=="Thursday"|x=="Friday") 1 else 0
  }
  
  

#this file originally came from data done in the "ISO Demand.R" workbook file
#this is hourly renewable generation data
load(file="California Hourly Data.Rda")


#bringing in the hourly load data
load <- read_excel("Power Demand data - SNL.xlsx", sheet="hourly", skip=5)
load$date <- as.POSIXct(load$date)
load$date <-format(load$date,"%Y-%m-%d")
load <- load[c(1,2,3,9)]
a$sramp <- a$solar - lag(a$solar, 1)
a$wramp <- a$wind - lag(a$wind, 1)
b <-left_join(a, load)

#bringing in the daily average price - this will be important for the aggregation step later.
price <- read_excel("Power Demand data - SNL.xlsx", sheet="prices", skip=3)
cprice <- price[c(3,9,11)]
cprice[, 3]  <- as.numeric(unlist(cprice[, 3]))
cprice$SOCAL <- na.lomf(cprice$SOCAL)
names(cprice) <- c("date", "sp15dayprice","socal")
cprice$date <- as.POSIXct(cprice$date)
cprice$date <-format(cprice$date,"%Y-%m-%d")


b <-left_join(b, cprice)


#-----
#this is getting generation data from all other sources
names <- c(1:7)
ca <- read.table(textConnection(""), col.names = names) 
date <- seq(as.Date("2010/4/20"), as.Date(Sys.Date()-1), "days")
date_td <- format(date, format="%Y%m%d")

for(i in date_td) {
  
  url <- paste("http://content.caiso.com/green/renewrpt/",i,"_DailyRenewablesWatch.txt", sep = "")
  carenew <- try(read.table(url, skip = 30, nrow = 24, sep = "", fill=TRUE), silent=FALSE)
  #the data breaks into Solar PV and Solar Thermal on 12/01/2012
  if('try-error' %in% class(carenew)) next
  else
    carenew$date <- as.Date(i, format="%Y%m%d")
  if(ncol(ca)==ncol(carenew)){
    ca <-rbind(ca,carenew)
  }else{
    next
  }
  
  
}


ca[1:6] <-as.data.frame(sapply(ca[,1:6], as.integer))


names(ca) <- c( "hour", "renewables","nuclear", "thermal", "imports", "hydro", "date")

str(b)
str(ca)
summary(b)

ca$date <- as.POSIXct(ca$date)
ca$date <-format(ca$date,"%Y-%m-%d")

ca$hour <- ca$hour-1
save(ca, file="CAISO hourly generation mix part 2.Rda")
b <-left_join(b,ca)

b <-filter(b, SP15hr >-250)
b <-filter(b, SP15hr <250)
b$month <- as.numeric(format(b$time, "%m"))
b$year <- as.numeric(format(b$time, "%Y"))
b$date <-as.Date(b$date)
b$wkdy <-  factor(weekdays(b$date))
b$wkdy <- sapply(b$wkdy, wkdy)
c <- filter(b, year > 2012)
eight <- filter(c, hour==8)
one <- filter(c, hour==13)
six <- filter(c, hour==18)
seven <-filter(c, hour==19)
mar21 <-filter(c, date=="2016-3-21")
mar22 <-filter(c, date=="2016-3-22")
mar23 <-filter(c, date=="2016-3-23")
mar24 <-filter(c, date=="2016-3-24")
mar25 <-filter(c, date=="2016-3-25")
mar26 <-filter(c, date=="2016-3-26")
mar27 <-filter(c, date=="2016-3-27")
mar21$two2 <-mar22$sramp
mar21$two3 <-mar23$sramp
mar21$two4 <-mar24$sramp
mar21$two5 <-mar25$sramp
mar21$two6 <-mar26$sramp
mar21$two7 <-mar27$sramp
jun21 <-filter(c, date=="2016-6-21")
sep21 <-filter(c, date=="2015-9-21")
dec21 <-filter(c, date=="2015-12-21")
mar21$jun21 <-jun21$sramp
mar21$sep21 <-sep21$sramp
mar21$dec21 <-dec21$sramp



#creating a dataset that only looks at hours where solar is ramping down significantly (by that I mean more than 1 GW in an hour)
rdown<-filter(c, sramp <= -1000)
rsix <-filter(rdown, hour==18)

save(b, file="Cal hourly full.Rda")



#END DATA MANIPULATION

#Statistics
summary(b)
summary(eight)
summary(one)
summary(six)

#charts
#the document "California hourly output and analysis" currently saved in my 'practice' folder has the relevant charts printed and organized by hour of focus

ggplot(c, aes(x=date, y=SP15hr, color=factor(year))) + geom_point()



ggplot(c, aes(x=date, y=solar, color=factor(hour))) + geom_point()
ggplot(six, aes(x=date, y=SP15hr, color=factor(year))) + geom_point()
ggplot(six, aes(x=date, y=wramp, color=factor(month))) + geom_point()
ggplot(six, aes(x=date, y=sramp, color=factor(month))) + geom_point()
ggplot(one, aes(x=date, y=sramp, color=factor(month))) + geom_point()
ggplot(one, aes(x=date, y=wramp, color=factor(month))) + geom_point()
ggplot(eight, aes(x=date, y=sramp, color=factor(month))) + geom_point()
ggplot(eight, aes(x=date, y=wramp, color=factor(month))) + geom_point()
ggplot(eight, aes(x=date, y=wind, color=factor(month))) + geom_point()
ggplot(eight, aes(x=date, y=solar, color=factor(month))) + geom_point()
ggplot(seven, aes(x=date, y=sramp, color=factor(month))) + geom_point()
ggplot(seven, aes(x=date, y=wramp, color=factor(month))) + geom_point()
ggplot(mar21, aes(x=hour, y=sramp)) + geom_point()
ggplot(jun21, aes(x=hour, y=sramp)) + geom_point()
ggplot(sep21, aes(x=hour, y=sramp)) + geom_point()
ggplot(dec21, aes(x=hour, y=sramp)) + geom_point()


ggplot(mar21, aes(hour)) + 
  geom_point(aes(y = sramp, colour = "21")) + 
  geom_point(aes(y = two2, colour = "22")) +
  geom_point(aes(y = two3, colour = "23")) +
  geom_point(aes(y = two4, colour = "24")) +
  geom_point(aes(y = two5, colour = "25")) +
  geom_point(aes(y = two6, colour = "26")) +
  geom_point(aes(y = two7, colour = "27")) +
  labs(title='solar ramping around the Spring equinox')


ggplot(mar21, aes(hour)) + 
  geom_point(aes(y = sramp, colour = "Mar21")) + 
  geom_point(aes(y = jun21, colour = "June21")) +
  geom_point(aes(y = sep21, colour = "Sep21")) +
  geom_point(aes(y = dec21, colour = "Dec21")) +
    labs(title='solar ramping during the solstice/equinox')


ggplot(mar21, aes(hour)) + 
  geom_line(aes(y = sramp, colour = "Mar21")) + 
  geom_line(aes(y = jun21, colour = "June21")) +
  geom_line(aes(y = sep21, colour = "Sep21")) +
  geom_line(aes(y = dec21, colour = "Dec21")) +
  labs(title='solar ramping')


ggplot(eight,aes(x=sramp, y=SP15hr ) )+geom_point() +geom_smooth(method = "lm") + labs(title='Eight AM Power Price and Solar Ramp')

ggplot(eight,aes(x=sramp, y=SP15hr, color=factor(year) ) )+geom_point() +geom_smooth(method = "lm")

ggplot(one,aes(x=sramp, y=SP15hr ) )+geom_point() +geom_smooth(method = "lm") + labs(title='One PM Power Price and Solar Ramp')
ggplot(one,aes(x=sramp, y=SP15hr, color=factor(year) ) )+geom_point() +geom_smooth(method = "lm")

ggplot(six,aes(x=sramp, y=SP15hr ) )+geom_point() +geom_smooth(method = "lm") + labs(title='Six PM Power Price and Solar Ramp')
ggplot(six,aes(x=sramp, y=SP15hr, color=factor(year) ) )+geom_point() +geom_smooth(method = "lm")

ggplot(six, aes(SP15hr)) + 
  geom_point(aes(y = sramp, colour = "Solar Ramp")) + 
  geom_point(aes(y = solar, colour = "Solar Gen")) +
  labs(title='Soar Vs Power price') 

ggplot(six,aes(x=sramp, y=solar ) )+geom_point() +geom_smooth(method = "lm") + labs(title='Six PM Solar gen and Solar Ramp')


ggplot(eight,aes(x=sramp, y=solar ) )+geom_point() +geom_smooth(method = "lm") + labs(title='Six PM Solar gen and Solar Ramp')
ggplot(eight,aes(x=sramp, y=solar , color=factor(year)) )+geom_point() +geom_smooth(method = "lm") + labs(title='Eight AM Solar gen and Solar Ramp')

ggplot(six,aes(x=sramp, y=(SP15hr/CAISO) , color=factor(year)) )+geom_point() +geom_smooth(method = "lm") + labs(title='6 pm  solar ramp to SP15/Demand')

ggplot(six,aes(x=sramp, y=(SP15hr/CAISO) ) )+geom_point() +geom_smooth(method = "lm") + labs(title='Eight AM Solar gen and Solar Ramp')
ggplot(b,aes(x=sramp, y=(SP15hr/CAISO) ) )+geom_point() +geom_smooth(method = "lm") + labs(title='Eight AM Solar gen and Solar Ramp')


ggplot(six,aes(x=sramp, y=solar ) )+geom_point() +geom_smooth(method = "lm") + labs(title='Eight AM Solar gen and Solar Ramp')


ggplot(one,aes(x=sramp, y=solar ) )+geom_point() +geom_smooth(method = "lm") + labs(title='One PM Solar gen and Solar Ramp')
ggplot(one,aes(x=sramp, y=solar , color=factor(year)) )+geom_point() +geom_smooth(method = "lm") + labs(title='One PM Solar gen and Solar Ramp')


ggplot(six,aes(x=sramp, y=solar ) )+geom_point() +geom_smooth(method = "lm") + labs(title='Six PM Solar gen and Solar Ramp')


ggplot(six,aes(x=date, y=hydro, color=factor(year) ) )+geom_point() 

ggplot(one,aes(x=date, y=hydro, color=factor(year) ) )+geom_point() 

ggplot(b,aes(x=date, y=hydro, color=factor(year) ) )+geom_point() 

ggplot(one,aes(x=solar, y=hydro, color=factor(year) ) )+geom_point() 

ggplot(six,aes(x=solar, y=hydro, color=factor(year) ) )+geom_point() 

ggplot(six,aes(x=date, y=thermal, color=factor(year) ) ) +geom_point()

ggplot(eight,aes(x=date, y=hydro, color=factor(year) ) )+geom_point() 

ggplot(eight,aes(x=date, y=thermal, color=factor(year) ) ) +geom_point()


ggplot(eight,aes(x=solar, y=hydro, color=factor(year) ) )+geom_point() +geom_smooth(method='lm')

ggplot(one,aes(x=solar, y=thermal, color=factor(year) ) )+geom_point() +geom_smooth(method='lm')

ggplot(one,aes(x=date, y=thermal, color=factor(year) ) ) +geom_point()

ggplot(one,aes(x=date, y=thermal ) ) +geom_point()

ggplot(one,aes(x=date, y=imports ) ) +geom_point()

ggplot(eight,aes(x=date, y=imports ) ) +geom_point()

ggplot(eight,aes(x=thermal, y=hydro, color=factor(year) ) )+geom_point() +geom_smooth(method='lm')

ggplot(eight,aes(x=thermal, y=hydro, color=factor(year) ) )+geom_smooth()

ggplot(eight,aes(x=thermal, y=hydro ) )+geom_smooth() + geom_point()

ggplot(six, aes(date)) + 
  geom_point(aes(y = thermal, colour = "thermal")) + 
  geom_point(aes(y = solar, colour = "solar")) +
  geom_point(aes(y = hydro, colour = "hydro")) +
  geom_point(aes(y = wind, colour = "wind")) +
    labs(title='six pm generation')

ggplot(six, aes(date)) + 
  geom_point(aes(y = thermal, colour = "thermal")) + 
    geom_point(aes(y = hydro, colour = "hydro")) +
    labs(title='six pm generation')

ggplot(b,aes(x=thermal, y=hydro, color=factor(year) ) )+geom_point() +geom_smooth(method='lm')

ggplot(b,aes(x=imports, y=hydro, color=factor(year) ) )+geom_point() +geom_smooth(method='lm')

ggplot(eight,aes(x=imports, y=hydro, color=factor(year) ) )+geom_point() +geom_smooth(method='lm')

ggplot(one,aes(x=imports, y=hydro, color=factor(year) ) )+geom_point() +geom_smooth(method='lm')

ggplot(one,aes(x=date, y=wind, color=factor(month) ) )+geom_point() +geom_smooth(method='lm')


ggplot(six,aes(x=CAISO, y=SP15hr ) )+geom_point() +geom_smooth(method='lm')

ggplot(six,aes(y=(SP15hr/CAISO), x=date ) )+geom_point() +geom_smooth(method='lm')

ggplot(six,aes(y=(SP15hr/CAISO), x=date ) )+geom_point() 
acf(resid(fit))
#----
#Geom density - use this section to create a solar density template for a project Brian Burgin is working on.

d <-filter(c, year == 2015)
e <- select(d, hour, solar, month)
ggplot(d, aes(x=solar, color=factor(month))) +geom_density() +facet_wrap(~hour) +labs(title='solar density')

library(xlsx)
write.xlsx(e, file="data.xlsx")

#-----
#correlation plot
detach("package:dplyr", unload=TRUE)
library(dplyr)
pcorr <- filter(b, IT>0)
M <-select(pcorr, CleanTTFPrice:NP)
M$carbon <- pcorr$carbon
M$ES <- pcorr$ES
N <-select(b, -hour, -date, -time, -year, -month)
#all of the filters below eliminate a mere 16 observations.  very robust data.  I like it.
N <-filter(N, geothermal>0)
N <-filter(N, solar>-100000000)
N <-filter(N, sramp>-1E10)
N <-filter(N, thermal>0)
O <-cor(N)
corrplot(O, method="circle")
corrplot.mixed(O)

#-----
#linear models
#-----
#these are the standard beareres.  Below these few are the other functional forms I tried.
fit <-lm(SP15hr ~ CAISO + socal +  solar + wind, data= eight)
summary(fit)



fit <-lm(SP15hr ~ CAISO + socal + solar, data= eight)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal + solar + wind, data= eight)
summary(fit)

fit <-lm(SP15hr/CAISO ~ socal + solar + wind + wkdy + as.factor(month), data= eight)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal + solar, data= one)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal + solar + wind, data= one)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal + solar + sramp + wind, data= one)
summary(fit)
vif(fit)

fit <-lm(SP15hr ~ CAISO + socal + solar + sramp + wind + wkdy + as.factor(month), data= one)
summary(fit)

fit <-lm(SP15hr/CAISO ~ socal + solar + wind + wkdy + as.factor(month), data= one)
summary(fit)

fit <-lm(SP15hr/CAISO ~ socal + solar + wind + wkdy, data= one)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal + solar, data= six)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal + solar + wind, data= six)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal + solar + sramp + wind, data= six)
summary(fit)
sqrt(vif(fit))

fit <-lm(SP15hr ~ CAISO + socal + sramp + wind, data= six)
summary(fit)

fit <-lm(SP15hr/CAISO ~ socal + solar + wind + wkdy + as.factor(month), data= six)
summary(fit)

#adding sramp to a solar model

fit <-lm(SP15hr/CAISO ~ socal + solar + sramp + wind + wkdy + as.factor(month), data= six)
summary(fit)

fit <-lm(SP15hr/CAISO ~ socal + solar + sramp + wind + wkdy + as.factor(month), data= one)
summary(fit)

fit <-lm(SP15hr/CAISO ~ socal + solar + sramp + wind + wkdy + as.factor(month), data= one)
summary(fit)

fit <-lm(SP15hr/CAISO ~ socal + solar + sramp + wind + wkdy + as.factor(month), data= eight)
summary(fit)

fit <-lm(SP15hr/CAISO ~ socal + solar + sramp + wind + wramp + wkdy + as.factor(month), data= eight)
summary(fit)


fit <-lm(SP15hr/CAISO ~ socal + solar + sramp + wind + wkdy + as.factor(month), data= b)
summary(fit)

fit <-lm(SP15hr/CAISO ~ socal + solar + sramp + wind + wkdy + as.factor(month)*as.factor(hour), data= b)
summary(fit)

fit <-lm(SP15hr/CAISO ~ socal + solar + wind + wkdy + as.factor(month), data= one)
summary(fit)

fit <-lm(SP15hr/CAISO ~ socal + solar + wind + wkdy + as.factor(month), data= eight)
summary(fit)

fit <-lm(SP15hr/CAISO ~ socal + solar + wind + wkdy, data= six)
summary(fit)

fit <-lm(SP15hr/CAISO ~ socal + sramp + wind + wkdy + as.factor(month), data= six)
summary(fit)


#not much different effect with sramp
#what is interesting, however, is the sign of the sramp variable.  It is negative for eight am, which means that a faster ramp weighs heavier on prices - that makes sense.
#but when we get to the 6pm model, the value becomes positive, which would mean that a lower ramping speed would have higher prices.  My wager here is that a lower ramping at six is a signal of seasonality, in that a higher (more negative) ramping value in the evening means net higher solar generation, all else equal.
fit <-lm(SP15hr ~ CAISO + socal +  sramp, data= eight)
summary(fit)
fit <-lm(SP15hr ~ CAISO + socal +  sramp + wind, data= eight)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal + wind, data= eight)
summary(fit)

fit <-lm(SP15hr ~ socal, data= eight)
summary(fit)


#the single hour's lag doesn't do it, but maybe 24 hours will... wait.  the price data is already one hour a day.  nm.
fit <-lm(SP15hr ~ socal + lag(SP15hr, 1), data= eight)
summary(fit)

fit <-lm(SP15hr ~ socal + lag(sp15dayprice,1), data= eight)
summary(fit)

fit <-lm(SP15hr ~ socal + sp15dayprice, data= eight)
summary(fit)

eight$lpricehr <-sapply(eight$SP15hr, reglag)

fit <-lm(SP15hr ~ socal + renewables, data= eight)
summary(fit)

fit <-lm(sp15dayprice ~ socal + renewables, data= eight)
summary(fit)

fit <-lm(sp15dayprice ~ socal , data= eight)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal +  solar + wind + as.factor(month), data= eight)
summary(fit)


#tried lagging wind, not much effect.
fit <-lm(SP15hr ~ CAISO + socal  + solar + lag(wind,1) , data= eight)
summary(fit)

#logging changes functional form, but not the fit.
fit <-lm(log(SP15hr) ~ log(CAISO) + log(socal) +  sramp + wind, data= eight)
summary(fit)

#moving on to other hours
#one pm
fit <-lm(SP15hr ~ CAISO + socal +  sramp + wind, data= one)
summary(fit)
fit <-lm(SP15hr ~ CAISO + socal +  solar + wind, data= one)
summary(fit)
fit <-lm(SP15hr ~ CAISO + socal +  solar + lag(wind, 1), data= one)
summary(fit)
#six pm
fit <-lm(SP15hr ~ CAISO + socal +  sramp + wind, data= six)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal +  solar + wind, data= six)
summary(fit)

fit <-lm((SP15hr/CAISO) ~  socal +  solar + wind, data= six)
summary(fit)

fit <-lm((sp15dayprice) ~  socal, data= six)
summary(fit)

fit <-lm((SP15hr/CAISO) ~  socal +  solar + wind + lag(SP15hr/CAISO,1), data= six)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal + wind, data= six)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal +  solar + lag(wind,1), data= six)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal +  solar + wind + hydro, data= six)
summary(fit)



fit <-rlm(SP15hr ~ CAISO + socal +  solar + wind, data= one)
summary(fit)
plot(fit)
fit$w[order(fit$w)][1:100]


fit <-lm(SP15hr ~ CAISO + socal +  solar + wind, data= one)
summary(fit)
plot(fit)

#six pm data on the daily average price
fit <-lm(sp15dayprice ~ CAISO + socal +  solar + wind, data= six)
summary(fit)

fit <-lm(sp15dayprice ~ CAISO + socal +  solar + wind, data= one)
summary(fit)

fit <-lm(sp15dayprice ~ CAISO + socal +  solar + wind, data= eight)
summary(fit)

fit <-lm(hydro ~ CAISO + solar + wind, data= one)
summary(fit)

#looking at the whole day
fit <-lm(SP15hr ~ CAISO + socal +  solar + wind, data= c)
summary(fit)

fit <-lm(SP15hr ~ CAISO + lag(socal,24) +  solar + wind, data= c)
summary(fit)

fit <-lm(SP15hr ~ lag(CAISO) + lag(socal,24) +  solar + wind, data= c)
summary(fit)

fit <-lm(SP15hr ~ (CAISO - lag(CAISO, 1)) + socal +  solar + wind, data= c)
summary(fit)

fit <-lm(sp15dayprice ~ CAISO + socal +  solar + wind, data= c)
summary(fit)


fit <-lm(SP15hr ~ CAISO + socal +  solar + lag(wind,1), data= c)
summary(fit)

#testing for outliers

outlierTest(c$SP15hr)

#quantile regressions

Y <-cbind(six$SP15hr)
X <-cbind(six$CAISO, six$socal, six$sramp, six$wind)




quantreg <-rq(Y ~ X, data=six, tau=0.05)
summary(quantreg)
r1 <-resid(quantreg)
f1<-fitted(quantreg)
plot(r1,f1)


quantreg <-rq(Y ~ X, data=six, tau=0.95)
summary(quantreg)
r1 <-resid(quantreg)
f1<-fitted(quantreg)
plot(r1,f1)

quantreg13579 <-rq(Y~X, data=six, tau=c(0.1,0.3,0.5,0.7,0.9))
summary(quantreg13579)

quantreg.all <-rq(Y~X, tau = seq(0.05, 0.95, by =0.05), data=six)
quantreg.plot <- summary(quantreg.all)
plot(quantreg.plot)


Y <-cbind(rdown$SP15hr)
X <-cbind(rdown$CAISO, rdown$socal, rdown$sramp, rdown$wind)
quantreg13579 <-rq(Y~X, data=rdown, tau=c(0.1,0.3,0.5,0.7,0.9))
summary(quantreg13579)

quantreg.all <-rq(Y~X, tau = seq(0.05, 0.95, by =0.05), data=rsix)
quantreg.plot <- summary(quantreg.all)
plot(quantreg.plot)

fit<-lm(thermal ~sramp, data=rdown)
summary(fit)

fit<-lm(imports ~sramp, data=rdown)
summary(fit)

fit<-lm(hydro ~sramp, data=rdown)
summary(fit)

fit <-lm(SP15hr ~ CAISO + socal + sramp + wind, data=rdown)
summary(fit)
