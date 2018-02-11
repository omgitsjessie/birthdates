library(ggplot2)
library(dplyr)
library(lubridate)

#Read in birthdate data
US_births_1994.2003_CDC_NCHS <- read.csv("US_births_1994-2003_CDC_NCHS.csv")
US_births_2000.2014_SSA <- read.csv("US_births_2000-2014_SSA.csv")

#Create date type field in each set
US_births_1994.2003_CDC_NCHS$date <- paste0(US_births_1994.2003_CDC_NCHS$month, "-", 
                                            US_births_1994.2003_CDC_NCHS$date_of_month, "-",
                                            US_births_1994.2003_CDC_NCHS$year) %>%
                                                as.Date("%m-%d-%Y")
US_births_1994.2003_CDC_NCHS$month_day <- paste0(US_births_1994.2003_CDC_NCHS$month, "-", 
                                          US_births_1994.2003_CDC_NCHS$date_of_month) %>%
                                              as.Date("%m-%d")

US_births_2000.2014_SSA$date <- paste0(US_births_2000.2014_SSA$month, "-", 
                                       US_births_2000.2014_SSA$date_of_month, "-",
                                       US_births_2000.2014_SSA$year)
US_births_2000.2014_SSA$date <- as.Date(US_births_2000.2014_SSA$date, "%m-%d-%Y")


#Look at time series over the ten years
ggplot(US_births_1994.2003_CDC_NCHS, aes(date, births)) + geom_point() +
  xlab("") + ylab("births")
#collapse to be over a year - scatter looks goofy; look at days of week!
ggplot(US_births_1994.2003_CDC_NCHS, aes(month_day, births)) + geom_point(aes(color = factor(day_of_week))) +
  xlab("") + ylab("births") + ggtitle("Ten years of births, by month") + 
  theme_bw()

#Even over ten years, there's a clear day-of-week trend where births aren't 
#scheduled on weekends.  And probably some non-weekend holidays.  

#Calculate avg conception dates
# US_births_1994.2003_CDC_NCHS$conception <- US_births_1994.2003_CDC_NCHS$date - 280
# US_births_1994.2003_CDC_NCHS$conception_month <- month(as.POSIXlt(US_births_1994.2003_CDC_NCHS$conception, format="%Y-%m-%d"))
# US_births_1994.2003_CDC_NCHS$conception_day <- day(as.POSIXlt(US_births_1994.2003_CDC_NCHS$conception, format="%Y-%m-%d"))
# 
# US_births_1994.2003_CDC_NCHS$conception_month_day <- paste0(US_births_1994.2003_CDC_NCHS$conception_month, "-", 
#                                                  US_births_1994.2003_CDC_NCHS$conception_day) %>%
#                                           as.Date("%m-%d")

# avg_conceptions <- US_births_1994.2003_CDC_NCHS %>%
#   group_by(conception_month_day) %>%
#   summarise(avg_conceptions = mean(births))

avg_births <- US_births_1994.2003_CDC_NCHS %>%
  group_by(month_day) %>%
  summarise(avg_births = mean(births))

#Plot showing estimated conception dates collapsed to be over a year
#without removing weekly & monthly scheduling artifacts
ggplot(avg_conceptions, aes(conception_month_day, avg_conceptions)) + geom_line() +
  xlab("") + ylab("conceptions") + ggtitle("Avg Conception Dates") + 
  theme_bw()

#TODO: Repeat time series, but with uncollapsed data.
#remove weekday trend.
births_ts2 <- ts(US_births_1994.2003_CDC_NCHS$births, frequency = 7, start = c(1,1))
plot.ts(births_ts2)
birthscomp2 <- decompose(births_ts2)
plot(birthscomp2)
#remove weekday trend, look at month trend.
births_ts3 <- births_ts2 - birthscomp2$seasonal
plot(births_ts3)
births_ts4 <- ts(births_ts3, frequency = 30, start = c(1,1))
births_ts4_decomp <- decompose(births_ts4)
plot(births_ts4_decomp)
#Separate out annual trend, that's what you want (but just one cycle).
births_ts5 <- births_ts4 - births_ts4_decomp$seasonal
births_ts6 <- ts(births_ts5, frequency = 365.25, start = c(1,1))
births_ts6_decomp <- decompose(births_ts6)
plot(births_ts6_decomp)

#trend in birthrate from 1994 to 2004
plot(births_ts6_decomp$trend)

#annual trend in birthrate, each year
#TODO - plot births_ts6_decomp[1:365, "seasonal"]

#monthly trends in birthrate, each month
#TODO - plot births_ts4_decomp[1:30, "seasonal"]

#weekly trends in birthrate, each week
#TODO - plot birthscomp2[1:7, "seasonal"]

#Specifically looking at annual trend in birthdate, shift the plot so that it
#becomes annual trends in "conception date".  
  #Consider smoothing with moving average to  avoid ideas like
  #"nobody gets busy 280 days before labor day weekend"
