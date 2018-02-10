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


#Calculate avg conception dates
US_births_1994.2003_CDC_NCHS$conception <- US_births_1994.2003_CDC_NCHS$date - 280
US_births_1994.2003_CDC_NCHS$conception_month <- month(as.POSIXlt(US_births_1994.2003_CDC_NCHS$conception, format="%Y-%m-%d"))
US_births_1994.2003_CDC_NCHS$conception_day <- day(as.POSIXlt(US_births_1994.2003_CDC_NCHS$conception, format="%Y-%m-%d"))

US_births_1994.2003_CDC_NCHS$conception_month_day <- paste0(US_births_1994.2003_CDC_NCHS$conception_month, "-", 
                                                 US_births_1994.2003_CDC_NCHS$conception_day) %>%
                                          as.Date("%m-%d")

ggplot(US_births_1994.2003_CDC_NCHS, aes(conception_month_day, births)) + geom_point() +
  xlab("") + ylab("births") + ggtitle("Avg Conception Dates") + 
  theme_bw()

avg_conceptions <- US_births_1994.2003_CDC_NCHS %>%
  group_by(conception_month_day) %>%
  summarise(avg_conceptions = mean(births))
#TODO - remove the day of week effect since that is due to scheduling births..

ggplot(avg_conceptions, aes(conception_month_day, avg_conceptions)) + geom_line() +
  xlab("") + ylab("businessdays") + ggtitle("Avg Conception Dates") + 
  theme_bw()


#Visualize time series of conception peaks
birth_ts <- ts(US_births_1994.2003_CDC_NCHS$births, frequency = 365, start = c(1994, 1))
plot.ts(birth_ts)
#decompose time series into trend data and random component
birthscomp <- decompose(test)
plot(birthscomp) #shows trend for births over the years

#Can you pull out the day-of-week component and annual component?  To find peaks @ holidays etc.