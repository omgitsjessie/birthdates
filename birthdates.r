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

#tbh I don't use this data.  recreate with these data next?
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
births_ts4 <- ts(births_ts3, frequency = 30.4, start = c(1,1))
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
annual_birthrates <- births_ts6_decomp$seasonal[1:365] %>% data.frame()
names(annual_birthrates) <- c("effect")
annual_birthrates$date <- seq(as.Date("2018/1/1"), by = "day", length.out = 365)
ggplot(annual_birthrates, aes(date, effect)) + geom_smooth(span = 0.1, se = FALSE) + 
  xlab("") + ylab("Birth Trend") + ggtitle("Annual Birthrate Trend, 1994-2004") + 
  scale_x_date(date_labels = "%B", date_minor_breaks = "1 month") + 
  theme_bw() + 
  #Annotate holidays in US between 1994-2004
  #Christmas 12/24 - 12/26
  #Thanksgiving 11/22 - 11/29
  #Labor Day 9/1 - 9/7
  #4th of July 7/4
  #Memorial Day 5/24 - 5/30
  annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2018-01-03"), ymin = -1500, ymax = -750, alpha = 0.2) +
  annotate("text", x=as.Date("2018-01-02"), y=-1500, label="New Year's") +
  annotate("rect", xmin = as.Date("2018-05-24"), xmax = as.Date("2018-05-30"), ymin = -250, ymax = 0, alpha = 0.2) +
  annotate("text", x=as.Date("2018-05-27"), y=-375, label="Memorial Day") +
  annotate("rect", xmin = as.Date("2018-07-04"), xmax = as.Date("2018-07-06"), ymin = 125, ymax = 375, alpha = 0.2) + 
  annotate("text", x=as.Date("2018-07-05"), y=50, label="4th of July") +
  annotate("rect", xmin = as.Date("2018-09-01"), xmax = as.Date("2018-09-07"), ymin = 200, ymax = 450, alpha = 0.2) + 
  annotate("text", x=as.Date("2018-09-05"), y=125, label="Labor Day") +
  annotate("rect", xmin = as.Date("2018-11-22"), xmax = as.Date("2018-11-29"), ymin = -625, ymax = -375, alpha = 0.2) +  
  annotate("text", x=as.Date("2018-11-27"), y=-700, label="Thanksgiving") +
  annotate("rect", xmin = as.Date("2018-12-24"), xmax = as.Date("2018-12-26"), ymin = -500, ymax = -250, alpha = 0.2) +  
  annotate("text", x=as.Date("2018-12-25"), y=-550, label="Christmas")
  
#monthly trends in birthrate, each month
monthly_birthrates <- births_ts4_decomp$seasonal[1:30] %>% data.frame()
names(monthly_birthrates) <- c("effect")
monthly_birthrates$date <- seq(1, by = 1, length.out = 30)
ggplot(monthly_birthrates, aes(date, effect)) + geom_smooth(span = 0.5) + 
  xlab("") + ylab("Birth Trend") + ggtitle("Monthly Birthrate Trend, 1994-2004") + 
  scale_x_continuous(minor_breaks = c(5, 15, 25)) + 
  theme_bw()
#TODO - does this make sense?  valleys @ 4th & 26th of each month?


#weekly trends in birthrate, each week
weekly_birthrates <- birthscomp2$seasonal[1:7] %>% data.frame()
names(weekly_birthrates) <- c("effect")
weekly_birthrates$date <- seq(as.Date("1994/1/1"), by = "day", length.out = 7)
weekly_birthrates$date <- c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
ggplot(weekly_birthrates, aes(date, effect)) + geom_smooth() + 
  xlab("") + ylab("Birth Trend") + ggtitle("Weekly Birthrate Trend, 1994-2004") + 
  scale_x_date(date_labels = "%A", date_breaks = "1 day") + 
  theme_bw()

#Specifically looking at annual trend in birthdate, shift the plot so that it
#becomes annual trends in "conception date".  
  #Consider smoothing with moving average to  avoid ideas like
  #"nobody gets busy 280 days before labor day weekend"

#Identify candidates for scheduled holidays impacting birthrate in US between 1994-2004
#Christmas 12/24 - 12/26
#Thanksgiving 11/22 - 11/29
#Labor Day 9/1 - 9/7
#4th of July 7/4
#Memorial Day 5/24 - 5/30

#TODO - smooth over those bands.
#Add an indicator variable to annual_birthrates to indicate holiday band.
annual_birthrates$holiday <- NA
for (i in 1:nrow(annual_birthrates)) {
  if (annual_birthrates[i, "date"] >= as.Date("2018-01-01") && annual_birthrates[i, "date"] <= as.Date("2018-01-03")) {
    annual_birthrates[i, "holiday"] <- 1 #New Year's
  }
  if (annual_birthrates[i, "date"] >= as.Date("2018-05-24") && annual_birthrates[i, "date"] <= as.Date("2018-05-30")) {
    annual_birthrates[i, "holiday"] <- 1 #Memorial Day
  }
  if (annual_birthrates[i, "date"] >= as.Date("2018-07-04") && annual_birthrates[i, "date"] <= as.Date("2018-07-06")) {
    annual_birthrates[i, "holiday"] <- 1 #July 4th
  }
  if (annual_birthrates[i, "date"] >= as.Date("2018-09-01") && annual_birthrates[i, "date"] <= as.Date("2018-09-07")) {
    annual_birthrates[i, "holiday"] <- 1 #Labor Day
  }
  if (annual_birthrates[i, "date"] >= as.Date("2018-11-22") && annual_birthrates[i, "date"] <= as.Date("2018-11-29")) {
    annual_birthrates[i, "holiday"] <- 1 #Thanksgiving
  }
  if (annual_birthrates[i, "date"] >= as.Date("2018-12-24") && annual_birthrates[i, "date"] <= as.Date("2018-12-26")) {
    annual_birthrates[i, "holiday"] <- 1 #Christmas
  }
}
holidays <- data.frame("holiday" = c("New Year", "Memorial Day", "July 4th", "Labor Day", "Thanksgiving", "Christmas"), 
                       "StartDate" = c("2018-01-01", "2018-05-24", "2018-07-04", "2018-09-01", "2018-11-22", "2018-12-24"), 
                       "EndDate" = c("2018-01-03", "2018-05-30", "2018-07-06", "2018-09-07", "2018-11-29", "2018-12-26"))
#TODO - for each holiday segment identify a start and a stop with lags
#TODO - for each holiday segment do a moving average from one day before to one day after.  Remove NYE entirely.


#TODO - move everything back 280 days to look at conception trends.

