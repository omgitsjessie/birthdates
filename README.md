# Visualizing Valentine's Day: Jessie Wastes a Weekend

Since fivethirtyeight.com did such a great article on birthdates, and some monthly effects related to superstition, I thought it would be cute to shift those data backwards and look at trends around conception dates.  Are there more Valentine babies?  Anything else going on that's interesting?

### U.S. Birth Data - from fivethirtyeight.com

This folder contains data behind the story [Some People Are Too Superstitious To Have A Baby On Friday The 13th](http://fivethirtyeight.com/features/some-people-are-too-superstitious-to-have-a-baby-on-friday-the-13th/).

There are two files:

`US_births_1994-2003_CDC_NCHS.csv` contains U.S. births data for the years 1994 to 2003, as provided by the Centers for Disease Control and Prevention's National Center for Health Statistics.

`US_births_2000-2014_SSA.csv` contains U.S. births data for the years 2000 to 2014, as provided by the Social Security Administration.

Both files have the following structure:

Header | Definition
---|---------
`year` | Year
`month` | Month
`date_of_month` | Day number of the month
`day_of_week` | Day of week, where 1 is Monday and 7 is Sunday
`births` | Number of births



## Step 1: What are we even looking at?

My first try was to look at the time series of birth data, over the ten years of CDC data. Looking at the point data, there's something strange with how the data points have a very clear separation. 

![Birthdates over ten years](img/US Births per day from 1994 to 2004.png?raw=true)

A short consultation with one of my friends that has a child solved that riddle; it's totally normal for doctors to only schedule births for weekdays. Looking at that same plot and grouping by weekday writes off the mystery!

![Birthdates over ten years, collapsed to show annual trend. Grouped by weekday.](img/Ten years of births by month.png)

If you look closely, you see a few clusters of Mondays lurking down in our weekend strata--visual inspection shows that these are happening around Labor Day & Memorial Day.  On top of that, we have a Thursday cluster down there, on Thanksgiving! There are known weekday trends; fivethirtyeight.com identified a few of the monthly trends--but what we're really after is conception data.  Since scheduling around holidays and weekends is not a factor that is likely to impact conception timing, we need to de-trend our data.  After all, we may have fewer births on Labor Days, but that is likely related more to scheduling effects than fewer people conceiving 9 months before Labor Day.  Let's decompose our signal to isolate out weekly, monthly, and annual effects.  Enter: Time Series!


## Step 2: De-trend our time series.

First, we know we need to remove the 7-day weekday/weekend trend in birthrates.  Easy enough with time series decomposition.

```
#Look at time series decomposition effects due to different seasonality.
#remove weekday trend.
births_ts2 <- ts(US_births_1994.2003_CDC_NCHS$births, frequency = 7, start = c(1,1))
plot.ts(births_ts2)
birthscomp2 <- decompose(births_ts2)
plot(birthscomp2)
#remove weekday trend, look at month trend.
births_ts3 <- births_ts2 - birthscomp2$seasonal
plot(births_ts3)
```
![Birthdate time series, decomposed to see weekly trend.](img/ts decomposition - removing weekday trend.png)

This is a standard decomposition plot.  With ten years of observations, this one is extremely dense.  R's ts package can identify and remove the weekly signal -- the 'trend' plot shows the clearer patterns for birth rates after removing the weekly effect.

![Birthdate time series, with weekly trend removed.](img/Birthdate ts with weekly trend removed.png)

With that weekly trend removed, we can easily see that there is annual repetition.  But before we get there, we will Next, look for and remove any evident monthly trends.  This should catch items such as avoiding Friday the 13th, identified in fivethirtyeight's article.  


```
births_ts4 <- ts(births_ts3, frequency = 30.4, start = c(1,1))
births_ts4_decomp <- decompose(births_ts4)
plot(births_ts4_decomp)
```
![Birthdate time series, with monthly trend removed.](img/Birthdate time series with monthly trend removed.png)

Finally, isolate out the annual trend over time.  This is likely to show effects of birthrates on holidays.  Similar to how doctors avoid scheduling on weekends, some holidays lead to dates with fewer scheduled births year-over-year. Worth noting--lower birth rates on annual holidays like Memorial Day or Thanksgiving are unlikely to have been part of a conception plan, so now we'll go back into our birthdate effect data, and remove birthdate effect data falling on those holidays.  

A more accurate approach may be to identify each holiday in the original 10-year set and remove those dates specifically; but for the sake of a quick-and-dirty analysis we will identify date intervals (Thanksgiving fell between 11/22 and 11/29 for 1994-2004), and gloss over those data.

```
#Separate out annual trend, that's what you want (but just one cycle).
births_ts5 <- births_ts4 - births_ts4_decomp$seasonal
births_ts6 <- ts(births_ts5, frequency = 365.25, start = c(1,1))
births_ts6_decomp <- decompose(births_ts6)
plot(births_ts6_decomp)
```
![Birthdate time series, with annual trend removed.](img/Birthdate decomposition with annual trend removed.png)

One interesting thing to look at in this decomposition is the trend over ten years -- we can see the rough fall and rise in birth rates over time.  Looking at one cycle of that annual trend, we can see the dips in our smoothed curve representing scheduling preferences over major holidays.

![Annual Birthrate Trend, without weekday or monthly trends.](img/Annual birthrate trend without weekday or monthly trends.png)


## Step 3: Visualize Conception (AKA: Conception Inception)

Re-plotting these data after we remove the effect around scheduled holidays gives us a reasonable representation of birth data, in a fictional universe where there are no holiday or weekend preferences by hospitals or new parents surrounding birth scheduling.  

![Annual Birthrate Trend, without weekday, monthly, or holiday birth-scheduling trends.](img/Annual birthrate trend with scheduling effects removed.png)

From this plot, it's easy enough to rearrange our data so that it takes into account the average gestational period of 280 days, and demonstrates the average conception periods that would have resulted in the annual birth date trends that we saw between 1994 and 2004.  

![Annual Conception Trend, without weekday, monthly, or holiday birth-scheduling trends.](img/Annual conception trend.png)

Immediately visible are two local maxima: Valentine's Day and St. Patrick's Day!  While not a statistical confirmation of the romantic evenings associated with both holidays, it's enough that I'm comfortable feeling a little smug about my initial feelings about conception trends on Valentine's Day.   