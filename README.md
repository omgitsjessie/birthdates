# U.S. Births

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


# Visualizing Valentine's Day: Jessie Wastes a Weekend

Since fivethirtyeight.com did such a great article on birthdates, and some monthly effects related to superstition, I thought it would be cute to shift those data backwards and look at trends around conception dates.  Are there more Valentine babies?  Anything else going on that's interesting?

## Step 1: What are we even looking at?

My first try was to look at the time series of birth data, over the ten years of CDC data. Looking at the point data, there's something strange with how the data points have a very clear separation. A short consultation with one of my friends that has a child solved that riddle; it's totally normal for doctors to only schedule births for weekdays. Looking at that same plot and grouping by weekday writes off the mystery!

There are known weekday trends; fivethirtyeight.com identified a few of the monthly trends--but what we're really after is conception data.  Since scheduling around holidays and weekends is not a factor that is likely to impact conception timing, we need to de-trend our data.  Enter: Time Series!

## Step 2: De-trend our time series.

First, we know we need to remove the 7-day weekday/weekend trend in birthrates.  Easy enough with time series decomposition.

Next, look for and remove any evident monthly trends.  This should catch items such as avoiding Friday the 13th, identified in fivethirtyeight's article.  

Finally, isolate out the annual trend over time.  This is likely to show effects of birthrates on holidays.  Similar to how doctors avoid scheduling on weekends; some holidays create days that are likely not going to have scheduled births year-over-year.  Days like Memorial Day or Thanksgiving recur annually. Those birthdates are unlikely to have been part of a conception plan, so now we'll go back into our birthdate effect data, and remove birthdate effect data falling on those holidays.  A more accurate approach may be to identify each holiday in the original 10-year set and remove those dates specifically; but for the sake of a quick-and-dirty analysis we will identify date intervals (Thanksgiving fell between 11/22 and 11/29 for 1994-2004), and gloss over those data.

## Step 3: Visualize Conception!!

Re-plotting these data give us a reasonable representation of birth data, in a fictional universe where there are no holiday or weekend preferences by hospitals or new parents surrounding birth scheduling.  

From this plot, it's easy enough to rearrange our data so that it takes into account the average gestational period of 280 days, and demonstrates the average conception periods that would have resulted in the annual birth date trends that we saw between 1994 and 2004.  Immediately visible are two local maxima: Valentine's Day and St. Patrick's Day!  While not a statistical confirmation of the romantic evenings associated with both holidays, it's enough to raise my eyebrows.  