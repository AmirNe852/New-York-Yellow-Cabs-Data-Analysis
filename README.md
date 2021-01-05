# New-York-Yellow-Cabs-Data-Analysis

New York Yellow Cabs Data Analysis
Kimia Khaffafzadeh Ahrabi, Amir Nejat, Tarek Halabi
5/16/2020
Introduction:

The dataset we have worked on belongs to the yellow taxi cabs that operate in New York. We chose October 2018 data to analyze and try to see if we can learn more about the commute new yorkers experience on a daily basis.

We began by setting up and cleaning the data, only selecting our desired attributes. The mentioned dataset had 8 million entries so we took a random sample of 2 million entries to work with.

url <- "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-10.csv"
taxi_tab <- read_csv(url)
## Parsed with column specification:
## cols(
##   VendorID = col_double(),
##   tpep_pickup_datetime = col_datetime(format = ""),
##   tpep_dropoff_datetime = col_datetime(format = ""),
##   passenger_count = col_double(),
##   trip_distance = col_double(),
##   RatecodeID = col_double(),
##   store_and_fwd_flag = col_character(),
##   PULocationID = col_double(),
##   DOLocationID = col_double(),
##   payment_type = col_double(),
##   fare_amount = col_double(),
##   extra = col_double(),
##   mta_tax = col_double(),
##   tip_amount = col_double(),
##   tolls_amount = col_double(),
##   improvement_surcharge = col_double(),
##   total_amount = col_double()
## )
head(taxi_tab)
## # A tibble: 6 x 17
##   VendorID tpep_pickup_dateti… tpep_dropoff_datet… passenger_count trip_distance
##      <dbl> <dttm>              <dttm>                        <dbl>         <dbl>
## 1        1 2018-10-01 00:23:34 2018-10-01 00:44:50               1           6.2
## 2        1 2018-10-01 00:40:05 2018-10-01 01:01:56               1          12.6
## 3        1 2018-10-01 00:05:35 2018-10-01 00:19:38               1           6.1
## 4        1 2018-10-01 00:42:56 2018-10-01 00:49:00               1           1.3
## 5        1 2018-10-01 00:19:14 2018-10-01 00:31:54               1           2.6
## 6        1 2018-10-01 00:36:27 2018-10-01 01:39:20               1          19.7
## # … with 12 more variables: RatecodeID <dbl>, store_and_fwd_flag <chr>,
## #   PULocationID <dbl>, DOLocationID <dbl>, payment_type <dbl>,
## #   fare_amount <dbl>, extra <dbl>, mta_tax <dbl>, tip_amount <dbl>,
## #   tolls_amount <dbl>, improvement_surcharge <dbl>, total_amount <dbl>
taxi_tab <- taxi_tab %>%
  sample_n(2000000) %>%
  select(VendorID, tpep_pickup_datetime, tpep_dropoff_datetime, passenger_count, trip_distance, fare_amount, tip_amount, tolls_amount, PULocationID, DOLocationID)
head(taxi_tab)
## # A tibble: 6 x 10
##   VendorID tpep_pickup_dateti… tpep_dropoff_datet… passenger_count trip_distance
##      <dbl> <dttm>              <dttm>                        <dbl>         <dbl>
## 1        2 2018-10-04 15:30:58 2018-10-04 15:58:15               1          2.27
## 2        2 2018-10-16 20:09:21 2018-10-16 20:25:00               1          2.57
## 3        1 2018-10-07 19:57:27 2018-10-07 20:08:03               1          1.1 
## 4        2 2018-10-23 17:57:50 2018-10-23 18:10:27               2          0.87
## 5        2 2018-10-22 13:25:44 2018-10-22 13:31:29               2          0.62
## 6        2 2018-10-28 16:50:18 2018-10-28 17:14:08               1          4.37
## # … with 5 more variables: fare_amount <dbl>, tip_amount <dbl>,
## #   tolls_amount <dbl>, PULocationID <dbl>, DOLocationID <dbl>
The data set includes the drop off and pickup location. However, it has used location identification number to specify different zones in New York. We found a helpful table mentioning which borough of New York these IDs refer to.

zoneurl <- "https://s3.amazonaws.com/nyc-tlc/misc/taxi+_zone_lookup.csv"
zone_tab <- read_csv(zoneurl)
## Parsed with column specification:
## cols(
##   LocationID = col_double(),
##   Borough = col_character(),
##   Zone = col_character(),
##   service_zone = col_character()
## )
head(zone_tab)
## # A tibble: 6 x 4
##   LocationID Borough       Zone                    service_zone
##        <dbl> <chr>         <chr>                   <chr>       
## 1          1 EWR           Newark Airport          EWR         
## 2          2 Queens        Jamaica Bay             Boro Zone   
## 3          3 Bronx         Allerton/Pelham Gardens Boro Zone   
## 4          4 Manhattan     Alphabet City           Yellow Zone 
## 5          5 Staten Island Arden Heights           Boro Zone   
## 6          6 Staten Island Arrochar/Fort Wadsworth Boro Zone
We now join our existing data set with this new table so that we can see to which burough of New York every ride entry belongs to.

  zone_df <- taxi_tab %>% full_join(zone_tab, by=c("PULocationID"="LocationID")) %>%
  select(-Zone, -service_zone)
  head(zone_df)
## # A tibble: 6 x 11
##   VendorID tpep_pickup_dateti… tpep_dropoff_datet… passenger_count trip_distance
##      <dbl> <dttm>              <dttm>                        <dbl>         <dbl>
## 1        2 2018-10-04 15:30:58 2018-10-04 15:58:15               1          2.27
## 2        2 2018-10-16 20:09:21 2018-10-16 20:25:00               1          2.57
## 3        1 2018-10-07 19:57:27 2018-10-07 20:08:03               1          1.1 
## 4        2 2018-10-23 17:57:50 2018-10-23 18:10:27               2          0.87
## 5        2 2018-10-22 13:25:44 2018-10-22 13:31:29               2          0.62
## 6        2 2018-10-28 16:50:18 2018-10-28 17:14:08               1          4.37
## # … with 6 more variables: fare_amount <dbl>, tip_amount <dbl>,
## #   tolls_amount <dbl>, PULocationID <dbl>, DOLocationID <dbl>, Borough <chr>
Since the dataset we had only included datetime attributes and we wanted to work with trends that would occur throughout the days of the week, we decided to add an extra attribute called ‘day’ that would show which day of the week that date was. The code below is showing this.

  split <- taxi_tab$tpep_dropoff_datetime
  split<-as.data.frame(split)
  split<- split%>%separate(split, c("date","time"), sep="\\s" )
  split$day <- weekdays(as.Date(split$date))
  taxi_tab$day <-split$day
We also added the same attribute to the dataframe that includes boroughs’ names.

split <- zone_df$tpep_dropoff_datetime
  split<-as.data.frame(split)
  split<- split%>%separate(split, c("date","time"), sep="\\s" )
  split$day <- weekdays(as.Date(split$date))
  zone_df$day <-split$day 
zone_df
## # A tibble: 2,000,005 x 12
##    VendorID tpep_pickup_dateti… tpep_dropoff_datet… passenger_count
##       <dbl> <dttm>              <dttm>                        <dbl>
##  1        2 2018-10-04 15:30:58 2018-10-04 15:58:15               1
##  2        2 2018-10-16 20:09:21 2018-10-16 20:25:00               1
##  3        1 2018-10-07 19:57:27 2018-10-07 20:08:03               1
##  4        2 2018-10-23 17:57:50 2018-10-23 18:10:27               2
##  5        2 2018-10-22 13:25:44 2018-10-22 13:31:29               2
##  6        2 2018-10-28 16:50:18 2018-10-28 17:14:08               1
##  7        2 2018-10-17 19:28:06 2018-10-17 19:47:19               1
##  8        1 2018-10-07 00:43:30 2018-10-07 00:58:39               1
##  9        1 2018-10-05 12:25:24 2018-10-05 12:31:06               1
## 10        2 2018-10-23 11:37:31 2018-10-23 11:42:41               1
## # … with 1,999,995 more rows, and 8 more variables: trip_distance <dbl>,
## #   fare_amount <dbl>, tip_amount <dbl>, tolls_amount <dbl>,
## #   PULocationID <dbl>, DOLocationID <dbl>, Borough <chr>, day <chr>
The chunk of code below gets the count of rides happening on days of the week, filtering out NA days and Unknown boroughs so that we have a more consistent and clean look at our data.

newdf <- zone_df %>%
  group_by(day,Borough) %>%
  summarise(count=n()) %>%
  filter(!is.na(day)) %>%
  filter(!(Borough=="Unknown"))
head(newdf)
## # A tibble: 6 x 3
## # Groups:   day [1]
##   day    Borough        count
##   <chr>  <chr>          <int>
## 1 Friday Bronx            417
## 2 Friday Brooklyn        3729
## 3 Friday EWR               28
## 4 Friday Manhattan     247290
## 5 Friday Queens         17914
## 6 Friday Staten Island      9
The number of rides in Manhatten is much higher than in the other buroughs. To be precise, out of our 2 million entry sample, 1.8 million belongs to this area and because of this significant difference, comparing counts of rides with other buroughs throughout the week causes our graph not to be that useful. We will basically see the trend in Manhattan clearly, but we will not be able to have any useful observation in regards to the other buroughs. Below we have singled out Manhattan rides. The graph is showing the number of rides that happen in this area (to be specific, picked up in this area) and how they change as the week goes by.

man_df<- newdf %>%
  filter(Borough=="Manhattan") %>%
  group_by(day) %>% 
  summarise(rides=sum(count))

#Arranging weekdays so that they are in the correct order when visualizing our data

man_df$day<- factor(man_df$day, levels=c("Monday",
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
man_df<-man_df[order(man_df$day),]

man_df %>%
  ggplot(aes(x=day, y=rides, fill=day))+geom_bar(stat = "identity")+ggtitle(label = "Weekday Ride Number Comparison")+theme_minimal()+theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+xlab("Days")+ylab("Number of Rides")


man_df
## # A tibble: 7 x 2
##   day        rides
##   <fct>      <int>
## 1 Monday    259433
## 2 Tuesday   292765
## 3 Wednesday 299997
## 4 Thursday  251516
## 5 Friday    247290
## 6 Saturday  244699
## 7 Sunday    211024
What can be observed is that the number of rides is the highest on Wednesdays and lowest on Sundays and Saturdays.

Another interesting observation is how the duration of rides might change throughout the week. Do newyorkers spend more time riding taxis on the weekend or do the majority use taxis to commute perhaps to and from work? Below we are adding the trip_duration attribute to our dataset.

 split2 <- taxi_tab$tpep_pickup_datetime
 split2 <- taxi_tab$tpep_dropoff_datetime
 split2<-as.data.frame(split2)
 split2$pickup <- taxi_tab$tpep_pickup_datetime
 split2 <- split2%>%separate(split2, c("date_drop","time_drop"), sep="\\s" )
 split2 <- split2%>%separate(pickup, c("date_pickup","time_pickup"), sep="\\s" )
 split2$drop_in_mins <- 60 * 24 * as.numeric(times(split2$time_drop))
 split2$pickup_in_mins <- 60 * 24 * as.numeric(times(split2$time_pickup))
split2$trip_duration <- split2$drop_in_mins - split2$pickup_in_mins
 taxi_tab$trip_duration <- split2$trip_duration
Now we graph the relationship the days of the week have with the trip durations.

dur_df<- taxi_tab %>%
  group_by(day) %>% 
  filter(trip_duration>0) %>%
  summarise(avgdur=mean(trip_duration)) 

dur_df$day<- factor(dur_df$day, levels=c("Monday",
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
dur_df<-dur_df[order(dur_df$day),]

dur_df
## # A tibble: 7 x 2
##   day       avgdur
##   <fct>      <dbl>
## 1 Monday      14.2
## 2 Tuesday     15.1
## 3 Wednesday   15.6
## 4 Thursday    16.7
## 5 Friday      16.5
## 6 Saturday    14.1
## 7 Sunday      14.1
dur_df %>%
  ggplot(aes(x=day, y=avgdur))+geom_bar(stat = "identity")+ggtitle(label = "Weekday Trip Duration Comparison")+theme_minimal()+theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+xlab("Days")+ylab("Number of Rides")


dur_df
## # A tibble: 7 x 2
##   day       avgdur
##   <fct>      <dbl>
## 1 Monday      14.2
## 2 Tuesday     15.1
## 3 Wednesday   15.6
## 4 Thursday    16.7
## 5 Friday      16.5
## 6 Saturday    14.1
## 7 Sunday      14.1
We see that in the middle of the week New Yorkers are spending longer times insinde the cabs which confirms our first graph as well.

Hypothesis testing(day of the week vs tipping):

Linear regression Model, We are trying to figure out the relationship between day of the week and the tip amount.For the linear regression model to actually be usable the mean should be very zero (or very close). In our case the mean is an extremely small value that it holds true. null hypothesis: there is no relationship between them. Alternative hypothesis, is there is a relationship between tip amount and day. If the value of the intercept <0.05 it rejects the null hypothesis. If the value of p is greater > 0.05 it does not reject the null hypothesis, meaning that there is no relationship. Even though the p-value is less than 0.05 and the mean of the residuals is close to 0. through the graph and collected data, we did not see a linear regression relationship between these two variables. So we did not feel comfortable using the Linear regression model because some of the assumptions are not met.

Pre-analysis Thought-process: Before testing the relationship between the day of the week and tipping, we thought we would write up what we thought of the relationship. We came to an agreement, saying that the tip percentage will be higher on the weekend than the weekdays. We considered the lifestyle of people in New York and believed that people are more likely to go out of their routine schedule(rather than using their daily transportation to go to work) and be willing to spend more money. In spending more money, we thought this would translate over to tipping taxi drivers. With that, we start our analysis:

Linear regression Model, We are trying to figure out the relationship between Day of the week and the Tip amount. For the linear regression model to actually be using the mean should be very zero (or very close). In our case, the mean is an extremely small value that it holds true.

null hypothesis: there is no relationship between them. The alternative hypothesis: is there is a relationship between tip amount and day. If the value of the intercept is less than 0.05 it rejects the null hypothesis. If the value of p is greater than 0.05, it does not reject the null hypothesis, meaning that there is no relationship. We were also able to identify the mean of the residuals as -9.138805e-17 which passes the assumption that the mean of the residuals needs to be very close to 0 for it to be a possible linear regression relationship.

Scatter plot below is showing Days of the Week vs Dist. Travelled

taxi_tab$day<- factor(taxi_tab$day, levels=c("Monday",
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
viewtaxi_tab<-taxi_tab[order(taxi_tab$day),]


taxi_tab %>% ggplot(aes(x = day, y = trip_distance))+ geom_point() + labs(title = "Day of the Week Vs Distance Travelled")


Using a boxplot to find a relationship between tipping habits and day of the week, we cannot easily identify any obvious trends with the eye test, To take a deeper look towards the data itself, We decided it would be good to create the Further_Testing table to get more exact values.

    Further_testing <- taxi_tab%>% group_by(day) %>% summarize(avg_fare = mean(fare_amount), avg_tip = mean(tip_amount), sum_fare = sum(fare_amount), sum_tip = sum(tip_amount), Count = n() )
  Further_testing
## # A tibble: 7 x 6
##   day       avg_fare avg_tip sum_fare sum_tip  Count
##   <fct>        <dbl>   <dbl>    <dbl>   <dbl>  <int>
## 1 Monday        13.4    1.95 3886709. 567214. 290927
## 2 Tuesday       13.3    1.97 4276528. 636356. 322551
## 3 Wednesday     14.6    1.99 4841590. 658872. 330854
## 4 Thursday      13.9    2.06 3865555. 573691. 277854
## 5 Friday        13.9    2.02 3813674. 553739. 274192
## 6 Saturday      12.7    1.70 3394608. 453745. 266754
## 7 Sunday        13.7    1.89 3237065. 446649. 236868
we can observe that from the percent tip from the overall payment it is slightly smaller on the weekends. That along with not being able to reject the null hypothesis because the p-value is <0.05 and the mean of the residual very close to 0 pushes us to further analyze this relationship. Using this sample, We tidy our residuals to get a better sense of the data. We get clear estimates that Saturday and Sunday see the least tip amounts from any day.

Post-Analysis: Given this information, We can safely assume that there is analytical proof that there is a relationship between the tip amount and whether it’s a weekend/weekday. We see that our pre-analysis assumptions regarding this relationship were incorrect and in fact, the opposite was true. A taxi driver is more likely to get a better tip on a weekday than a weekend.

For further information visit: “http://r-statistics.co/Assumptions-of-Linear-Regression.html”

Now we will graph the boxplot.

  # box plot representing tip amounts compared to the day
  taxi_tab %>% 
  filter(tip_amount>=0) %>%
  ggplot(aes(x = day, y = tip_amount))+ geom_boxplot() + geom_smooth(method = lm)+ labs(title = "Day of the Week Vs Tip amount")


  get_resid <- lm(tip_amount~day, data = taxi_tab)
  #checking the mean of the residuals
  mean(get_resid$residuals)
## [1] 2.268914e-15
  get_resid_tidy <- get_resid %>% tidy() 
  
  augment_lm <- get_resid %>%augment()
  
  augment_lm %>% ggplot(aes(x=day, y =.resid)) +geom_violin() + geom_smooth(method = lm)+ labs(title = "Residuals vs year", x ="day", y = "residual")


  get_resid_tidy %>% knitr :: kable()
term	estimate	std.error	statistic	p.value
(Intercept)	1.9496795	0.0050492	386.138640	0.0000000
dayTuesday	0.0232061	0.0069634	3.332597	0.0008604
dayWednesday	0.0417496	0.0069218	6.031593	0.0000000
dayThursday	0.1150433	0.0072241	15.924916	0.0000000
dayFriday	0.0698509	0.0072487	9.636281	0.0000000
daySaturday	-0.2486944	0.0073006	-34.065012	0.0000000
daySunday	-0.0640349	0.0075370	-8.496055	0.0000000
  augment_lm %>% ggplot(aes(x =factor(.fitted), y =.resid)) +geom_violin() + geom_smooth()+ labs(title = "Residuals vs fitted", x =" fitted", y = "residual")
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'


Taxi information for November of 2018. This is a secondary data set needed for our machine learning part of the project.

url <- "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2018-11.csv"
taxi2 <- read_csv(url)
## Parsed with column specification:
## cols(
##   VendorID = col_double(),
##   tpep_pickup_datetime = col_datetime(format = ""),
##   tpep_dropoff_datetime = col_datetime(format = ""),
##   passenger_count = col_double(),
##   trip_distance = col_double(),
##   RatecodeID = col_double(),
##   store_and_fwd_flag = col_character(),
##   PULocationID = col_double(),
##   DOLocationID = col_double(),
##   payment_type = col_double(),
##   fare_amount = col_double(),
##   extra = col_double(),
##   mta_tax = col_double(),
##   tip_amount = col_double(),
##   tolls_amount = col_double(),
##   improvement_surcharge = col_double(),
##   total_amount = col_double()
## )
Before we start the prediction task analysis, we need to combine the two datasets to make a comparison between the months of October and November.

taxi1 <- taxi_tab %>%
    select(tpep_pickup_datetime, tpep_dropoff_datetime, passenger_count, trip_distance, fare_amount, tip_amount, tolls_amount, PULocationID, DOLocationID) 
taxi2 <- taxi2 %>%
  sample_n(2000000) %>%
    select(tpep_pickup_datetime, tpep_dropoff_datetime, passenger_count, trip_distance, fare_amount, tip_amount, tolls_amount, PULocationID, DOLocationID) 

total <- rbind(taxi1, taxi2)
head(total)
## # A tibble: 6 x 9
##   tpep_pickup_dateti… tpep_dropoff_datet… passenger_count trip_distance
##   <dttm>              <dttm>                        <dbl>         <dbl>
## 1 2018-10-04 15:30:58 2018-10-04 15:58:15               1          2.27
## 2 2018-10-16 20:09:21 2018-10-16 20:25:00               1          2.57
## 3 2018-10-07 19:57:27 2018-10-07 20:08:03               1          1.1 
## 4 2018-10-23 17:57:50 2018-10-23 18:10:27               2          0.87
## 5 2018-10-22 13:25:44 2018-10-22 13:31:29               2          0.62
## 6 2018-10-28 16:50:18 2018-10-28 17:14:08               1          4.37
## # … with 5 more variables: fare_amount <dbl>, tip_amount <dbl>,
## #   tolls_amount <dbl>, PULocationID <dbl>, DOLocationID <dbl>
The prediction task that we are going to respond is:

Can we predict if the tip amount will increase or decrease one month from now?

We are going to use machine learning to answer this question. Even though there are many different methods of machine learning, the method that we are going to use is a random forest with 50 tress. Here is a link to explore different methods of machine learning in R: https://machinelearningmastery.com/machine-learning-in-r-step-by-step/ This website has interesting info on machine learning and explains how to use each method.

First, we will compare the tip-amount for October to November label it as up or down depending on the sign of the difference. The difference would be October - November. In order to do that, we will extract the date from date and time. Then, we extract day and year from date. Lastly, we create the direction varible based on the value of diff, so If the difference is negative, the direction would be up, otherwise the direction would be down.

   total$my <- format(as.Date(total$tpep_pickup_datetime,format="%Y-%m-%d"), format = "%m") 
   total$my <- as.numeric(total$my) 
   total$yy <- format(as.Date(total$tpep_pickup_datetime,format="%Y-%m-%d"), format = "%Y") 
   total$yy <- as.numeric(total$yy) 
   
   total <- mutate(total, dm = my + yy*1000) 
   
   
   wp <- total %>%
   drop_na() %>%
   filter(dm  %in% c("2018010", "2018011")) %>%
   select(PULocationID, dm, tip_amount) %>%
   distinct(PULocationID, dm, .keep_all = TRUE) %>%
   spread(dm, tip_amount)
   names(wp)[names(wp) == "2018010"] <- "October"
   names(wp)[names(wp) == "2018011"] <- "November"
   
   
   wp <- mutate(wp, diff = November - October) 
   wp <- mutate(wp, Direction = ifelse(diff>0, "up", "down"))
   wp <- select(wp, PULocationID, Direction)
   
   head(wp)
## # A tibble: 6 x 2
##   PULocationID Direction
##          <dbl> <chr>    
## 1            1 down     
## 2            2 down     
## 3            3 up       
## 4            4 up       
## 5            5 down     
## 6            6 down
In order to train our model, we tidy the data into a wide dataset using the tidyr::spread function, then we create a data frame that shows the differences for each day in the month of Octobor. This table will be used later in our code.

taxi_tab$tpep_pickup_datetime <- as.Date(taxi_tab$tpep_pickup_datetime)
wide_df <- taxi_tab %>%
  select(  PULocationID, tpep_pickup_datetime , tip_amount) %>%
     distinct(PULocationID, tpep_pickup_datetime, .keep_all = TRUE) %>%
  tidyr::spread(tpep_pickup_datetime, tip_amount)%>%
  select(1,5:35)
wide_df[is.na(wide_df)] <- 0.00
head(wide_df)
## # A tibble: 6 x 32
##   PULocationID `2018-09-30` `2018-10-01` `2018-10-02` `2018-10-03` `2018-10-04`
##          <dbl>        <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
## 1            1            0        10           21.2             0        10   
## 2            2            0         0            0               0         0   
## 3            3            0         1.17         1.77            0         3.5 
## 4            4            0         0            0               0         1.15
## 5            5            0         0            0               0         0   
## 6            6            0         0            0               0         0   
## # … with 26 more variables: `2018-10-05` <dbl>, `2018-10-06` <dbl>,
## #   `2018-10-07` <dbl>, `2018-10-08` <dbl>, `2018-10-09` <dbl>,
## #   `2018-10-10` <dbl>, `2018-10-11` <dbl>, `2018-10-12` <dbl>,
## #   `2018-10-13` <dbl>, `2018-10-14` <dbl>, `2018-10-15` <dbl>,
## #   `2018-10-16` <dbl>, `2018-10-17` <dbl>, `2018-10-18` <dbl>,
## #   `2018-10-19` <dbl>, `2018-10-20` <dbl>, `2018-10-21` <dbl>,
## #   `2018-10-22` <dbl>, `2018-10-23` <dbl>, `2018-10-24` <dbl>,
## #   `2018-10-25` <dbl>, `2018-10-26` <dbl>, `2018-10-27` <dbl>,
## #   `2018-10-28` <dbl>, `2018-10-29` <dbl>, `2018-10-30` <dbl>
Now we create a matrix from the wide dataset then we inner join the new matrix with wp dataset(which gives the direction )

matrix <- wide_df %>%
  as.matrix()

ab_df <- matrix%>%
  magrittr::set_colnames(NULL) %>%
  as_data_frame() %>%
  mutate(PULocationID= wide_df$PULocationID)
## Warning: `as_data_frame()` is deprecated as of tibble 2.0.0.
## Please use `as_tibble()` instead.
## The signature and semantics have changed, see `?as_tibble`.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
## Warning: The `x` argument of `as_tibble.matrix()` must have column names if `.name_repair` is omitted as of tibble 2.0.0.
## Using compatibility `.name_repair`.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
final_df <- ab_df %>%
  inner_join(wp %>% select(PULocationID, Direction), by="PULocationID") %>%
  mutate(Direction=factor(Direction, levels=c("down", "up")))%>%
  select(-PULocationID) %>%
  na.omit()

head(final_df)
## # A tibble: 6 x 33
##      V1    V2    V3    V4    V5    V6    V7    V8    V9   V10   V11   V12   V13
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1     1     0 10    21.2      0 10     26.6   0   24.3   0    24.6      0    28
## 2     2     0  0     0        0  0      0     0    1.76  0     0        0     0
## 3     3     0  1.17  1.77     0  3.5    0    10.9  0     0     0        0     0
## 4     4     0  0     0        0  1.15   0     1.1  1     1.36  1.15     0     0
## 5     5     0  0     0        0  0      0     0    0     0     0        0     0
## 6     6     0  0     0        0  0      0     0    0     0     0        0     0
## # … with 20 more variables: V14 <dbl>, V15 <dbl>, V16 <dbl>, V17 <dbl>,
## #   V18 <dbl>, V19 <dbl>, V20 <dbl>, V21 <dbl>, V22 <dbl>, V23 <dbl>,
## #   V24 <dbl>, V25 <dbl>, V26 <dbl>, V27 <dbl>, V28 <dbl>, V29 <dbl>,
## #   V30 <dbl>, V31 <dbl>, V32 <dbl>, Direction <fct>
The pervious codes were performed to get the necessary information to the test. This is the most important part of the machine learning, which is running the test to make the prediction. We will use 10-fold cross-validation to find out how many mtry is enough to give a decent prediction. Since the data set is big, we are using 10-fold-cross validation to make the validation sets have more examples.

https://machinelearningmastery.com/machine-learning-in-r-step-by-step/

set.seed(1234)

cv_partition <- createFolds(final_df$Direction,
                            k=10, returnTrain = TRUE)

fit_control <- trainControl( 
  method = "cv",
  number = 10,
  index = cv_partition,
  #indexOut = cv_partition,
  summaryFunction=twoClassSummary,
  classProbs=TRUE,
  savePredictions=TRUE)


fit <- train(Direction~.,
                    data= final_df,
                    method = "rf",
                    ntree = 50,
                    trControl = fit_control,
                    metric="ROC")

fit
## Random Forest 
## 
## 256 samples
##  32 predictor
##   2 classes: 'down', 'up' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 230, 231, 231, 230, 230, 230, ... 
## Resampling results across tuning parameters:
## 
##   mtry  ROC        Sens       Spec      
##    2    0.6460109  0.9842105  0.02857143
##   17    0.6215957  0.8988304  0.17142857
##   32    0.6108187  0.8713450  0.15952381
## 
## ROC was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 2.
curve_df <-
  fit$pred %>%
    filter(mtry == 32)

curve_df %>%
  ggplot(aes(m=up,d=factor(obs, levels=c("up","down")))) +
    geom_roc() +
    coord_equal() +
    style_roc() 
## Warning in verify_d(data$d): D not labeled 0/1, assuming down = 0 and up = 1!


Based on the roc curve, we can see the our curve is very similar to the diagonal line in terms of shape. This indicates that our test is useless and it cannot be used to predict the tip amount of one month based on the previous month.

Note that the echo = FALSE parameter was added to the code chunk to prevent printing of the R code that generated the plot.
