library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(dplyr)

#q1.1
#read the csv files
pu2014 <- read.csv('Twitter Data File_pu2014.csv', header = TRUE)

#selecting specific column
pu2014c01 <- pu2014[, c('epoch', 'weekday', 'hour','month','day','year', 
                        'longitude','latitude','lang','user_id','uear_year',
                        'user_followers_count', 'user_friends_count')]
#run first five row of data
head(pu2014c01, 5)

#conduct a summary
summary(pu2014c01)
#change the missing data to NA
pu2014c01[pu2014c01 == ""] <-NA

#convert epoch time to datetime. 
pu2014c01$datetime <- as.POSIXct(pu2014c01$ep, origin = "1970-01-01")
head(pu2014c01, 5)



#q1.2
#find distinct users in this table
distinct_user <- unique(pu2014c01$user_id)
num_distinct_user <- length(distinct_user)

cat('Distinct Twitter Users in this table:', num_distinct_user,"\n")

usertweet_count <- as.data.frame(table(pu2014c01$user_id))
names(usertweet_count) <- c('user_id','freq')
usertweet_count <- usertweet_count[order(usertweet_count$freq, decreasing=TRUE),]
#do the plot with descending 
plot(usertweet_count$freq, col = 'red', ylab = 'Frequency', xlab = 'user',
     main = "tweets frequency")


#show that small amount of people tweet the most tweets. 
#draw the plot again
plot(usertweet_count$freq, col = 'blue', ylab = 'Frequency', xlab = 'user',
     main = "tweets frequency with 50% line")

# calculate the cumulative sum of tweet frequencies
cumulative_freq <- cumsum(usertweet_count$freq)

# find the total amount of tweet
total_tweets <- sum(usertweet_count$freq)
# find the 50 percent of the tweet is at
threshold <- total_tweets * 0.5
index_50_percent <- which(cumulative_freq >= threshold)[1]

# Draw a vertical line at the 50% point
abline(v = index_50_percent, col = 'green', lty = 2)#dashed lines
#the above will show the line at the left side which represents that most tweets
#were did by the small amount of users. 

#q1.3



#create a new column to categorize Weekdays and Weekends from weekday column
pu2014c01 <- pu2014c01 %>%
  
  #put into two categories Weekday and Weekend, Caps enabled here. 
mutate(days = ifelse(weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"),
                             "Weekday", "Weekend"))

#summarize the number of tweets for each category
tweet_summary <- pu2014c01 %>%
group_by(days) %>% #summary operation.
summarize(total_tweets = n())

#create a bar plot to visualize the comparison
ggplot(tweet_summary, aes(x = days, y = total_tweets, fill = days)) +
geom_bar(stat = "identity") + labs( title = "Number of Tweets on Weekdays vs. Weekends",x = "Day Category",y = "Total Tweets")+ 
  theme_classic()#classic plot


#better one with each days
pu2014c01$day_category <- factor(pu2014c01$weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

#summarize the number of tweets for each day
tweet_summary <- pu2014c01 %>%
  group_by(day_category) %>%
  summarize(total_tweets = n())

#create a bar plot to visualize the comparison
ggplot(tweet_summary, aes(x = day_category, y = total_tweets, fill = day_category)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Tweets by Day of the Week",
    x = "Day of the Week",
    y = "Total Tweets"
  ) +
  theme_minimal()#different styles

#create a bar plot of the number of tweets per hour
ggplot(pu2014c01, aes(x = hour)) +
  geom_bar() +
  labs(
    title = "Number of Tweets by Each Hour",
    x = "Hour of the Day",
    y = "Total Tweets"
  ) +
  theme_classic()


#sort the data by user and datetime
pu2014c01 <- pu2014c01[order(pu2014c01$user_id, pu2014c01$datetime), ]

#calculate the time gap between consecutive tweets for each user in seconds
pu2014c01$time_gap <- c(0, diff(pu2014c01$datetime))  # Use 0 for the first tweet of each user

#do it in minutes
user_avg_time_gap <- aggregate(time_gap ~ user_id, data = pu2014c01, FUN = function(x) mean(x) / 60)

#remove zero or negative time gaps
user_avg_time_gap <- user_avg_time_gap[user_avg_time_gap$time_gap > 0, ]

#create a histogram to visualize the average time gaps in log scale
hist(log(user_avg_time_gap$time_gap), col = 'blue', xlab = 'Average Time Gap (log, min)',
     ylab = 'Number of Users', main = 'Average Time Gap Between Tweets (log, min)')



# draw a heat map to shows the location fo tweets
#extra: Create a leaflet map
twitmap <- leaflet() %>%
  setView(lng = mean(pu2014c01$longitude), lat = mean(pu2014c01$latitude), zoom = 5)

#add the OpenStreetMap base layer
twitmap <- twitmap %>%
  addTiles()

#add the heatmap layer
twitmap <- twitmap %>%
  addHeatmap(
    data = pu2014c01,
    lat = ~latitude,
    lng = ~longitude,
    radius = 5,  #set the radius, can be changed in the future
    blur = 5    #set the blur, can be changed in the future
  )

#visualize the result on the map
twitmap













