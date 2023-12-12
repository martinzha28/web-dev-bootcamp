library(MASS)

#1
mydata <- read.csv("C:/Users/Martin Zhao/Documents/Waterloo/stat231dataset20936386.csv")

user1 <- subset(mydata, subset = (username == "@JohnTory"))
user2 <- subset(mydata, subset = (username == "@CTVKitchener"))

# 1b
table(factor(user1$day.of.week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
table(factor(user2$day.of.week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

# 1c
barplot(table(factor(user1$day.of.week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))), 
         xlab = "Day of Week", ylab = "Frequency", las = 1, main = "Bar Plot of @JohnTory Tweets separated by Day of Week", 
         col = c("dodgerblue3", "darkorchid2", "forestgreen", "orange"), ylim = c(0,40), density = 25)
barplot(table(factor(user2$day.of.week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))), 
        xlab = "Day of Week", ylab = "Frequency", las = 1, main = "Bar Plot of @CTVKitchener Tweets separated by Day of Week", 
        col = c("dodgerblue3", "darkorchid2", "forestgreen", "orange"), ylim = c(0,60), density = 25)

# 2
mydata$time.of.day.hour <- mydata$time.of.day/3600
user1 <- subset(mydata, subset = (username == "@JohnTory"))
user2 <- subset(mydata, subset = (username == "@CTVKitchener"))

#2b
user1.mean <- mean(user1$time.of.day.hour)
user2.mean <- mean(user2$time.of.day.hour)

median(user1$time.of.day.hour)
median(user2$time.of.day.hour)

user1.sd <- sd(user1$time.of.day.hour)
user2.sd <- sd(user2$time.of.day.hour)

skewness <- function(x) {(sum((x - mean(x))^3)/length(x))/(sum((x - mean(x))^2)/length(x))^(3/2)}
kurtosis <- function(x) {(sum((x - mean(x))^4)/length(x))/(sum((x - mean(x))^2)/length(x))^2}
skewness(user1$time.of.day.hour)
skewness(user2$time.of.day.hour)
kurtosis(user1$time.of.day.hour)
kurtosis(user2$time.of.day.hour)

#2c
truehist(user1$time.of.day.hour, xlab = "Hour of Day", ylab = "Relative Frequency", 
         main = "Relative Frequency Histogram of @JohnTory Tweets at Hour of Day", 
         ylim = c(0,0.15), las = 1, col = "dodgerblue3", density = 25, angle = 45, nbins = 30)
curve(dnorm(x, user1.mean, user1.sd), col = "red", add = TRUE, lwd = 1.5)
truehist(user2$time.of.day.hour, xlab = "Hour of Day", ylab = "Relative Frequency", 
         main = "Relative Frequency Histogram of @CTVKitchener Tweets at Hour of Day", 
         ylim = c(0,0.06), las = 1, col = "dodgerblue3", density = 25, angle = 45, nbins = 24)
curve(dnorm(x, user2.mean, user2.sd), col = "red", add = TRUE, lwd = 1.5)

#2d
plot(ecdf(user1$time.of.day.hour), xlab = "Hour of Day", ylab = "Fn(x)", 
     main = "e.c.d.f. of @JohnTory Tweets at Hour of Day", las = 1, lwd = 2, pch = NA)
curve(pnorm(x, user1.mean, user1.sd), col = "red", add = TRUE, lwd = 1.5, lty = 2)

plot(ecdf(user2$time.of.day.hour), xlab = "Hour of Day", ylab = "Fn(x)", 
     main = "e.c.d.f. of @CTVKitchener Tweets at Hour of Day", las = 1, lwd = 2, pch = NA)
curve(pnorm(x, user2.mean, user2.sd), col = "red", add = TRUE, lwd = 1.5, lty = 2)

#3b
user1$likes.log <- log(user1$likes + 1)

boxplot(user1$likes.log[user1$time.of.day.hour < 12],user1$likes.log[user1$time.of.day.hour >= 12],
        col = c("dodgerblue1","darkorchid4"), xlab = "Time of day", ylab = "Likes", 
        main = "@JohnTory Likes and Tweet Time",ylim = c(0,8.5), names = c("Before Noon", "At or After Noon"))

#3c

weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday","Sunday")

boxplot(user1$likes.log[user1$day.of.week %in% weekday],user1$likes.log[user1$day.of.week %in% weekend],
        col = c("dodgerblue1","darkorchid4"), xlab = "Day of Week", ylab = "Likes", 
        main = "@JohnTory Likes and Tweet Date",ylim = c(0,8.5), names = c("Weekday", "Weekend"))