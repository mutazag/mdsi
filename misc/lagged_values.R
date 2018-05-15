# https://stackoverflow.com/questions/11389415/calculating-lagged-values

library(zoo)

#Start with a simple one column matrix
x <- matrix(1:6, ncol = 1)

#Compute the diffence, pad with an NA at the front, and then divide by x
cbind(x, c(NA, diff(x))/x)


# https://danieljhocking.wordpress.com/2014/12/03/lags-and-moving-means-in-dplyr/
df = expand.grid(site = factor(seq(2)),
                 year = 2000:2004,
                 day = 1:4)
# use Poisson to make math easy to check moving means of temperature
df$temp = rpois(dim(df)[1], 5)


# moving mean for that day and previous days (e.g. 5 represents the mean of that day and the for previous days)
df2 = df %>%
  group_by(site, year) %>%
  arrange(site, year, day) %>%
  mutate(temp.5 = rollmean(x = temp, 5, align = "right", fill = NA))
head(df2, 75)

# moving mean for the previous days not including the current day (e.g. 5 represents the mean of the 5 previous days)
df3 = df %>% group_by(site) %>% arrange(site, year,day) %>%
  mutate(temp.lag1 = lag(temp, n = 1)) %>% 
  mutate(temp.5.previous = zoo::rollapply(data = temp.lag1, 
                                     width = 5, 
                                     FUN = mean, 
                                     align = "right", 
                                     fill = NA, 
                                     na.rm = T))
head(df2, 75)
