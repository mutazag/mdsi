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




head(df)

df_historical <- df 

featurise <- function (x, hh){
  # return(paste("site: ", x$site, " year: ", x$year, " day ", x$day, " temp ", x$temp))
  # print("run")
  # print(x)
  hh %>% filter(site == x$site ) %>%
    arrange(desc(year), desc(day)) -> h
  which(h$year == x$year & h$day == x$day) -> i

  h %>% filter(row_number() > i ) %>% head(3) %>% summarise(m3 = mean(temp)) -> x$m3
  h %>% filter(row_number() > i ) %>% head(6) %>% summarise(m6 = mean(temp)) -> x$m6
  # print(x)
  return(x)
}
df$m3 <- NA
df$m6 <- NA


df<-as.data.frame(df)
lapply(df, featurise, hh = df)
mapply(df, featurise, hh= df)
df2 <- data_frame(site=df$site, year=df$year, day=df$day, df$temp, df$m3, df$m6)
apply(df, nrow(df), featurise, hh=df)
df2 %>% mutate_all(funs(featurise(.,hh=df2)))


for(i in 1:nrow(df)){
  x <- df[i,]
  df[i,] <- featurise(x, df)
}
