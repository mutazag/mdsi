## trying tidyr transpose (spread) function 


# install.packages("dplyr")
# install.packages("tidyr")

library(tidyr)
library(dplyr)


## example from stackoverflow
## https://stackoverflow.com/questions/34004008/transposing-in-dplyr#_=_

df = structure(list(HEADER = c("HOME_TRPM", "AWAY_TRPM", "HOME_TEAM","AWAY_TEAM"),
                    price = c("0.863104076023855", "-0.845186446996287","CHA", "NOP")),
               .Names = c("HEADER", "price"), row.names = c(NA, 4L), class = "data.frame")

df %>% spread(HEADER, price)

df %>% mutate(group = 1) %>% spread(HEADER, price) %>% select (-group)


# my example 

s <- c("abc", "efg")
ss_df <- data.frame(text = s)
ss_df
ss_df[1,]
ss_df$color = c("Red", "Black")
ss_df


# add a couple more rows
ss2 <- ss_df %>% rbind(data.frame(text = c("abc", "efg"), color = c("Green", "Red")))   
ss2 <- ss2 %>% rbind(data.frame(text = c("abc", "efg"), color = c("Yellow", "Red")))                
# notice how efg/red appears twice
ss2

# add a color code quanitative variable so later we can sum it to count how many
# times a color appears, group by text,color combination, then summarise to sum
# color_code variable
ss3 <- ss2 %>% mutate(color_code = 1) %>% group_by(text, color) %>% summarise(color_code = sum(color_code))

# transpose now
ss4 <- ss3 %>% spread(color, color_code)

ss4

# this one will fail since there is still grouping on the data frame 
ss4 %>% select(-text) %>% colSums(na.rm = TRUE)
# so ungroup first, remove the non numberic variables and then calc col sums
# then gathe to produce a long df of color sums
ss5 <- ss4 %>% ungroup() %>% 
  select(-text) %>% 
  colSums(na.rm = TRUE)  
  
ss5 %>% gather("color", "count")
ss5
?colSums
