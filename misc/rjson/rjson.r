# read json in R 
# data file: "data_json.txt"

install.packages("rjson")
require(rjson)


sleep_data <- rjson::fromJSON(file = "data_json.txt")
str(sleep_data)


for (sleep_record in sleep_data)
{
  print(paste("Start:", sleep_record$start))
  print(paste("Stop:", sleep_record$stop))
  print(paste("number of events:", length(sleep_record$events)))
  se <- matrix(unlist(sleep_record$events), ncol = 3, byrow = TRUE)
  sleep_record$num_sleepevents <- length(sleep_record$events)
  # sleep_record$events <- NULL
}

print("first few records from sleep events list")
head(se)


# summarising sleepevents into a new df
lapply(X = sleep_data, FUN = function(x) { 
  return (  data.frame( start = x$start, 
                        stop = x$stop, 
                        num_sleepevents = length(x$events)))
            })
