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

}

print("first few records from sleep events list")
head(se)
