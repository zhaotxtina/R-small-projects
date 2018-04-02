# read a new file
airquality = read.csv('airquality.csv')
head(airquality)
View(airquality)
library(magrittr)

# number of rows
airquality %>% nrow()

# count the missing values
airquality %>% is.na() %>% sum()

# get the location of each missing value
airquality %>% is.na() %>% which()

library(dplyr)
airquality %>% summarise(mean(Ozone))

airquality %>% summarise(mean(Ozone, na.rm = TRUE))

# complete cases
airquality1 = airquality %>% filter(complete.cases((airquality)))

airquality2 = airquality %>% na.omit()

class(airquality2)
str(airquality)

# non numeric 
airquality %>% summarise(mean(Ozone, na.rm=TRUE))
class(airquality$Ozone) = as.character(airquality$Ozone)
class(airquality$Ozone)
airquality %>% summarise(mean(Ozone, na.rm=TRUE))
airquality$Ozone[1]= 41.0
mean(summarise(airquality$Ozone))

(airquality$Ozone) = as.numeric(airquality$Ozone)


airquality4 = airquality %>% mutate(Ozone_char = as.character(Ozone, na.rm = TRUE))

airquality4 = airquality %>% mutate(new_col = Wind+ Temp, new_date=paste(Month,"_",Day))

      