library(purrr)
library(fs)
library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)


#installing data
living_index <- read.csv("C:\\Users\\12703\\Downloads\\advisorsmith_cost_of_living_index.csv")
living_index$place <- paste0(living_index$City, ", ", living_index$State)




hundred_twofifty <- read.csv ("C:\\Users\\12703\\downloads\\crime_100_250.csv")
hundred_twofifty$states <- state.abb[match(hundred_twofifty$states, state.name)]
hundred_twofifty$place <- paste0(hundred_twofifty$cities, ", ", hundred_twofifty$states)




forty_sixty <- read.csv ("C:\\Users\\12703\\downloads\\crime_40_60.csv")
forty_sixty$states <- state.abb[match(forty_sixty$states, state.name)]
forty_sixty$place <- paste0(forty_sixty$cities, ", ", forty_sixty$states)




sixty_hundred <- read.csv ("C:\\Users\\12703\\downloads\\crime_60_100.csv")
sixty_hundred$states <- state.abb[match(sixty_hundred$states, state.name)]
sixty_hundred$place <- paste0(sixty_hundred$cities, ", ", sixty_hundred$states)




twofifty_plus <- read.csv ("C:\\Users\\12703\\downloads\\crime_250_plus.csv")
twofifty_plus$states <- stringr::str_squish(twofifty_plus$states)
twofifty_plus$states <- state.abb[match(twofifty_plus$states, state.name)]
twofifty_plus$place <- paste0(twofifty_plus$cities, ", ", twofifty_plus$states)

#sorting data by place
living_index <- left_join(living_index, twofifty_plus, by = "place", 
                          suffix = c("_li", "_tf"))

living_index <- left_join(living_index, hundred_twofifty[, -c(1:3)], by = "place",
                          suffix = c("_li", "_h2"))

living_index <- left_join(living_index, forty_sixty, by = "place",
                      suffix = c("_li", "_fs"))

living_index <- left_join(living_index, sixty_hundred, by = "place",
                      suffix = c("_li", "_sh"))

living_index$murder <- coalesce(living_index$murder_li, 
                                as.numeric(living_index$murder_h2), 
                                as.numeric(living_index$murder_sh))



library(dplyr)

living_index <- replace(living_index, is.na(living_index),0)

living_index$population <-sum(living_index$population.x, living_index$population.y, living_index$population.x.1, living_index$population.y.1)


#removing commas 
living_index$Cost.of.Living.Index <- gsub(",", "", living_index$Cost.of.Living.Index)
living_index$population_li <- gsub(",", "", living_index$population_li)
living_index$total_crime <- gsub(",", "", living_index$total_crime)
living_index$murder_li<- gsub(",", "", living_index$murder_li)
living_index$rape_li <- gsub(",", "", living_index$rape_li)
living_index$robbery_li <- gsub(",", "", living_index$robbery_li)
living_index$agrv_assault_li <- gsub(",", "", living_index$agrv_assault_li)
living_index$tot_violent_crime <- gsub(",", "", living_index$tot_violent_crime)
living_index$burglary_li <- gsub(",", "", living_index$burglary_li)
living_index$larceny_li <- gsub(",", "", living_index$larceny_li)
living_index$vehicle_theft_li <- gsub(",", "", living_index$vehicle_theft_li)
living_index$tot_prop_crim <- gsub(",", "", living_index$tot_prop_crim)
living_index$arson <- gsub(",", "", living_index$arson)
living_index$violent_crime_li <- gsub(",", "", living_index$violent_crime_li)
living_index$murder_h2 <- gsub(",", "", living_index$murder_h2)
living_index$rape_h2 <- gsub(",", "", living_index$rape_h2)
living_index$robbery_h2 <- gsub(",", "", living_index$robbery_h2)
living_index$agrv_assault_h2 <- gsub(",", "", living_index$agrv_assault_h2)
living_index$prop_crime_li <- gsub(",", "", living_index$prop_crime_li)
living_index$burglary_h2 <- gsub(",", "", living_index$burglary_h2)
living_index$larceny_h2 <- gsub(",", "", living_index$larceny_h2)
living_index$vehicle_theft_h2 <- gsub(",", "", living_index$vehicle_theft_h2)
living_index$population_fs <- gsub(",", "", living_index$population_fs)
living_index$violent_crime_fs <- gsub(",", "", living_index$violent_crime_fs)
living_index$murder_li_li <- gsub(",", "", living_index$murder_li_li)
living_index$rape_li_li <- gsub(",", "", living_index$rape_li_li)
living_index$robbery_li_li <- gsub(",", "", living_index$robbery_li_li)
living_index$agrv_assault_li_li <- gsub(",", "", living_index$agrv_assault_li_li)
living_index$prop_crime_fs <- gsub(",", "", living_index$prop_crime_fs)
living_index$burglary_li_li <- gsub(",", "", living_index$burglary_li_li)
living_index$larceny_li_li <- gsub(",", "", living_index$larceny_li_li)
living_index$vehicle_theft_li_li <- gsub(",", "", living_index$vehicle_theft_li_li)
living_index$population <- gsub(",", "", living_index$population)
living_index$violent_crime <- gsub(",", "", living_index$violent_crime)
living_index$murder_sh <- gsub(",", "", living_index$murder_sh)
living_index$rape_sh <- gsub(",", "", living_index$rape_sh)
living_index$robbery_sh <- gsub(",", "", living_index$robbery_sh)
living_index$agrv_assault_sh <- gsub(",", "", living_index$agrv_assault_sh)
living_index$larceny_sh <- gsub(",", "", living_index$larceny_sh)
living_index$burglary_sh <- gsub(",", "", living_index$burglary_sh)
living_index$Cost.of.Living.Index <- gsub(",", "", living_index$Cost.of.Living.Index)
living_index$prop_crime <- gsub(",", "", living_index$prop_crime)



living_index$place <- NULL
living_index$states_li <- NULL
living_index$cities_li <- NULL
living_index$states_fs <- NULL
living_index$cities_fs <- NULL
living_index$cities <- NULL
living_index$states <- NULL


living_index <- living_index %>%
  mutate(population_clean = coalesce(population_li, population_fs, population))
living_index$population_li <- NULL
living_index$population_fs <- NULL
living_index$population <- NULL


living_index <- living_index %>%
  mutate(murder_clean = coalesce(murder_li, murder_sh, murder_li_li, murder_h2))
living_index$murder_li <- NULL
living_index$murder_li_li <- NULL
living_index$murder_h2 <- NULL
living_index$murder_sh <- NULL




living_index <- living_index %>%
  mutate(rape_clean = coalesce(rape_li, rape_h2, rape_li_li, rape_sh))
living_index$rape_li <- NULL
living_index$rape_li_li <- NULL
living_index$rape_h2 <- NULL
living_index$rape_sh <- NULL



living_index <- living_index %>%
  mutate(robbery_clean = coalesce(robbery_li, robbery_h2, robbery_li_li, robbery_sh))
living_index$robbery_li <- NULL
living_index$robbery_h2 <- NULL
living_index$robbery_li_li <- NULL
living_index$robbery_sh <- NULL



living_index <- living_index %>%
  mutate(agrv_assault_clean = coalesce(agrv_assault_h2, agrv_assault_li, agrv_assault_li_li, agrv_assault_sh))
living_index$agrv_assault_li <- NULL
living_index$agrv_assault_h2 <- NULL
living_index$agrv_assault_li_li <- NULL
living_index$agrv_assault_sh <- NULL

living_index <- living_index %>%
  mutate(burglary_clean = coalesce(burglary_li, burglary_h2, burglary_li_li, burglary_sh))
living_index$burglary_li <- NULL
living_index$burglary_h2 <- NULL
living_index$burglary_li_li <- NULL
living_index$burglary_sh <- NULL


living_index <- living_index %>%
  mutate(larceny_clean = coalesce(larceny_li, larceny_h2, larceny_li_li, larceny_sh))
living_index$larceny_li <- NULL
living_index$larceny_h2 <- NULL
living_index$larceny_li_li <- NULL
living_index$larceny_sh <- NULL



living_index <- living_index %>%
  mutate(vehicle_theft_clean = coalesce(vehicle_theft_li, vehicle_theft_h2, vehicle_theft_li_li, vehicle_theft_sh))
living_index$vehicle_theft_li <- NULL
living_index$vehicle_theft_h2 <- NULL
living_index$vehicle_theft_li_li <- NULL
living_index$vehicle_theft_sh <- NULL

living_index <- living_index %>%
  mutate(arson_clean = coalesce(arson))
living_index$arson <- NULL


living_index <- living_index %>%
  mutate(violent_crime_clean = coalesce(tot_violent_crime,violent_crime_li, violent_crime_fs, violent_crime))
living_index$violent_crime_li <- NULL
living_index$violent_crime_fs <- NULL
living_index$violent_crime <- NULL


living_index <- living_index %>%
  mutate(prop_cime_clean = coalesce(prop_crime, prop_crime_li, prop_crime_fs, tot_prop_crim))
living_index$prop_crime_li<- NULL
living_index$prop_crime_fs <- NULL
living_index$prop_crime <- NULL
living_index$tot_prop_crim <- NULL

living_index <- living_index %>% filter(population_clean != 0)


living_index$agrv_assault_clean <- NULL
living_index$violent_crime_clean <- NULL
living_index$prop_cime_clean <- NULL
living_index$murder <- NULL


#sums for specific crimes to observe the most common amongst all the cities
total_murder_clean <- sum(as.numeric(living_index$murder_clean), na.rm = TRUE)
print(total_murder_clean)

total_rape_clean <- sum(as.numeric(living_index$rape_clean), na.rm = TRUE)
print(total_rape_clean)

total_robbery_clean <- sum(as.numeric(living_index$robbery_clean), na.rm = TRUE)
print(total_robbery_clean)

total_burglary_clean <- sum(as.numeric(living_index$burglary_clean), na.rm = TRUE)
print(total_burglary_clean)

total_larceny_clean <- sum(as.numeric(living_index$larceny_clean), na.rm = TRUE)
print(total_larceny_clean)

total_vehicle_theft_clean <- sum(as.numeric(living_index$vehicle_theft_clean), na.rm = TRUE)
print(total_vehicle_theft_clean)

total_arson_clean <- sum(as.numeric(living_index$arson_clean), na.rm = TRUE)
print(total_arson_clean)

#observing top 3 most common crimes

# Top 5 cities for larceny_clean
top_larceny <- living_index %>%
  arrange(desc(larceny_clean)) %>%
  select(City, State, larceny_clean) %>%
  head(5)

# Bottom 5 cities for larceny_clean
bottom_larceny <- living_index %>%
  arrange(larceny_clean) %>%
  select(City, State, larceny_clean) %>%
  head(5)

# Top 5 cities for burglary_clean
top_burglary <- living_index %>%
  arrange(desc(burglary_clean)) %>%
  select(City, State, burglary_clean) %>%
  head(5)

# Bottom 5 cities for burglary_clean
bottom_burglary <- living_index %>%
  arrange(burglary_clean) %>%
  select(City, State, burglary_clean) %>%
  head(5)

# Top 5 cities for vehicle_theft_clean
top_vehicle_theft <- living_index %>%
  arrange(desc(vehicle_theft_clean)) %>%
  select(City, State, vehicle_theft_clean) %>%
  head(5)

# Bottom 5 cities for vehicle_theft_clean
bottom_vehicle_theft <- living_index %>%
  arrange(vehicle_theft_clean) %>%
  select(City, State, vehicle_theft_clean) %>%
  head(5)




write.csv(living_index, "living_index.csv", row.names = FALSE)


