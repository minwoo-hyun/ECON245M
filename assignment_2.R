library(tidyverse)
library(readr)

# 1
airbnb = read_csv('assign_2.csv')
View(airbnb)
colnames(airbnb)
airbnb <- airbnb %>%
  rename(neighborhood = neighbourhood)


# 2(a)(b)
neighborhoods <- airbnb %>% 
  count(neighborhood) %>% 
  filter(is.na(neighborhood)==FALSE) %>% 
  arrange(desc(n)) %>% 
  head(20)


# 2(c)
airbnb_top_neighborhoods <- airbnb %>% 
  filter(neighborhood %in% neighborhoods$neighborhood)

# 2(d)

summary_stats_top_neighborhoods <- airbnb_top_neighborhoods %>% 
  group_by(neighborhood) %>% 
  summarise(avg_square_feet = mean(square_feet,na.rm=TRUE), 
            avg_price = mean(price,na.rm=TRUE), 
            sd_price = sd(price,na.rm=TRUE), 
            max_price = max(price,na.rm = TRUE), 
            min_price = min(price,na.rm=TRUE)) %>% 
  arrange(desc(avg_square_feet))

# 2(e)
highest_avg_square_ft <- summary_stats_top_neighborhoods %>% 
  select(avg_square_feet) %>% 
  slice(1) %>% 
  pull(avg_square_feet)

# 2(f)
second_avg_square_ft <- summary_stats_top_neighborhoods %>% 
  select(avg_square_feet) %>% 
  slice(2) %>% 
  pull(avg_square_feet)

