library(magrittr)

##### Part 1
### 1-a)
library(readr)
airbnb = readr::read_csv('~/Dropbox/Mac/Documents/minwoo/ucsb/2022f/245M/homework/assign_2.csv')

### 1-b)
View(airbnb)
colnames(airbnb)

### 1-c)
airbnb %<>% rename(neighborhood = neighbourhood)

##### Part 2
### 2-a)
neighborhoods = airbnb %>%
  group_by(neighborhood) %>%
  summarise(n = sum(n())) %>%
  ungroup()

### 2-b)
neighborhoods %<>%
  filter(!is.na(neighborhood)) %>%
  arrange(desc(n)) %>%
  head(20)

### 2-c)
airbnb_top_neighborhoods = airbnb %>%
  filter(neighborhood %in% neighborhoods$neighborhood)

### 2-d)
summary_stats_top_neighborhoods = airbnb_top_neighborhoods %>%
  group_by(neighborhood) %>%
  summarise(avg_square_feet = mean(square_feet, na.rm = T),
            across(c(price), list(avg = ~ mean(.x, na.rm = T), sd = ~ sd(.x, na.rm = T), max = ~ max(.x, na.rm = T), min = ~ min(.x, na.rm = T)), .names = "{.fn}_{.col}")) %>%
  ungroup() %>%
  arrange(desc(avg_square_feet))

# No, the price is not proportional to the quare footage.

### 2-e)
highest_avg_square_ft = summary_stats_top_neighborhoods %>%
  slice(c(1)) %>%
  pull(avg_square_feet)

### 2-f)
second_avg_price = summary_stats_top_neighborhoods %>%
  arrange(desc(avg_price)) %>%
  slice(c(2)) %>%
  pull(avg_price)







