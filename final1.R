# Final 1 Code
# Robin Hollingsworth, 3010287

PERMID <- 3010287

library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(tidyr)
library(dplyr)

f1_dat <- read_csv("final1.csv")

# 1
f1_unique <- distinct(f1_dat)
total_response <- nrow(f1_unique)

# 2
cleaned_min <- function(x){
  x <- gsub("Prefer*",NA,x)
  x <- gsub("\\-.*", "", x)
  x <- gsub("\\,", "", x)
  gsub("\\$","", x) %>% as.numeric()
}

# 3
cleaned_max <- function(x){
  x <- gsub("Prefer*",NA,x)
  x <- gsub(".*\\-", "", x)
  x <- gsub(" [A-z]*", "", x)
  x <- gsub("\\,", "", x)
  gsub("\\$","", x) %>% as.numeric()
}

# 4 
cleaned_mid <- function(x){
  low <- cleaned_min(x)
  high <- cleaned_max(x)
  (high + low) / 2
}

# 5
cleaned_col <- function(data_vector, output_type){
  if (output_type == "min") {
    col <- cleaned_min(data_vector)
  }
  else if (output_type == "mid") {
    col <- cleaned_mid(data_vector)
  }
  else {
    col <- cleaned_max(data_vector)
  }
  return(col)
}

# 6
max_avg_age <- f1_unique$age %>% cleaned_col('max') %>% mean(na.rm=TRUE)
min_avg_age <- f1_unique$age %>% cleaned_col('min') %>% mean(na.rm=TRUE)
mid_avg_age <- f1_unique$age %>% cleaned_col('mid') %>% mean(na.rm=TRUE)

# 7
columns <- list(f1_unique$spend_food_drink,
              f1_unique$spend_private,
              f1_unique$spend_clothes,
              f1_unique$spend_transportation,
              f1_unique$spend_donations,
              f1_unique$spend_other,
              f1_unique$spend_food_drink_total,
              f1_unique$spend_entertainment_total,
              f1_unique$spend_shopping_total,
              f1_unique$spend_travel_total)

mean_min <- c()
mean_mid <- c()
mean_max <- c()

for (col in columns){
  mean_min %<>% append(col %>% cleaned_col('min') %>% mean(na.rm=TRUE))
  mean_mid %<>% append(col %>% cleaned_col('mid') %>% mean(na.rm=TRUE))
  mean_max %<>% append(col %>% cleaned_col('max') %>% mean(na.rm=TRUE))
}

variable <- c("spend_food_drink", 
              "spend_private", 
              "spend_clothes", 
              "spend_transportation",
              "spend_donations", 
              "spend_other", 
              "spend_food_drink_total", 
              "spend_entertainment_total",
              "spend_shopping_total", 
              "spend_travel_total")

avgs <- tibble(variable, mean_min, mean_mid, mean_max)
