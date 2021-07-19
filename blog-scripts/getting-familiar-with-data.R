# Getting familiar with data

## The code below accompanies the blog post "getting familiar with data"
## This R script is how it would look in RStudio 

# load readr
library(readr)

# load data
gps_data <- read_csv("https://raw.githubusercontent.com/aaronzpearson/midsprint-blog/main/golden-cheetah-data/2017_12_30_09_43_27.csv")

# see data summary
summary(gps_data)

# visualize heart rate distribution
hist(gps_data$hr)