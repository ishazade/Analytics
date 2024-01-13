cat("\014")
rm(list = ls()) # clears global environment try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) 
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) 
options(scipen = 100) 
library(dplyr)
library(ggplot2)
library(geosphere)


ny_bnb <- read.csv("AB_NYC_2019.csv")
ny_bnb

#Exploring
#Data Cleaning
#Renaming the columns

ny_bnb <- ny_bnb |>
  rename(
    air_bnb_name = name,
    neighbourhood_df = neighbourhood_group,
    neighbourhood_name = neighbourhood,
    reviews_num = number_of_reviews,
    host_list_count = calculated_host_listings_count,
    yearly_availability = availability_365
  )
ny_bnb

#Managing NAs

ny_bnb <- na.omit(ny_bnb)
ny_bnb

#Correcting datatype
ny_bnb$yearly_availability <- as.numeric(ny_bnb$yearly_availability)


#Manipulating Strings
ny_bnb$host_name <- toupper(ny_bnb$host_name)
ny_bnb


# Descriptive Statistics
interesting_var <-ny_bnb[c("price","reviews_num")]
summary(interesting_var)
sd_price <- sd(interesting_var$price)
sd_price
sd_review <- sd(interesting_var$reviews_num)
sd_review

mean_price <- mean(interesting_var$price)
mean_price

#Variables affecting the price of Airbnb.

plot(
  x = ny_bnb$price,
  y = ny_bnb$reviews_num,
  xlab = "Price",
  ylab = "Number of Reviews",
  main = "Scatter Plot: Price vs. Number of Reviews",
  pch = 16,  
  col = "blue"  
)

#Top 10 most to least expensive Airbnbs.
airbnb_data <- ny_bnb[c("neighbourhood_df", "neighbourhood_name", "price")]
avg_prices <- tapply(airbnb_data$price, airbnb_data$neighbourhood_name, mean, na.rm = TRUE)
avg_prices <- sort(avg_prices, decreasing = TRUE) 
top_10_neighborhoods <- head(avg_prices, 10)
barplot(top_10_neighborhoods, col = "skyblue", main = "Top 10 Neighborhoods by Average Price",
        xlab = "Neighborhood", ylab = "Average Price", las = 2)


#Seasonal Patterns followed by New York Airbnbs.
seasonal_data <- ny_bnb |>
  group_by(yearly_availability) |>
  summarize(avg_price = mean(price))
seasonal_data <- seasonal_data[order(seasonal_data$yearly_availability), ]
ggplot(data = seasonal_data, aes(x = yearly_availability, y = avg_price)) +
  geom_line(color = "purple") +
  labs(
    title = "Airbnb Pricing Seasonal Patterns in New York City",
    x = "Yearly Availability",
    y = "Average Price"
  )
#Expanding
#Creating price cateogry to categorize Airbnbs according to their price range.
ny_bnb$price_category <- cut(ny_bnb$price, breaks = c(0, 50, 100, Inf), labels = c("Budget", "Mid-Range", "Expensive"))
ny_bnb

#Finding distance to time square from neighbouring Airbnbs.
latitude_airbnb <- ny_bnb$latitude
longitude_airbnb <- ny_bnb$longitude
latitude_times_square <- 40.758896  
longitude_times_square <- -73.985130

distances_to_ts <- distHaversine(matrix(c(longitude_airbnb, latitude_airbnb), ncol = 2),
                                           matrix(c(longitude_times_square, latitude_times_square), ncol = 2))
ny_bnb$distance_to_ts <- distances_to_ts
ny_bnb

#Arranging the distance of Airbnbs to time square from least to most distance.
arranged_airbnb <-ny_bnb |>
  arrange(distance_to_ts)
arranged_airbnb

#Counting the number of Airbnbs in Budget and Mid-Range and Expensive category.
price_category_count <- ny_bnb |>
  count(price_category)
price_category_count

# Visualizing Distance to Times Square from Neighbouring Airbnbs.
ggplot(ny_bnb, aes(x = distance_to_ts)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "orange") +
  labs(title = "Distribution of Distances to Times Square", x = "Distance to Times Square") +
  theme_minimal()

#Visualizing the number of Airbnbs according to their price range.
price_category_sum <- table(ny_bnb$price_category)
price_category_df <- as.data.frame(price_category_sum)
names(price_category_df) <- c("Price_Category", "Count")
ggplot(price_category_df, aes(x = Price_Category, y = Count, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Distribution of Price Categories", x = "Price Category", y = "Count") +
  theme_economist()


