list = ls() # clears global environment 
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R Session

#Loading some libraries
library(tidyverse)
library(dplyr)
library(pacman)
library(MASS)
library(gtsummary)
library(corrplot)
library(ggplot2)
library(scatterplot3d)

#Reading the dataset
df <- read.csv("dataset.csv")
df

#Finding EDA

colnames(df)
dim(df)

head(df, n=5)
tail(df, n=5)

is.na(df)
table(is.na(df))
str(df)

summary(df)



# R Code
library(ggplot2)
library(ggthemes)
library(psych)
describeBy(df$danceability, df$popularity)


#Is there a relationship between energy and acousticness in tracks?
#Linear Regression Model for energy and acousticness
model1 <- lm(energy ~ acousticness, data = df)

#Summary of linear regression model
summary(model1)

#Visualization of relationship between energy and acousticness
plot1 <- ggplot(df, aes(x = acousticness, y = energy, color = acousticness)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add the regression line
  scale_color_gradient(low = "red", high = "blue", name = "Acousticness") +
  labs(title = "Scatter Plot of Energy vs. Acousticness",
       x = "Acousticness", y = "Energy") +
  theme_minimal()
plot1

#Correlation Matrix
correlation_matrix1 <- cor(df[c("energy", "acousticness")])
correlation_matrix1

#Creating a heatmap to visualize the correlation matrix
corr1 <- corrplot(correlation_matrix1, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#Hypothesis Testing
t_test_result1 <- t.test(df$energy,df$acousticness,alternative = "two.sided")
t_test_result1

#Are danceability and energy jointly significant in predicting popularity?
#Linear Regression Model for popularity, danceability and energy
model2 <- lm(popularity ~ danceability + energy, data=df)

#Summary of Linear Regression Model
summary(model2)

#Visualization for prediction of popularity with the help of danceability and energy
plot2 <- scatterplot3d(df$danceability, df$energy, df$popularity,
              color = ifelse(df$popularity < 60, "red", 
                             ifelse(df$popularity < 80, "blue", "yellow")),
              main = "Scatterplot of Danceability, Energy, and Popularity",
              xlab = "Danceability", ylab = "Energy", zlab = "Popularity")
plot2

# Adding a color legend
legend("topright", legend = c("Low Popularity", "Medium Popularity", "High Popularity"), 
       fill = c("red", "blue", "yellow"), pch = 19)

#Correlation Matrix
correlation_matrix2 <- cor(df[c("popularity", "danceability","energy")])
correlation_matrix2

#Creating a heatmap to visualize the correlation matrix
corr2 <- corrplot(correlation_matrix2, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


#Hypothesis testing
# Hypothesis Testing for Overall Model Significance
t_test_result2 <- t.test(df$popularity,df$danceability,alternative = "two.sided")
t_test_result2

t_test_result3 <-t.test(df$popularity,df$energy,alternative = "two.sided")
t_test_result3

#Is there any impact of valence and tempo on the duration of the song?
# Linear Regression Model for impact of valence and tempo on duration
model_duration <- lm(duration_ms ~ valence + tempo, data = df)

#Summary of Linear Regression Model
summary(model_duration)

#Visualization of impact of valence and tempo on the duration of song tracks
plot3 <- ggplot(df, aes(x = valence, y = duration_ms, color = tempo)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot of Valence and Tempo vs. Song Duration",
       x = "Valence", y = "Duration (ms)", color = "Tempo") +
  theme_minimal()
plot3

#Correlation Matrix
correlation_matrix3 <- cor(df[c("duration_ms", "valence","tempo")])
correlation_matrix3

#Creating a heatmap to visualize the correlation matrix
corr3 <- corrplot(correlation_matrix3, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


#Hypothesis Testing
t_test_result4 <- t.test(df$duration_ms,df$valence,alternative = "two.sided")
t_test_result4

t_test_result5 <-t.test(df$duration_ms,df$tempo,alternative = "two.sided")
t_test_result5





