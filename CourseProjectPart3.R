setwd("/Users/crorick/Documents/MS\ Applied\ Stats\ Fall\ 2023/MA4710/Course\ Project")
set.seed(10)
library(multcomp)

#Load in NYC Airbnb data
NYC <- read.csv('AB_NYC_2019.csv')
NYC <- subset(NYC, select = -c(latitude,longitude, price, calculated_host_listings_count, last_review, host_id, neighbourhood, availability_365, host_name, name, id))
NYC <- na.omit(NYC)

#Make reviews per month reviews per year instead
NYC$reviews_per_year <- NYC$reviews_per_month * 12

#Reducing the data points down to a random sample of 1000 for better and
# faster processing
NYC <- NYC[sample(nrow(NYC), 1000), ]

#There is not enough data on staten Island so removing it from the data
NYC <- NYC[!(NYC$neighbourhood_group == 'Staten Island'),]

#make sure factor variables are in data as factors
NYC$neighbourhood_group <- as.factor(NYC$neighbourhood_group)
NYC$room_type <- as.factor(NYC$room_type)

# 1 Fit the multiple regression model, using two of your categorical predictors and 2-3 numerical predicitors, and include all pairwise interaction terms
NYC.lm <- lm(reviews_per_year  ~  (minimum_nights + number_of_reviews + neighbourhood_group + room_type)^2, NYC)

# 2 Assess whether or not any of the interaction terms should be included in the model by investigating the p-values associated with each interaction term
# - remove interaction terms one at a time, starting with the least significant p-value, and stopping when all p-values are less than 0.1
reduced.NYC.lm1 <- update(NYC.lm, ~ . - neighbourhood_group:number_of_reviews)
reduced.NYC.lm2 <- update(reduced.NYC.lm1, ~ . - neighbourhood_group:minimum_nights)
reduced.NYC.lm3 <- update(reduced.NYC.lm2, ~ . - number_of_reviews:room_type)
reduced.NYC.lm4 <- update(reduced.NYC.lm3, ~ . - number_of_reviews:minimum_nights)
summary(reduced.NYC.lm4)

# 3 Create a set of diagnostic plots and interpret them
# - how does the linearity assumption appear to be met?
plot(reduced.NYC.lm4, which = 1)
# **The linearity assumption does not appear to be a perfect assumption as the residuals 
# ** are not evenly scattered around the fitted line. 
# - How does the normality assumption appear to be met?
plot(reduced.NYC.lm4, which = 2)
# ** The data skews towards extremes at the higher and lower ends of the theoretical quantiles.  
# - How does the homoscedasticity assumption appear to be met?
plot(reduced.NYC.lm4, which = 3)
# ** Homoscedasticity does not appear to be met as the scale-location plot increases with increasing fitted values
# - Are there any influential observations in your model? how do you know they’re influential?
plot(reduced.NYC.lm4, which = 5)
cat(which(cooks.distance(reduced.NYC.lm4) > 0.5))
# ** I consider Point 864 as an influential point because the cook's distance is greater than 0.5

# 4 Interpret the presence of any interaction terms in the model
summary(reduced.NYC.lm4)
# - If there is an interaction between a numerical predictor and a categorical predictor, provide a verbal interpretation of the associated regression coefficients 
# - (i.e. calculate the intercept and slope, when controlling for the other predictors, associated with observations in the different
# ** There is an interaction between the room type and the minimum nights of stay. For a stay in a private room you if you increase the 
# ** minimum nights required for a stay, there is a positive impact on the review frequency. This association is less clear for shared rooms.
# - If there is an interaction between two categorical predictors, provide verbal interpretations of the associated regression coefficients
# ** There is an interaction between neighborhood group and room type. The burrough of the Airbnb combined with
# ** whether the room is shared or private has an impact on the frequency of reviews. A shared room in Manhattan for example
# ** will raise the review frequency more than a private room in Manhattan. And a private room in Brooklyn will get raise 
# ** the review frequency more than a private room in Manhattan. 

# 5 Are there any categories in one of your categorical variables which could be combined together?
# - perform a set of simultaneous hypothesis tests associated with the different 
# - levels of one of your categorical variables (the multcomp or car package will be very useful in this regard)
diff.glht <- glht(reduced.NYC.lm4, 'neighbourhood_groupManhattan - neighbourhood_groupBrooklyn = 0')
summary(diff.glht)

# - if you get insignificant p-values associated with combining two (or more) categories together, 
# - refit your model and perform diagnostics to see what assumptions are still met (or not met)
new_neighbourhood_group <- NYC$neighbourhood_group
levels(new_neighbourhood_group)[2:3] <- "Brooklyn.Manhattan"
reduced.NYC.lm5 <- update(reduced.NYC.lm4, ~ . -neighbourhood_group - neighbourhood_group:room_type + new_neighbourhood_group + new_neighbourhood_group:room_type)
anova(reduced.NYC.lm4, reduced.NYC.lm5)
cat(summary(reduced.NYC.lm4)$adj.r.squared)
cat(summary(reduced.NYC.lm5)$adj.r.squared)
# ** Has a positive impact on the correlation coefficient, as the adjusted R squared value increases. 
# ** There is not a significant difference between the model that combines Brooklyn and Manhattan and the model that does not. 




