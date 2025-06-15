setwd("/Users/crorick/Documents/MS\ Applied\ Stats\ Fall\ 2023/MA4710/Course\ Project")
set.seed(10)
library(multcomp)
library(car)

# 1 Refit the model that you built in the previous submission to use as your "base" model...
# load in NYC Airbnb data
NYC <- read.csv('AB_NYC_2019.csv')
NYC <- subset(NYC, select = -c(latitude,longitude, price, calculated_host_listings_count, last_review, host_id, neighbourhood, host_name, name, id))
NYC <- na.omit(NYC)

#Make reviews per month reviews per year instead
NYC$reviews_per_year <- NYC$reviews_per_month * 12

# Reducing the data points down to a random sample of 1000 for better and
# faster processing
NYC <- NYC[sample(nrow(NYC), 1000), ]

#There is not enough data on staten Island so removing it from the data
NYC <- NYC[!(NYC$neighbourhood_group == 'Staten Island'),]

#make sure categorical variables are in data as factors
NYC$neighbourhood_group <- as.factor(NYC$neighbourhood_group)
NYC$room_type <- as.factor(NYC$room_type)

# Fit the multiple regression model
NYC.lm <- lm(reviews_per_year  ~ minimum_nights + availability_365 + number_of_reviews + neighbourhood_group, NYC)

# 2 Thoroughly assess your base model for each of the following assumptions violations. 
# Make sure your R program is organized to make it clear which assumption is being assessed, 
# and in your written summary you should use brief but clear and accurate language which 
# illustrates the reasoning you use in diagnosing any problem with the assumptions:

# Non-normality of the residuals
plot(NYC.lm, which = 2)
shapiro.test(rstandard(NYC.lm))
# Residuals are clearly non-normally distributed as shown by the qqplot. There is a 
# right skew in the data. The standardized residuals tend to fall higher than predicted
# by the current regression model. 

# Model misspecification
summary(NYC.lm)
# The R squared value is 0.4015, meaning that approximately 40.15% of the variation in review 
# frequency can be explained by the multiple regression model that was built. This ideally
# should be larger for a well fitting regression model.

# Heteroscedasticity in the residuals
plot(NYC.lm, which = 3)
# The Scale-Location plot shows a steady increasing linear trend in the standardized residuals. 
# This means the residuals fan out as the fitted values become larger. 

# "Extremely" influential points (just use Cook's distance as your sole measure of influence, 
# via the influencePlot or an index plot of Cook's distance) to simplify this process, 
# you can just list the observations identified as influential by the output of the 
# influencePlot function from the car package.
influencePlot(NYC.lm)

# 3 Now comes the tricky part. Use the suite of tools that have been 
# investigated to try and address any problems that you diagnosed with your base model. 
# You do not need to try out all of these, but should use your previous diagnoses of 
# assumption violations to guide your choices. These tools include...
max(cooks.distance(NYC.lm))
influentialIndicies <- whichNames(c('39876'), NYC)
NYC.lm1 <- update(NYC.lm, ~ ., subset=-influentialIndicies)
max(cooks.distance(NYC.lm1))

# Removing point 39876 as its cook's distance is incredibly large at 1.61 compared to other data. 
# the largest cooks distance is now only 0.2694369
summary(NYC.lm1)

# The new models Adjusted R squared has increased, indicating an improvement in the model fit

crPlots(NYC.lm1)
# Component-Residual plots don't show any non-linear trends in the predictors. 

# Weighting based on the required minimum nights to stay drastically improved the model.
# The new R squared value is 0.9535, which is a 54.48% increase in explanatory power of the model
# after removing the influential point.
NYC.lm2 <- update(NYC.lm1, ~ ., weights = minimum_nights^2)

# 4 The previous step will likely be an iterative process. 
# Recall the goal is to find a set of tools which adequately address all 
# assumption violations; you might have to settle on a model which is "good enough" 
# at addressing most of your assumption violations. Your R program and written summary 
# should reflect the work that is done in coming to this final model. You do not need to 
# include mention of any dead-end attempts which do not help your model. You should 
# include in your written summary:

# The removal of the very influential point didn't have as big of an impact 
# as I was hoping for, potentially because the effect was diluted by 
# such a large sample size.

# I wanted to avoid using a transformation on the response variable if possible to reduce 
# any potential impacts on the ability to interpret the model The variance still needs to be 
# reduced so the weighted least squares approach makes sense. I decided on 
# weighting based on the predictor that indicates the required minimum nights of stay

# Weighting residuals based on the minimum nights required for a stay had more of an impact 
# on the model fit and heteroscedacity than I was originally anticipating. This was
# a drastic improvement to the model fit. One hypothesis i have for why this is the 
# case is because some places that require long term stays for guests might have 
# more of an ability to make a large impression on a guest than quick stays, 
# resulting in a very high hit rate for reviews.

# 5 After outlining the process you use in settling on a final model, 
# provide one last assessment of this final model in your written summary:
summary(NYC.lm)$sigma
summary(NYC.lm2)$sigma
# Summary comparison, R squared increased from 0.4015 to 0.9535 which is a 54.48% increase in explanatory power.
# Sigma increased from 15.19491 to 66.03, so the variance and standard error has increased between the models. 

plot(NYC.lm2, which = 2)
shapiro.test(rstandard(NYC.lm))
# Normality assumption is still violated as the Shapiro test shows an extremely low p value, 
# this isn't a big concern as there is a large enough sample size to make the model robust against this violation.

plot(NYC.lm2$fitted.values, rstandard(NYC.lm2), xlab = 'fitted values', ylab = 'standardized residuals')
residualPlots(NYC.lm2)


