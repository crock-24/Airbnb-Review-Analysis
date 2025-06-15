setwd("/Users/crorick/Documents/MS\ Applied\ Stats\ Fall\ 2023/MA4710/Course\ Project")
library(car)
library(leaps)
library(olsrr)
set.seed(10)

#Load in NYC Airbnb data
NYC <- read.csv('AB_NYC_2019.csv')
NYC <- subset(NYC, select = -c(latitude,longitude, last_review, host_id, neighbourhood, host_name, name, id))
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

# 1 You will look at a variety of ways to detect the presence of collinearity in 
# your set of numerical predictors. Include "all" predictors from your data set in a full model, 
# with the following caveats:
NYC.lm <- lm(reviews_per_year  ~ calculated_host_listings_count + price + minimum_nights + availability_365 + number_of_reviews + neighbourhood_group + room_type, NYC)

# 2 Assess this full model for problems with collinearity by calculating the VIF's for 
# each variable and the condition number (remember to scale the variables); you can 
# use the crude rules of thumb mentioned in your book and notes.

mat.data.pc <- model.matrix(~ . - reviews_per_year - reviews_per_month + 0, NYC)
NYC.pc <- prcomp(mat.data.pc, scale=T)
NYC.pc.sdev <- NYC.pc$sdev
NYC.con.ind <- NYC.pc.sdev[1] / NYC.pc.sdev
print(max(NYC.con.ind))
vif(NYC.lm)

# 3 In your summary, explain the reasoning you use in coming to a decision if there 
# is a serious problem with collinearity in your set of predictors, referencing the 
# condition number and the values of the VIF's

# The VIF's did not point to any predictors being collinear with any other predictor, 
# as the largest VIF did not get above 5, which is the rule of thumb for being moderately problematic
# in regards to collinearity. Most of the condition indices were fine as well other than the maximum,
# that condition number is incredibly large at 1.443422e+14. This indicates that there is collinearity with 
# one of the components.

# 4 Use a few different variable selection procedures on your full data set.

# *Implement in your R program a forward selection, backward elimination, and stepwise selection, 
# using the AIC as the selection criterion 
# * Discuss any differences in the set of variables that are chosen by these three different procedures 
# * Use the regsubsets function to perform a "best subsets" search
# Create plots of the adjusted R2 and Mallow's Cp against the number of predictors p
# * Use the plot function on your regsubsets output with a scale for the adjusted R2 
# * Choose a model using the above plots, describing the variables that are contained in it and the 
# reasoning you use in your selection. 
NYC.null <- lm(reviews_per_year ~ 1, NYC)
step(NYC.null, scope=formula(NYC.lm), direction='forward')
step(NYC.lm, direction='both')
step(NYC.lm, direction='backward')
# All of the AIC model selection procedures settled on the same 3 variables in their conclusion,
# Number_of_reviews, minimum_nights, and availability_365 were chosen in all three. 

NYC.rs <- regsubsets(reviews_per_year ~ . - reviews_per_month, NYC, nbest = 1)
summary(NYC.rs)
subsets(NYC.rs, statistic='cp', legend = FALSE, ylim = c(1,9), xlim = c(3,9))
abline(a=1, b=1, col='red', lty='dashed', lwd=2)
subsets(NYC.rs, statistic='adjr2', legend = FALSE, ylim = c(0.3165, 0.3192), xlim = c(2,8))
plot(NYC.rs, scale = 'adjr2')
NYC.lm2 <- lm(reviews_per_year  ~ minimum_nights + availability_365 + number_of_reviews + calculated_host_listings_count, NYC)
ols_mallows_cp(NYC.lm2, NYC.lm)
# Since all of the step AIC selection procedures chose Number_of_reviews, minimum_nights, and availability_365,
# I decided to include those in the model. I also decided to also include the calculated_host_listings variable  
# as that brought the Mollow's value slightly closer to p+1, and it was shown as valuable because it was included 
# in a significant proportion of the best models in regsubsets that optimize for adjusted r squared value. 

# 5 Use your accumulated knowledge with your data set to ideally settle on a model.
NYC.lm3 <- lm(reviews_per_year  ~ minimum_nights + availability_365 + number_of_reviews + calculated_host_listings_count, NYC, weights = minimum_nights^2)
summary(NYC.lm3)
# model specification/linearity
plot(NYC.lm3$fitted.values, rstandard(NYC.lm3))
# homoscedasticity
plot(NYC.lm3, which = 3)
ncv.test(NYC.lm3)
# the presence of influential observations (no long lists are needed, just look at the influencePlot or similar and point out any glaring problems)
influencePlot(NYC.lm3)
# collinearity
mat.data.pc <- model.matrix(~ minimum_nights + availability_365 + number_of_reviews + calculated_host_listings_count + 0, NYC)
NYC.pc <- prcomp(mat.data.pc, scale=T)
NYC.pc.sdev <- NYC.pc$sdev
NYC.con.ind <- NYC.pc.sdev[1] / NYC.pc.sdev
print(max(NYC.con.ind))
vif(NYC.lm)



