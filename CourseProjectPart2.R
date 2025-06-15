#set working directory to the current one
setwd("/Users/crorick/Documents/MS\ Applied\ Stats\ Fall\ 2023/MA4710/Course\ Project")
set.seed(10)
library(car)
library(olsrr)

#Load in NYC Airbnb data
NYC <- read.csv('AB_NYC_2019.csv')
NYC <- subset(NYC, select = -c(latitude,longitude, last_review, host_id, host_name, name, id, neighbourhood, neighbourhood_group, room_type))
NYC <- na.omit(NYC)

#Make reviews per month reviews per year instead
NYC$reviews_per_year <- NYC$reviews_per_month * 12

#reducing the data points down to a random sample of 1000 for better and
# faster processing
NYC <- NYC[sample(nrow(NYC), 1000), ]

#create linear model for NYC AirBNB data
NYC.lm <- lm(reviews_per_year  ~  minimum_nights + price + calculated_host_listings_count + availability_365 + number_of_reviews, NYC)

#create output vector
Y <- NYC$reviews_per_year

# Create the hat matrix (aka projection matrix) using matrix algebra 
# (do not use lm and related functions).
X <- with(NYC, cbind(1, minimum_nights, price, calculated_host_listings_count, availability_365, number_of_reviews))
P <- X %*% solve(crossprod(X)) %*% t(X)
leverage <- diag(P)

#Create a scatterplot matrix of your response and five predictor variables.
scatterplotMatrix(~reviews_per_year + minimum_nights + price + calculated_host_listings_count + availability_365 + number_of_reviews, NYC, smooth=FALSE)

#Create a normal q-q plot of the standardized residuals, including a reference line, 
# and also perform the Shapiro-Wilk test on the set of standardized residuals.
plot(NYC.lm, which = 2)
shapiro.test(rstandard(NYC.lm))

#Create a plot of the standardized residuals against the fitted values.
plot(rstandard(NYC.lm) ~ NYC.lm$fitted.values, xlab = 'Fitted Values', ylab = 'Standardized Residuals')

#Create a plot of the standardized residuals against each of the predictor variables.
par(mfrow = c(3, 2))
plot(rstandard(NYC.lm) ~ NYC$price, ylab = 'standard residuals')
plot(rstandard(NYC.lm) ~ NYC$number_of_reviews, ylab = 'standard residuals')
plot(rstandard(NYC.lm) ~ NYC$minimum_nights, ylab = 'standard residuals')
plot(rstandard(NYC.lm) ~ NYC$availability_365, ylab = 'standard residuals')
plot(rstandard(NYC.lm) ~ NYC$calculated_host_listings_count, ylab = 'standard residuals')

#Create a location-spread plot.
par(mfrow=c(1,1))
plot(NYC.lm, which = 3)

#Create a residual-leverage plot.
plot(NYC.lm, which = 5)

#Create an index plot of Cook’s distance.
plot(NYC.lm, which = 4)

#Create an index plot of DFFITS.
ols_plot_dffits(NYC.lm)

#Create a panel of index plots of DFBETAS.
ols_plot_dfbetas(NYC.lm)

#Create a panel of residual plus component plots.
crPlots(NYC.lm, terms = ~., id = T)






