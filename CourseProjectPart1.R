#set working directory to the current one
#setwd("/Users/crorick/Documents/MS\ Applied\ Stats\ Fall\ 2023/MA4710/Course\ Project")

#Bring in NYC Airbnb data
set.seed(10)
NYC <- read.csv('AB_NYC_2019.csv')
NYC <- subset(NYC, select = -c(latitude,longitude, last_review, host_id, host_name, name, id, neighbourhood, neighbourhood_group, room_type))
NYC <- na.omit(NYC)

#Make reviews per month reviews per year instead
NYC$reviews_per_year <- NYC$reviews_per_month * 12

#reducing the data points down to a random sample of 1000 for better and
# faster processing
NYC <- NYC[sample(nrow(NYC), 1000), ]

#Create design matrix and price variable that we want to predict, 
X <- with(NYC, cbind(1, minimum_nights, price, calculated_host_listings_count, availability_365, number_of_reviews))
colnames(X) <- c('intercept', 'min_night_stay', 'price', 'host_listings', 'availability', 'tot_reviews')
Y <- NYC$reviews_per_year

#Solve for coefficients
beta.hat <- solve(crossprod(X), crossprod(X,Y))

#Get predictions 
Y.pred <- X %*% beta.hat

#Find residuals and sum of squares error
res <- Y - Y.pred
df <- nrow(res) - ncol(X)
SSE <- crossprod(res)
Variance <- SSE / df

#Finding the correlation coefficient
R <- cor(Y, Y.pred)
R.sq <- R^2
R.sq.adjust <- 1 - ((nrow(X) - 1)/(nrow(X) - ncol(X))) * (1-R.sq)

#Calculating SSE and degrees of freedom for reduced model 
reduced.Y.pred <- mean(Y)
reduced.res <- Y- reduced.Y.pred
reduced.df <- length(reduced.res) - 1
reduced.SSE <- crossprod(reduced.res)

#Calculating the F statistic for completely reduced model vs full model
f.stat <- ((reduced.SSE - SSE) / (reduced.df - df)) / (SSE / df)
p.value <- pf(f.stat, (reduced.df - df), df, lower.tail=FALSE)

#calculating the covariance matrix and standard error of the beta coefficients
cov.matrix <- solve(crossprod(X))
scaled.cov.matrix <- cov.matrix * as.vector(Variance)
beta.variances <- diag(scaled.cov.matrix)
beta.se <- sqrt(beta.variances)

#calculating the confidence intervals of the different beta coefficients
t.stat <- beta.hat / beta.se
t.stat.abs.value <- abs(t.stat)
p.values <- 2 * pt(t.stat.abs.value, df, lower.tail = FALSE)

# creating a list of observations to predict and then predicting them 
observations <- matrix(data = c(1, 1, 100, 3, 100, 75, 1, 1, 200, 8, 158, 38, 1, 3, 65, 1, 40, 12, 1, 1, 600, 3, 200, 51, 1, 2, 500, 3, 300, 213), nrow = 5, ncol = 6)
predictions <- observations %*% beta.hat

#Critical t value and prediction confidence intervals
t.crit <- qt(0.975, nrow(X) - ncol(X))
conf.intervals <- matrix(nrow=5,ncol=2)
conf.intervals[1,] <- predictions[1] + c(-1, 1) * t.crit*sqrt(as.vector(Variance))*as.vector(sqrt(1+t(observations[1,])%*%solve(crossprod(X))%*%observations[1,]))
conf.intervals[2,] <- predictions[2] + c(-1, 1) * t.crit*sqrt(as.vector(Variance))*as.vector(sqrt(1+t(observations[2,])%*%solve(crossprod(X))%*%observations[2,]))
conf.intervals[3,] <- predictions[3] + c(-1, 1) * t.crit*sqrt(as.vector(Variance))*as.vector(sqrt(1+t(observations[3,])%*%solve(crossprod(X))%*%observations[3,]))
conf.intervals[4,] <- predictions[4] + c(-1, 1) * t.crit*sqrt(as.vector(Variance))*as.vector(sqrt(1+t(observations[4,])%*%solve(crossprod(X))%*%observations[4,]))
conf.intervals[5,] <- predictions[5] + c(-1, 1) * t.crit*sqrt(as.vector(Variance))*as.vector(sqrt(1+t(observations[5,])%*%solve(crossprod(X))%*%observations[5,]))

#Find if host number of host listings coefficients is equal to zero using the built in lm function
rpm.lm <- lm(reviews_per_year  ~  minimum_nights + price + calculated_host_listings_count + availability_365 + number_of_reviews, NYC)
reduced1.rpm.lm <- update(rpm.lm, ~ . - calculated_host_listings_count)
anova(reduced1.rpm.lm, rpm.lm)

#Find if availability coefficient could equal 0.015 using the built in lm function
reduced2.rpm.lm <- update(rpm.lm, ~ . - availability_365 + offset(I(0.015 * availability_365)))
anova(reduced2.rpm.lm, rpm.lm)

#Find if availability_365 and minimum_nights coefficients are equal using the built in lm function
reduced3.rpm.lm <- update(rpm.lm, ~ . - availability_365 - minimum_nights + I(availability_365 + minimum_nights))
anova(reduced3.rpm.lm, rpm.lm)

