
# How the training/test split was obtained
# x <- read.csv('rutgers-lib-30861_CSV-1.csv')
# set.seed(123)
# ii <- sample(rep(1:4, each=15))
# x.tr <- x[ii != 2, ]
# x.te <- x[ii == 2, ]
# write.csv(x.tr, file='pollution-train.dat', row.names=FALSE, quote=FALSE)
# write.csv(x.te, file='pollution-test.dat', row.names=FALSE, quote=FALSE)

# Read training set
x.tr <- read.table('pollution-train.dat', header=TRUE, sep=',')

# sanity check
head(x.tr)

# fit a linear regression model with all available
# predictors
full <- lm(MORT ~ . , data=x.tr)

# look at the estimated coefficients
summary(full)

# a couple of diagnostic plots
# everything looks fine
plot(full, which=1)
plot(full, which=2)


# now fit a smaller linear model 
# using only 5 predictors
reduced <- lm(MORT ~ POOR + HC + NOX + HOUS + NONW, data=x.tr)

# look at the fit
summary(reduced)

# the linear model with 5 predictors isn't as
# good as the full one, but it is not terrible either
plot(reduced, which=1)
plot(reduced, which=2)

# which model fit the data better?
# (in terms of residual sum of squares)
sum( resid(reduced)^2 )
sum( resid(full)^2 )

# no surprises there

# which model produces better predictions
# on the test set?

x.te <- read.table('pollution-test.dat', header=TRUE, sep=',')

head(x.te)
# obtain predicted values for the test set
# with the full and reduced models
x.te$pr.full <- predict(full, newdata=x.te)  
x.te$pr.reduced <- predict(reduced, newdata=x.te)  

# compute the mean squared prediction error
# (on the test set) obtained with each of the
# models

with(x.te, mean( (MORT - pr.full)^2 ))
with(x.te, mean( (MORT - pr.reduced)^2 ))

# repeat with different partitions
x <- read.csv('rutgers-lib-30861_CSV-1.csv')
# set.seed(123)
ii <- sample(rep(1:4, each=15))
x.tr <- x[ii != 2, ]
x.te <- x[ii == 2, ]
full <- lm(MORT ~ . , data=x.tr)
reduced <- lm(MORT ~ POOR + HC + NOX + HOUS + NONW, data=x.tr)
x.te$pr.full <- predict(full, newdata=x.te)  
x.te$pr.reduced <- predict(reduced, newdata=x.te)  
with(x.te, mean( (MORT - pr.full)^2 ))
with(x.te, mean( (MORT - pr.reduced)^2 ))
