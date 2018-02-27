setwd("~/Desktop/S-S/Aalto/IN Stats/Excercise/ex_18")
# 1b
# Require e1071 package to calculate skewness and kurtosis
library(e1071)
BS <- function(x) {
  n <- length(x)
  v <- skewness(x)
  k <- kurtosis(x)
  n*(v^2/6+k^2/24)
}

BS(runif(20))
BS(runif(50))
BS(runif(100))
BS(runif(500))
BS(runif(1000))

#BS-statistic starts to 'explore' with relatively high n
#the p-value can be computed as
sample <- runif(50)
bs1 <- BS(sample)
pval <- 2*min(pchisq(bs1,2),1-pchisq(bs1,2))
pval

# let's test with a bimodal distribution
BS(c(rnorm(10),rnorm(10)+5))
BS(c(rnorm(20),rnorm(20)+5))
BS(c(rnorm(50),rnorm(50)+5))
BS(c(rnorm(100),rnorm(100)+5))
BS(c(rnorm(500),rnorm(500)+5))

# 2a
?qqplot
?qqline
?rchisq
?runif
# prepare data sets
norm.sample <- rnorm(50)
chi.sample <- rchisq(50, df=3)
uni.sample <- runif(50, min = 0, max = 1)
outlier.nsample <- rnorm(50)
outlier.nsample[1] = -15 # add an outlier to the sample 

# plot histograms for datasets 
par(mfrow=c(2,2))
hist(norm.sample); hist(chi.sample); hist(uni.sample); hist(outlier.nsample)
par(mfrow=c(1,1))

# Calculate the sample means, medians, and the skewness and kurtosis values of the samples.
summary(norm.sample)
summary(chi.sample)
summary(uni.sample)
summary(outlier.nsample)

# Vectorize our computations; collect all data into data frame.
data <- data.frame(norm.sample,chi.sample,uni.sample,outlier.nsample)
apply(data,2,median)
v <- apply(data,2,skewness)
k <- apply(data,2,kurtosis)

# Require tseries package to do Bowman and shenton test (same as jarque and bera test)
library(tseries)
apply(data, 2, jarque.bera.test)
# jarque.bera.test(norm.sample)
# jarque.bera.test(chi.sample)
# jarque.bera.test(uni.sample)
# jarque.bera.test(outlier.nsample)

# Plot rank plots of each sample, and test normality using the
# Shapiro-Wilk normality test.
apply(data, 2, shapiro.test)
par(mfrow=c(2,2))

# Plot qqplots for every sample data
qqnorm(norm.sample); qqline(norm.sample)
qqnorm(chi.sample); qqline(chi.sample)
qqnorm(uni.sample); qqline(uni.sample)
qqnorm(outlier.nsample); qqline(outlier.nsample)
par(mfrow=c(1,1))



# Bart rolls one dice 120 times and gets the score one 12 times, two 16
# times, three 20 times, four 17 times, five 22 times, and the score six
# 33 times. Use the Chi-square goodness of fit test to test the fairness of Bart’s
# dice.
fre_observe <- c(12,16,20,17,22,33)
barplot(fre_observe)
expected_prop <- c(1,1,1,1,1,1)/6 # or use expected_prop = rep(1/6,6)
chisq.test(x = fre_observe, p = expected_prop, correct = F)
# Use significance level of 3%. The null hypothes is rejected because p-value = 0.022

