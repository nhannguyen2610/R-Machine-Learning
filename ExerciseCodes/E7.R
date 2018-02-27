setwd("~/Desktop/S-S/Aalto/IN Stats/Excercise/ex_18")
dt = read.delim('https://mycourses.aalto.fi/pluginfile.php/627422/mod_resource/content/1/H07HEIGHTSHOE.txt',
                header = T)
# a sample from the class
heights <- c(179,176,185,188,168,171,170,178,177,173,157,165)
shoesize <- c(39,46,42,47,41,41,42,45,42,41,36,36)

# Or generate by R
# heights = sample(150:180, size = 30, replace = TRUE)
# shoesize = sample(32:49, size = 30, replace = TRUE)

# Plot a scatter plot of the data and based on the plot, estimate the
# sign and the magnitude of the correlation between the variables.
m <- lm(heights ~ shoesize)
plot(shoesize, heights)
abline(m, col='red')
summary(m)

# Calculate the Pearson correlation coefficient
cor(x=shoesize, y=heights, method = 'pearson')
cor.test(x=shoesize, y=heights, method = 'pearson')

# Calculate the Spearman rank correlation coefficient
cor(x=shoesize, y=heights, method = 'spearman')
cor.test(x=shoesize, y=heights, method = 'spearman')
# with ties means that data has identical values.

# Use the permutation test to test the statistical significance of the
# correlation coefficients.
# Method 1: lib-coin
library(coin) #load coin package to run significant test of
              #correlation coefficient for non-parametric distribution
spearman_test(heights ~ shoesize, distribution=approximate(B=9999))

#Method 2: lib-jmOutlier
library(jmuOutlier)
# use pearson test for strict assumptions on normality, linearity and iid
perm.cor.test(x=shoesize, y=heights, method = 'pearson', num.sim=9999)
# use spearman test requires less strict assumptions on distribution
perm.cor.test(x=shoesize, y=heights, method = 'spearman', num.sim=9999) 

n <- 10000
x <- rep(NA,n)
for (i in 1:n) {
  s <- sample(shoesize, replace = F)
  x[i] <- cor(heights, s, method = 'pearson')
}
 
hist(x)
corr.p <- cor(x=shoesize, y=heights, method = 'pearson')
corr.p
x.sig <- x[x>= corr.p] #one-sided
length(x.sig)/n #or /length(x)
#two-sided
x.sig <- x[abs(x) >= abs(corr.p)]
length(x.sig)/n

install.packages('combinat')
library(combinat)
system.time
system.time(permCor <- sapply(permn(heights[1:9]),
                              y=shoesize[1:9],
                              method='pearson',
                              cor))
mean(abs(permCor) >= abs(corr.p)) #0.022
cor.test(heights[1:9], shoesize[1:9], method = 'pearson')            
