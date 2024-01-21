# DATA PROCESSING: Data manipulation and analysis


### Downloading "diamonds" content from "ggplot2" package
install.packages('ggplot2')
library(ggplot2)
data("diamonds")
head(diamonds)

# Output:
# A tibble: 6 x 10
#   carat cut       color clarity depth table price     x     y     z
#   <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
# 1  0.23 Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
# 2  0.21 Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
# 3  0.23 Good      E     VS1      56.9    65   327  4.05  4.07  2.31
# 4  0.29 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63
# 5  0.31 Good      J     SI2      63.3    58   335  4.34  4.35  2.75
# 6  0.24 Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48


### Information on the "diamonds" database
str(diamonds)

# Output:
# tibble [53,940 x 10] (S3: tbl_df/tbl/data.frame)
#  $ carat  : num [1:53940] 0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 ...
#  $ cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 4 2 4 2 3 3 3 1 3 ...
#  $ color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...
#  $ clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 3 5 4 2 6 7 3 4 5 ...
#  $ depth  : num [1:53940] 61.5 59.8 56.9 62.4 63.3 62.8 62.3 61.9 65.1 59.4 ...
#  $ table  : num [1:53940] 55 61 65 58 58 57 57 55 61 61 ...
#  $ price  : int [1:53940] 326 326 327 334 335 336 336 337 337 338 ...
#  $ x      : num [1:53940] 3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...
#  $ y      : num [1:53940] 3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...
#  $ z      : num [1:53940] 2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...

# Number of observations and number of variables
dim(diamonds)   # 53940 observations and 10 variables

# Output:
# 53940    10


# The names of the different variables
names(diamonds)

# Output:
# "carat"   "cut"     "color"   "clarity" "depth"   "table"   "price"   "x"       "y"       "z"


### Calculation of the average and the quartiles of the variable 'price'

# The mean and the three quartiles
summary(diamonds$price)

# Output:
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#     326     950    2401    3933    5324   18823


### Box plot of price by cut
boxplot(
  diamonds$price~diamonds$cut,
  col  = c('slategray1','skyblue1','steelblue1','steelblue3','steelblue4'),
  main = 'Price by cut',
  xlab = 'CUT',
  ylab = 'PRICE'
)


### Distribution of the 'price' and 'carat' variables

# Visual representation of the distribution of the two variables
plot(
  diamonds$price ~ diamonds$carat,
  main = "Distribution of the price and carat variables",
  xlab = 'CARAT', ylab = 'PRICE', col = c('blue')
)

cat("
  Observation:
  We can see that the more the number of carat increases, the more the price increases.
  The price and the number of carats is strongly correlated.
  We also note that the majority of the points are between a little more than 0 and a little less than 3 carats.\n\n"
)


### Asymmetry coefficients and Flattening coefficients of the 'price' and 'carat' variables

# Various statistics functions
install.packages('e1071')
library( e1071 )

# Skewness coefficient
Price.asymetrie <- skewness(diamonds$price) # 1.618305
Price.aplattissement <- kurtosis(diamonds$carat) # 1.25625

# Kurtosis coefficients
Carat.asymetrie <- kurtosis(diamonds$price) # 2.177191
Carat.aplattissement <- skewness(diamonds$carat) # 1.116584

# Summary table
tableauRecap <- data.frame(
  matrix(
    c(Price.asymetrie,Price.aplattissement,
    Carat.asymetrie, Carat.aplattissement),
    nrow = 2,
    ncol = 2,
    byrow = TRUE
  )
)

dimnames(tableauRecap) <- list(
  c('Price','Carat'),
  c('Skewness','Kurtosis')
)

tableauRecap

# Output:
#       Skewness Kurtosis
# Price 1.618305 1.256250
# Carat 2.177191 1.116584


### Comparison of results

cat("
  Comparison of results:\n
  The skewness (asymmetry) coefficient is greater than zero for both variables, so the distribution of the variable 'price' and that of 'carat' are both shifted to the left and their mean is therefore greater than the median.
  The kurtosis (flattening) coefficient is greater than zero for both variables so # the distribution is more concentrated than normal.\n\n"
)


### Checking the price difference between two diamonds of different colors: color 'D' and 'J'

cat("
  Formulation of the hypothesis: \n
  H0: µD = µJ \n
  H1 : µD ≠ µJ \n
  seuil : 5% \n\n"
)


##  Data set

# Diamonds based on price and color
diamantsColor <- data.frame(diamonds$price, diamonds$color)
names(diamantsColor) <- c("Price", "Color")

# Price based on 'D' and 'J' colors
diamondsD <- diamantsColor$Price[diamantsColor$Color == 'D']
diamondsJ <- diamantsColor$Price[diamantsColor$Color == 'J']

# Hypothesis test
t.test( diamondsD, diamondsJ, mean = 0 )

# Output:
#
# 	Welch Two Sample t-test
#
# data:  diamondsD and diamondsJ
# t = -23.121, df = 4197.9, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -2336.496 -1971.232
# sample estimates:
# mean of x mean of y


cat("
  Results interpretation: \n
  The null hypothesis is rejected because the two means are different from 0.
  This means that there is a significant difference between the two means.
  There is therefore a difference between diamonds of color D and those of color J.
  We can see that the average of diamondsD is 3169.954 and that of diamondsJ is 5323.818, which confirms that the averages are different.\n\n"
)


### Fair type diamond groupings


## Data Preparation

# Diamonds by price, carat and cut
diamantsCut <- data.frame( diamonds$price, diamonds$carat, diamonds$cut )
names(diamantsCut) <- c("Price", "Carat", "Cut")

# Price and Carat for 'Fair' cuts
FairPrice <- diamantsCut$Price[diamantsCut$Cut == 'Fair']
FairCarat <- diamantsCut$Carat[diamantsCut$Cut == 'Fair']

# Sample of the top 500
diamondsFair <- data.frame(
  matrix(
    c(FairPrice[1:500],FairCarat[1:500]), ncol=2
  )
)
colnames(diamondsFair) <- c('Price','Carat')

# The first 'diamondsFair' sample data
head(diamondsFair)

# Output:
#   Price Carat
# 1   337  0.22
# 2  2757  0.86
# 3  2759  0.96
# 4  2762  0.70
# 5  2762  0.70
# 6  2763  0.91

str(diamondsFair)

# Output:
# 'data.frame':	500 obs. of  2 variables:
#  $ Price: num  337 2757 2759 2762 2762 ...
#  $ Carat: num  0.22 0.86 0.96 0.7 0.7 0.91 0.91 0.98 0.84 1.01 ...

# Clustering with the K-mean algorithm
set.seed(20)
fairCluster <- kmeans(diamondsFair, 2, nstart = 20)

# Number of samples per group
fairCluster$size

# Output:
# 237 263

cat("
  Group 1: 263 samples
  Group 2: 237 samples\n\n"
)


## Calculation of the silhouette: 0.63

# Calculation of the silhouette
library(cluster)
s <- silhouette( fairCluster$cluster, dist(diamondsFair) )

# Silhouette Results
summary(s)

# Output:
# Silhouette of 500 units in 2 clusters from silhouette.default(x = fairCluster$cluster, dist = dist(diamondsFair)) :
#  Cluster sizes and average silhouette widths:
#       237       263
# 0.6339659 0.6173530
#
# Individual silhouette widths:
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.01911 0.58612 0.70635 0.62523 0.74424 0.76097

# Graphic representation of the silhouette
plot(s)

cat("
  Observations on the two groups:
  The silhouette index is positive for the 2 groups, which shows the homogeneity within the groups.
  Also the 2 groups have the same coefficient, which shows a good separation of the data.
  We can see on the abscissa axis the silhouette index and on the ordinate axis the grouping of the data of the two groups.\n\n"
)


### Determining the correlation between the 'price' and 'carat' variables for VS1-like clarity


## Data Preparation

# Table with price, carat and clarity
tableauVS1 <- data.frame(diamonds$price,diamonds$carat,diamonds$clarity)
names(tableauVS1) <- c("Price","Carat","Clarity")

# Price and Carat for 'Fair' cuts
VS1Price <- tableauVS1$Price[tableauVS1$Clarity == 'VS1']
VS1Carat <- tableauVS1$Carat[tableauVS1$Clarity == 'VS1']

# The 'diamondsVS1' dataset
diamondsVS1 <- data.frame( matrix( c( VS1Carat, VS1Price ), ncol = 2 ) )
colnames(diamondsVS1) <- c('Carat','Price')

# The first data from 'diamondsVS1'
head(diamondsVS1)

# Output:
#   Carat Price
# 1  0.23   327
# 2  0.23   338
# 3  0.23   340
# 4  0.23   353
# 5  0.24   355
# 6  0.23   357

cat("
  Variables to correlate:
  Y: Dependent variable: Price
  X: Independent variable: Cara"
)

# Regression relationship between the two variables
require(stats)
regression <- lm(diamondsVS1$Price ~ diamondsVS1$Carat)
coeff <- coefficients(regression)

# Regression Information
regression

# Output:
#
# Call:
# lm(formula = diamondsVS1$Price ~ diamondsVS1$Carat)
#
# Coefficients:
#       (Intercept)  diamondsVS1$Carat
#             -2705               9001

# Regression Line Equation
equation <- paste0(
  'y = ',
  round(coeff[2],0),
  ' * x ',
  round(coeff[1],0)
)

# Equation
equation

# Output:
# "y = 9001 * x -2705"


## Interpretation of the regression result:

# Detail of the regression
summary(regression)

# Output:
#
# Call:
# lm(formula = diamondsVS1$Price ~ diamondsVS1$Carat)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -6000.0  -587.1    11.1   559.2  7843.4
#
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)
# (Intercept)       -2705.50      27.48  -98.47   <2e-16 ***
# diamondsVS1$Carat  9000.73      32.65  275.66   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1250 on 8169 degrees of freedom
# Multiple R-squared:  0.9029,	Adjusted R-squared:  0.9029
# F-statistic: 7.599e+04 on 1 and 8169 DF,  p-value: < 2.2e-16

cat("
  Interpretation of regression results:
  We see the price = α + β carat pattern with α = -2705.50 and β = 9000.73.
  This equation tells us that the price depends linearly on the carats: the price increases by $9000.73 per additional carat.
  Since the p value = 2.2e-16 is less than 0.05, we can conclude that there is a significant relationship between the variables of the linear regression model and since R-squered is very high with 0.9029, it allows us to conclude that the regression model is of very good quality.  "
)


## Graphic representation of the regression line:

# Graphical representation of the regression line
plot(diamondsVS1, main =  equation, col = 'blue')
abline(regression, col = 'red', lwd = 2)
