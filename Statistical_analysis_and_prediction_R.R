## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################
# Business questions:

## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

# what is the impact on sales per product (Week 4)?

###############################################################################

# Check and set working directory
getwd()
setwd(dir = 'C:/Users/Maia/OneDrive/MG/01-PD/LSE Data Course/Course 3/00-Assignment-03/LSE_DA301_assignment_files')

# 1. Load and explore the data



# Install and import Tidyverse.
install.packages('tidyverse')
library(tidyverse)
library(dplyr)
library(ggplot2)

# Import the data set.
sales <- read.csv('turtle_sales.csv', header=TRUE)


# View data frame in separate window.
View(sales)

# Explore data
dim(sales)
str(sales)
as_tibble(sales)


# Check for missing values
sum(is.na(sales))

# Locate missing values
which(is.na(sales))

# find location of missing values in data frame
print("Position of missing values by column")
sapply(sales, function(x) which(is.na(x)))

# Check for duplicates
sum(duplicated(sales))

#####################
#NOTE:
# Although there are 2 missing values, they are in the year column which 
# will be removed, as year is not part of the analysis.
# Therefore the rows with NA in the year column will be left in.
# There are no duplicates in the data frame.
#######################

# Print the data frame.
head(sales, n=5)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 

sales2 <-select(sales, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
View(sales2)

# View the descriptive statistics.
summary(sales2)

####################
# NOTE:
# Product has been incorrectly detected as an integer, consider changing to
# character (using mutate) to aid charting.

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

# Global sales
qplot(y=Global_Sales, data=sales2)

# NA comparison with EU sales
qplot(NA_Sales , EU_Sales , data=sales2)

# Relationship between region and global sales
qplot(NA_Sales , Global_Sales , data=sales2)
qplot(EU_Sales , Global_Sales , data=sales2)

##################
# NOTE:
# A small number of games sold well over the median (4.3) exceeding sales of 
# £20m, what are these?
# Sales are greater in NA than EU, but there is a positive linear correlation
# between sales in the 2 different regions
# Linear relationship between Global sales and NA sales is stronger than EU sales
# suggesting that NA sales contribute more to Global sales.
# There are at several extreme outliers.
##################

## 2b) Histograms
# Create histograms.
# Use 20 bins TO provides better detail

# NA sales
qplot(NA_Sales, bins=10, data=sales2)
qplot(NA_Sales, bins=20, data=sales2)

# EU sales
qplot(EU_Sales, bins=20, data=sales2)

# Global sales
qplot(Global_Sales, bins=20, data=sales2)

###############
# NOTE:
# Majority of game sales under £10m.
# NA and EU histograms have different scales along X, take care when comparing.
###############

## 2c) Boxplots
# Create boxplots.

# Global sales
qplot(Global_Sales, data=sales2, geom='boxplot')

# Global sales by platform
qplot(Platform, Global_Sales, data=sales2, geom='boxplot')

# Create sales by platform plot without extreme outlier
r_out <- filter(sales2,Global_Sales < 35.00)
qplot(Platform, Global_Sales, data=r_out, geom='boxplot')

###############
# NOTE:
# A lot of variation between different product sales across platforms and
# Genres

###############################################################################
# Which products sold over £20m in Global sales in a year?

# Create new data frame over products that sold over £20m
top_p <- filter(sales2,Global_Sales > 20.00)

#View data frame
top_p

# Change Product variable to a character data type
top_p <- mutate(top_p, Product = as.character(Product))

#Check data frame
summary(top_p)

# Plot results
qplot(x=Product,y=Global_Sales,data=top_p, colour=Platform)

# Sum total global sales and product sales above £20
# Calculate percentage of Global sales contributed by product sales above £20
apply(sales2['Global_Sales'], 2, sum)
apply(top_p['Global_Sales'], 2, sum)
323.2 / 1877.81*100



##################
# NOTE:
# Of 11 games that sold over £20m, 5 are on the Wii platform
# 17% of Global sales come from from 11 products.

###############################################################################

# 3. Observations and insights

## Notes have been added to each section above ##


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

# Assess the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(sales2)


# Check output: Determine the min, max, and mean values.
apply(sales2[,c(-1,-2)], 2, min)
apply(sales2[,c(-1,-2)], 2, mean)
apply(sales2[,c(-1,-2)], 2, max)

# View the descriptive statistics.
summary(sales2)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

sales3 <- sales2 %>% group_by(Product) %>%
  summarise(sum_NA_Sales =sum(NA_Sales),
            sum_EU_Sales =sum(EU_Sales),
            sum_Global_Sales =sum(Global_Sales),
            .groups='drop')

# View the data frame.
View(sales3)

# Explore the data frame.
summary(sales3)
dim(sales3)
str(sales3)

# Check for duplicates in the product column to be verify correct grouping.
sum(duplicated(sales3$Product))

## 2b) Determine which plot is the best to compare game sales.
# Colour has been added for ease of recognition as follows:
# NA = red
# EU = blue
# Global = purple

# Create scatterplots.

# NA sales
ggplot(sales3, aes(x=Product, y=sum_NA_Sales)) + 
  geom_point(color = 'red')

# EU Sales
ggplot(sales3, aes(x=Product, y=sum_EU_Sales)) + 
  geom_point(color = 'blue')

# EU compared to NA sales
ggplot(sales3, aes(x=sum_EU_Sales, y=sum_NA_Sales)) + 
  geom_point()

# Create histograms.

# Global sales
ggplot(sales3, aes(x=sum_Global_Sales)) + 
  geom_histogram(bins = 20, fill= 'purple')

# NA sales
ggplot(sales3, aes(x=sum_NA_Sales)) + 
  geom_histogram(bins = 20, fill= 'red')

# EU sales
ggplot(sales3, aes(x=sum_EU_Sales)) + 
  geom_histogram(bins = 20, fill= 'blue')


# Create boxplots.

# Global sales
ggplot(sales3, aes(x = Product, y = sum_Global_Sales)) +
  # Specify the geom_boxplot function.
  geom_boxplot(color = 'purple')

# NA sales
ggplot(sales3, aes(x = Product, y = sum_NA_Sales)) +
  # Specify the geom_boxplot function.
  geom_boxplot(color = 'red')

# EU sales
ggplot(sales3, aes(x = Product, y = sum_EU_Sales)) +
  # Specify the geom_boxplot function.
  geom_boxplot(color = 'blue')

#############################
# NOTE
# Error messages received suddenly regarding box plots caused becuase product is
# incorrectly specified as a numerical value, leave as is as these are just EDA
# plots.
# NA and EU plots show different axis scales, so need to amend or take care when
# comparing, to avoid misleading visuals/
# Possibly use grid.arrange(plot1,plot2,plot3,ncol=1) or nrow=1 to display charts
# together to highlight box plots and histograms.


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.

# Global sales
qqnorm(sales3$sum_Global_Sales)
# Add a line to the plot.
qqline(sales3$sum_Global_Sales) 

# NA_Sales
qqnorm(sales3$sum_NA_Sales)
# Add a line to the plot.
qqline(sales3$sum_NA_Sales)

# EU_Sales       
qqnorm(sales3$sum_EU_Sales)
# Add a line to the plot.
qqline(sales3$sum_EU_Sales)

## NOTE:
# q plots suggest data is not normally distributed, which is supported by
# earlier histogram plots showing skew.
# verify with Shapiro-Wilk test
#######################

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Global sales
shapiro.test(sales3$sum_Global_Sales)

# North America
shapiro.test(sales3$sum_NA_Sales)

# Europe
shapiro.test(sales3$sum_EU_Sales)

## NOTE:
# Highly significan p values  of much <0.05 (for all 3 sales groups) confirm 
# that data shows significant deviation from a normal distribution.
# Assess the deviation from the normal distribution by checking skewness and
# kurtosis.
#########################

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

# Global sales
skewness(sales3$sum_Global_Sales)
kurtosis(sales3$sum_Global_Sales)

## NOTE:
# Global sales data is highly positively skewed = 3.07
# Kurtosis of 17.8 shows Leptokurtic / heavy tailed distribution,
# this indicates that the variance is caused by a few extreme values that 
# are more extreme than expected.
########################

# NA sales
skewness(sales3$sum_NA_Sales)
kurtosis(sales3$sum_NA_Sales)

## NOTE:
# NA sales data is highly positively skewed = 3.05
# Kurtosis of 15.6 shows Leptokurtic / heavy tailed distribution,
# this indicates that the variance is caused by a few extreme values that 
# are more extreme than expected
#######################

# EU sales
skewness(sales3$sum_EU_Sales)
kurtosis(sales3$sum_EU_Sales)

## NOTE:
# NA sales data is highly positively skewed = 2.89, but less so than NA sales 
# Kurtosis of 16.2 shows Leptokurtic / heavy tailed distribution,
# this indicates that the variance is caused by a few extreme values that 
# are more extreme than expected, which can be clearly seen on the chart. showing
# a number of points over the line above theoretical quantile 1
#################

## 3d) Determine correlation


# Determine correlation.
# As the data is not normally distributed select Spearman's correlation test.
# Spearman's correlation is suitable for monotonic data and
# is robust to outliers, which are present in this data set.

# Correlation between EU and Global sales
cor.test(sales3$sum_Global_Sales, sales3$sum_EU_Sales, method = "spearman", exact=FALSE)


# Correlation between NA and Global sales
cor.test(sales3$sum_Global_Sales, sales3$sum_NA_Sales,method = "spearman", exact=FALSE)


# Correlation between EU and NA sales
cor.test(sales3$sum_NA_Sales, sales3$sum_EU_Sales, method = "spearman", exact=FALSE)


##########################
# NOTE:

# EU sales and Global sales
# p-value < 2.2e-16 shows highly significant very strong 
# positive correlation (0.82),as EU  sales go up global sales go up.

# NA sales and Global sales
# p-value < 2.2e-16 shows highly significant strong positive correlation (0.79),
# as NA  sales go up global sales go up.


# NA sales and EU sale
# p-value = 4.47e-14 shows highly significant positive correlation (0.53), 
# as NA  sales go up EU sales go up


###############################################################################
##############################################################################
# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.



# Compare boxplots without extreme outlier

install.packages("gridExtra", repo="http://cran.r-project.org",dep=TRUE)
library(gridExtra)
install.packages("ggpubr")
library(ggpubr)

plot1 <- ggplot(r_out, aes(x = Product, y = sum_Global_Sales)) +
  # Specify the geom_boxplot function.
  geom_boxplot(color = 'purple') +
  scale_y_continuous(breaks = seq(0, 50, 5), "Global Sales")

# NA sales
plot2 <- ggplot(r_out, aes(x = Product, y = sum_NA_Sales)) +
  # Specify the geom_boxplot function.
  geom_boxplot(color = 'red') +
  scale_y_continuous(breaks = seq(0, 50, 5), "North American Sales")

# EU sales
plot3 <- ggplot(r_out, aes(x = Product, y = sum_EU_Sales)) +
  # Specify the geom_boxplot function.
  geom_boxplot(color = 'blue') +
  scale_y_continuous(breaks = seq(0, 50, 5), "European Sales")

grid.arrange(plot2,plot1,plot3,nrow=1, ncol=3,)

# Create bar chart for top selling products
ggplot(top_p, aes(Product, Global_Sales)) +
  geom_col(fill = 'purple') +
  scale_y_continuous(breaks = seq(0, 65, 5), "Global Sales") +
  geom_hline(yintercept = 4.3, linewidth = 2)


###############################################################################

# 5. Observations and insights
## NOTES are included in the sections above ##



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

# What the relationship(s) is/are (if any) between North American, European, and global sales? 

# recommendations to Turtle Games based on:
##   confidence in the models based on goodness of fit and accuracy of predictions?
##   suggestions and recommendations be to the business?
##   how could you improve the model(s)?

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.

View(sales3)

# Determine a summary of the data frame.
summary(sales3)

# Create subset of data frame without product column as this is categorical
# variable that is incorrectly typed as an integer and could interfere with
# correlation tests.
sales4 <- select(sales3,-Product)
head(sales4)
###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
cor(sales4)

# Basic visualisation.
plot(sales4$sum_NA_Sales, sales4$sum_Global_Sales)

# Create a linear regression model on the original data.
# Note model uses original data frame for consistency of comparison with mlr
# model tested below.
# Model 1 uses NA sales to predict global sales
lmmodel1 <- lm(Global_Sales~NA_Sales,
             data=sales)


# View model
lmmodel1
summary(lmmodel1)

## NOTE
# Based on result expect Global sales to increase on average by £1.72 million  
# each £1m of NA_sales
# small p value indicates that NA_sales is a highly significant variable for global sales
#R-squared value indicates NA_sales explains 84% of the value of the global sales.
#######################

# View residuals on a plot to check for homoscedastity
plot(lmmodel1$residuals)


## NOTE:
# Plot shows cone like arrangement which indicates heteroskedastity.
# Possibly an indication of impure heteroscedasticity, meaning that the model is
# incorrectly specified,due to leaving an important variable out of the model.
#####################

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

plot(sales$NA_Sales, sales$Global_Sales)
coefficients(lmmodel1)
# Add line-of-best-fit.
abline(coefficients(lmmodel1))
# Plot indicates good fit

################################################################
# Create a linear regression model on the _original data_   

# Model 2 uses EU sales to predict global sales
lmmodel2 <- lm(Global_Sales~EU_Sales,
               data=sales)

# View model
lmmodel2
summary(lmmodel2)

## NOTE:
# Based on result expect Global sales to increase on average AS EU sales increase.
# small p value indicates that EU_sales is a highly significant variable for global sales
#R-squared value  indicates that EU_sales explains 77% of the value of the global sales.

# View residuals on a plot to check for homoscedastity
plot(lmmodel2$residuals)

## NOTE:
# Plot shows cone like arrangement which indicates heteroskedastity.
# Possibly an indication of impure heteroscedasticity, meaning that the model is
# incorrectly specified,due to leaving an important variable out of the model.
######################

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(sales$EU_Sales, sales$Global_Sales)
coefficients(lmmodel2)
# Add line-of-best-fit.
abline(coefficients(lmmodel2))

## NOTE:
# Plot indicates good fit

##############################

# Model 3 uses EU sales to predict NA sales
lmmodel3 <- lm(NA_Sales~EU_Sales,
               data=sales)

# View model
lmmodel3
summary(lmmodel3)

# Based on result expect NA sales to increase on average as EU sales increase.
# small p value indicates that EU_sales is a highly significant variable to 
# indicate NA sales.
# R-squared value indicates that EU_sales explains 50% of the value of the 
# NA sales.

# View residuals on a plot to check for homoscedastity
plot(lmmodel3$residuals)

## NOTE:
# Plot shows cone like arrangement which indicates heteroskedastity.
# Possibly an indication of impure heteroscedasticity, meaning that the model is
# incorrectly specified,due to leaving an important variable out of the model.
###################

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(sales$EU_Sales, sales$NA_Sales)
coefficients(lmmodel3)
# Add line-of-best-fit.
abline(coefficients(lmmodel3))


###############################################################################

# 3. Create a multiple linear regression model
# Establish if other variables also play an important role on global sales

# Select only numeric columns from the original data frame.
sales_num <- select_if(sales, is.numeric)
head(sales_num)
View(sales_num)

# Check correlation between variables to identify best variables for model
# Determine correlation between variables.
cor(sales_num)

# Import the psych package.
library(psych)

# Use the corPlot() function to create easier output to identify variables to test
# Specify the data frame and set character size (cex=2).
corPlot(sales_num, cex=2)

## NOTE:
# correlation plot and numerical table indicate that NA and EU sales have the 
# most significant effects on Global sales and should be included in the MLR model
# There is:
# a strong positive correlation (0.93) between NA sales and Global sales 
# a strong negative correlation (0.88) between EU sales and Global sales
# Product and ranking are not good indicators. Although product appears to show 
# some negative correlation it is not a true numerical value and so cannot be used
# in the model.
############################

# Multiple linear regression model.
# specify the lm function and the variables.
mlmodel4 = lm(Global_Sales~NA_Sales+EU_Sales, data=sales_num)

# Print the summary statistics.
summary(mlmodel4)


## NOTE:
# The star ratings and p values for NA and EU sales indicates that these are very
# significant variables that explain global sales
# The R squared value indicates that the 2 variables explain 97% of the variation in 
# Global sales price.
# The adjust r-squared value also shows 97% indicating that the r-squared value has
# not been overly inflated by the addition of the second variable.
# Model 4 is the best fit to predict global sales compared to models 1 and 2.

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# Create test data and add to a data frame

# EU sales figures.
EU_Sales <- c(23.80, 1.56, 0.65, 0.97, 0.52)

# NA sales figures.
NA_Sales <- c(34.02, 3.93, 2.73,2.26, 22.08)

# Create the data frame.
sales_test <- data.frame(EU_Sales, NA_Sales)

# View the data frame.
sales_test

# Test model
# Create a new object and specify the predict function.
predictTest = predict(mlmodel4, newdata=sales_test,
                      interval='confidence')

# Print the object.
predictTest

# Compare expected and predicted values
# Create new columns for predicted and expected results.
# Expected results are based on data frame sales_num.
# Predicted results are based on output of predictTest.
Expected <- c(67.85, 6.04, 4.32, 3.53, 23.21)
Predicted <- c(71.47, 6.86, 4.24, 4.13, 26.43)

# Add The new columns to the data frame.
sales_test$Expected <- Expected
sales_test$Predicted <- Predicted

# View new data frame ro compare model predictions against expected results
sales_test

##NOTE:
# The predictions are higher than the expected results, but the model does a 
# reasonable job of predicting global sales based on NA and EU sales.

###############################################################################

# 5. Observations and insights
## NOTES included in the sections above ##



###############################################################################
###############################################################################




