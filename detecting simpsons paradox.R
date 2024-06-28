library(readr)
library(mice)
library(reshape2)
library(ggplot2)
library(reshape)
library(dplyr)

# reading in the data
sp <- read_csv("IC_SolarPower_38m.csv")

# replace spaces in column names with dots
colnames(sp) <- gsub(" ", ".", colnames(sp))
head(sp)

# summary(sp)

# Specifying the number of quartiles, "n", for the grouping of the categorical 
# variables
n = 10

# Summary shows missing value in pressure column, using regression imputation to 
# impute the missing value
sp <- mice(sp, seed = 1, method = "norm.predict", m = 1, maxit = 5)
sp <- complete(sp, action=1)
#summary(sp)

# Storing the target variable in a dataframe called "target"
target <- sp[11]

# Storing the categorical variables in a dataframe called "cats" and computing 
# the correlation of each of the catagorical variables with the target variable
# and storing the results in a dataframe called "cor_cats"
cats <- sp[2:9]
cor_cats <- cor(cats, target)

# Computing the "n"% quartiles for all categorical variables and storing the 
# results in a dataframe called "quarts"
quarts <- cats
for (i in 1:ncol(cats)) { quarts[i] <- ntile(cats[i],10) }

# Computing the correlation of the categorical variables with the target 
# variable for each quartile of each categorical variable and storing the 
# results in a 3-dimensional array (of size ncol(cats) x ncol(cats) x n) 
# called "cor_cons". Checking for a correlation reversal and storing the details
# for each correlation reversal in a dataframe called "reversals"
cor_cons <- array(dim=c(ncol(cats),ncol(cats),n))
reversals <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(reversals) <- c("Correlation","Variable","Grouped by","Percentile",
                         "Reverse Correlation")
m <- 0
for (i in 1:ncol(cats)){
  for (j in 1:ncol(cats)){
    for (k in 1:n){
      cor_cons[i,j,k] <- cor(cats[i][quarts[j]==k],target[quarts[j]==k])
      if ((is.na(cor_cons[i,j,k])==FALSE) & ((cor_cats[i] < -0.1 & 
        cor_cons[i,j,k] > 0.1) | (cor_cats[i] > 0.1 & cor_cons[i,j,k] < -0.1))){
        reversals[m,] <- list(cor_cats[i], colnames(cats)[i], 
                                           colnames(cats)[j],k,cor_cons[i,j,k])

        m <- m+1
      }
    }  
  }
}

# Outputting a list of reversals
reversals