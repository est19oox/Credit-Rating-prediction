library(kohonen)
library(dummies)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)
library(arules)

pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

### DATA PREPARATION

# Census data comes in counts of people per area. 
# To compare areas, we will convert a number of the
# stats collected into percentages. Without this, 
# the primary differentiator between the different 
# areas would be population size.

# Load the data into a data frame
# Get the map of these areas and filter for Dublin areas.

data_raw <- read.csv("./creditworthiness.csv")  
head(data_raw)
idcol="functionary"
avgBal <- data_raw[, c(12,15,18,21,24,27,30,33,36,39,42,45)]
data_train <- data_raw[, c(1,2,3,4,6,7,45,46)]
names(data_train)[2] <- "Payback?"
names(data_train)[3] <- "FicoScore"
names(data_train)[5] <- "RefusedBefore?"
names(data_train)[7] <- "Balance"
names(data_train)[8] <- "CRating"
colMeans(avgBal[sapply(avgBal,is.numeric)])

data_som <- as.matrix(scale(data_train[, -8]))
summary(data_som)
set.seed(2022)
g <- somgrid(xdim = 5, ydim = 5, topo ="rectangular")
map <- som(data_som,
           grid = g)

plot(map,type = "codes")




