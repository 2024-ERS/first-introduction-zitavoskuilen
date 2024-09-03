# script name: 06-relational-database-joins.R
# Combine cockle data with elevation data using a relational database approach 
# Schiermonnikoog transect

# clear everything in memory
rm(list=ls())

# load libraries
library(tidyverse) # including ggplot2, dplyr that we 

# load the elevation data and show the first 10 records of the dataset

# plot the change in transect  elevation along the transect, using a separate graph for each for each year 

# plot the change in transect  elevation along the transect, using a separate line color for each year 


# Extract the data for 2017 in a new tibble, keep only variables distance_m and elevation
# omit records where Distance_ID is missing (NA)

# read the cockle data 
# keep only the data for 2017, 
# omit observations (Obs_ID) 468 and 1531
# calculate the mean number of cockles and mean size for each distance

# plot (with a line and points)  how the number of cockles changes with distance along the transect


##### merge the cockle and elevation data into a single table you call "combidat"
# using Distance_ID as the common variable between the two tables


# show in a plot how cockle density changes with elevation


# fit a linear regression

# predicted y at x=0.5 m
# y = b0 + b1x   (b0 is intercept and b1 is the slope, x is elevation, y is no cockles


# show this model as a line in ggplot, with the confidence interval

# fit a better model, using a loess smoother
# show this model in ggplot


##### plot  how the size (as mean length) of cockles changes with  elevation along the transect
# omit the observations where length is NA (because no cockles were measures)
# fit a quadratic model (second-order polynomial)
# show for each point also the standard errors
# add appropriate labels for the x and y axis 
