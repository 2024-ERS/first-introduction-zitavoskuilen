# Combine cockle data with elevation data using a relational database approach 
# Schiermonnikoog transect

# clear everything in memory
rm(list=ls())

# restore and load libraries
renv::restore()
library(tidyverse) # including ggplot2, dplyr that we 

# load the elevation data do not forget to put a piping sign after the link stupid 
elevdat <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=1550309563&single=true&output=csv")|>
dplyr::mutate(year=factor(year))# make year a factor

elevdat


# plot the change in transect  elevation along the transect, using a separate graph for each for each year 
elevdat |>
ggplot(aes(x=TransectPoint_ID, y=elevation_m)) + geom_line() + geom_point() + facet_wrap(~year)


# plot the change in transect  elevation along the transect, using a separate line color for each year 
elevdat |> 
  ggplot(aes(x=TransectPoint_ID, y=elevation_m, col=year)) + geom_line() 



# Extract the data for 2017 in a new tibble, keep only variables distance_m and elevation
# omit records where Distance_ID is missing (NA)
# we are making a new tibble with only the data for 2017

elevdat2017 <- elevdat |>
  dplyr::filter(year=="2017") |>
  dplyr::select(TransectPoint_ID, elevation_m) |>
  dplyr::filter(!is.na(TransectPoint_ID))


# read the cockle data 
# keep only the data for 2017, 
# omit observations (Obs_ID) 468 and 1531
# calculate the mean number of cockles and mean size for each distance
cdat <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSpormjGiM4uWMTIZYm9Upt5j1Ige_Wu9CrhGGdvjXwxmHP2R6S7JDyEmiGTn3fXihl5s5yBn_sdo8h/pub?gid=1538766002&single=true&output=csv") 

cdat2017<-cdat|>  
dplyr::filter(year==2017, 
                CockleObs_ID!=468,
                CockleObs_ID!=1531) |>
  dplyr::group_by(TransectPoint_ID) |> #group by distance
  dplyr::summarise(n_obs = n(),
                    avg_l=mean(length_mm, na.rm=T),
                    sd_l=sd(length_mm, na.rm=T),
                    se_l=sd_l/sqrt(n_obs))
print(cdat2017)



# plot (with a line and points)  how the number of cockles changes with distance along the transect
cdat2017 |>
  ggplot(aes(x=TransectPoint_ID, y=n_obs)) + geom_line() + geom_point()

##### merge the cockle and elevation data into a single table you call "combidat"
# always put the longest table first, so you do not lose data 
# using Distance_ID as the common variable between the two tables

combidat <- dplyr::left_join(elevdat2017, cdat2017, by="TransectPoint_ID") |>
  dplyr::mutate(n_obs=tidyr::replace_na(n_obs, 0)) # replace NA with 0

combidat


# show in a plot how cockle density changes with elevation

combidat |>
  ggplot(aes(x=elevation_m, y=n_obs)) + geom_point()

# fit a linear regression

combidat |>
  ggplot(aes(x=elevation_m, y=n_obs)) + geom_point() +
  geom_smooth(method = "lm", se = T
  ) # but this makes the lm below zero which is not possible

combidat |>
  ggplot(aes(x=elevation_m, y=n_obs)) + geom_point() +
  geom_smooth(method = "loess"
  ) # now it doesn't go below zero anymore 

# predicted at 0.5 m (x)
# y = b0 + b1x   (b0 is intercept and b1 is the slope, x is elevation, y is no cockles)


# show this model as a line in ggplot, with the confidence interval

# fit a better model, using a loess smoother
# show this model in ggplot

##### plot how the size (as mean length) of cockles changes with  elevation along the transect
# omit the observations where length is NA (because no cockles were measures)

combidat |>
  dplyr::filter(!is.na(avg_l)) |>
  ggplot(aes(x=elevation_m, y=avg_l)) + geom_point()


# fit a quadratic model (second-order polynomial)

combidat |>
  dplyr::filter(!is.na(avg_l)) |>
  ggplot(aes(x=elevation_m, y=avg_l)) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) # Fit a quadratic model

# show for each point also the standard errors
# add appropriate labels for the x and y axis 

combidat |>
  dplyr::filter(!is.na(avg_l)) |>
  ggplot(aes(x=elevation_m, y=avg_l)) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) + 
  labs(x="Elevation (m)", y="Mean cockle length (mm)")


