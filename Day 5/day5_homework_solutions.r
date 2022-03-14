###################################
##### Day 5 Homework
###################################

##### Prep

evmem <- read.csv("everest_members.csv") # Load the data

require(ggplot2) # load graphics package
require(ggformula) # load easier graphics interface

##############
### Part I

### 1. Change the season codes to usable terms...

evmem$MSEASON <- factor(evmem$MSEASON)
levels(evmem$MSEASON) <- c("Spring", "Summer", "Fall", "Winter")

### 2. Find out how many climbs occurred in each season.
### Graph the above with gf_col()

climbs_season <- aggregate(MSUCCESS ~ MSEASON, data=evmem, FUN=length)
  # note that we use MSUCCESS here but it doesn't matter which variable we use,
  # because length() will always give the same result (the value doesn't matter)

gf_col(MSUCCESS ~ MSEASON, data=climbs_season)

### 3. Get the success rate per season.

success_season <- aggregate(MSUCCESS ~ MSEASON, data=evmem, FUN=mean)
gf_col(MSUCCESS ~ MSEASON, data=success_season)


##############
### Part II

### 1. Now find the number per year and per season (and store the result)

climbs_ys <- aggregate(MSUCCESS ~ MSEASON * MYEAR, data=evmem, FUN=length)

### 2. Graph with gf_line(). Use `color = ` to create multiple lines

gf_line(MSUCCESS ~ MYEAR, color = ~ MSEASON, data=climbs_ys)

### 3. Now use gf_col to do the same plot. Use `fill = ` to change the colors. What else has changed about the values displayed?

gf_col(MSUCCESS ~ MYEAR, fill = ~ MSEASON, data=climbs_ys)
  # Values are now stacked - each year shows the total (for all seasons)

##############
### Part III

### 1. Get yearly counts, overall and for spring alone

climbs_yr <- aggregate(MSUCCESS ~ MYEAR, data=evmem, FUN=length) # Overall

evmem_spring <- evmem[evmem$MSEASON == "Spring", ]
climbs_yr_spring <- aggregate(MSUCCESS ~ MYEAR, data=evmem_spring, FUN=length) #  Spring only

climbs_yr$spring <- climbs_yr_spring$MSUCCESS
names(climbs_yr) <- c("MYEAR", "TOTAL", "SPRING")

### 2. Calculate spring %

climbs_yr$spring_pct <- climbs_yr$SPRING / climbs_yr$TOTAL

### Graph

gf_line(spring_pct ~ MYEAR, data=climbs_yr)