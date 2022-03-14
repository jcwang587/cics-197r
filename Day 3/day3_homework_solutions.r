##### CICS 197R
##### Day 3 Homework

# Prep

load("day2_homework_data.rdata")
nexped[is.na(nexped)] <- 0

### Part I: Mondays

DoW <- factor(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) # days of week
expDoW <- rep(DoW, times = 9)[1:length(nexped)]

# 1. Percentage of days that are Mondays:
mean(expDoW == "Mon")

# 2. Days that are Monday and have climbs:
sum(nexped > 0 & expDoW == "Mon")

# 3. Mean number of climbs on Mondays:
mondayclimbs <- nexped[expDoW == "Mon"]
mean(mondayclimbs)

# 4. Percentage of Mondays that have any climbs:
mean(mondayclimbs > 0)

# 5. Percentage of all climbs occurring on a Monday
sum(mondayclimbs) / sum(nexped)

# 6. % climbs on Mon, Tue, or Wed
mtw_climbs <- nexped[expDoW %in% c("Mon", "Tue", "Wed")]
sum(mtw_climbs) / sum(nexped)

### Part II: Weekends

# 1. Make weekend factor:
weekend <- expDoW
levels(weekend) <- c("Week-Day", "Week-Day", "Weekend", "Weekend", "Week-Day", "Week-Day", "Week-Day")

# 2. Percentage climbs on week-days:
weekdayclimbs <- nexped[weekend == "Week-Day"]
sum(weekdayclimbs) / sum(nexped)

# 3. How does above differ from expected (null hypothesis)?
ideal <- 5/7
# Answer: Actual week-day climbs are only slightly lower than if day didn't matter. Probably day doesn't matter to climbers.


### Part III: Bins

# 1. Find break point
mid_break <- mean(nexped[nexped > 0])

# 2. Make category factor (see next answer too):
climb_bin <- cut(nexped, breaks = c(-1, 0, mid_break, Inf), labels = c("No climbs", "Few climbs", "Many climbs"))

# A different way of doing 3 by hand:
climb_bin <- rep("", length(nexped)) # set up blank vector
climb_bin[nexped == 0] <- "No climbs"
climb_bin[nexped <= mid_break & nexped > 0] <- "Few climbs" # Note that we have to exclude the 0 days, or they'll all become "Few"!
climb_bin[nexped > mid_break] <- "Many climbs"
climb_bin <- factor(climb_bin)

# 3. Count of days in each bin:
table(climb_bin)

# 4. Graph:
barplot(table(climb_bin))
