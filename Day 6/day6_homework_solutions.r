##### Day 6 Homework

load("Tabei_data.rdata")

########## Part I

### I-1 

tabei <- cbind(tabei_climbs, tabei_details)

### I-2

names(tabei)
tabei[, c(9, 10,11)] <- NULL
# or:
tabei <- tabei[, -c(9, 10,11)]
# Note that which columns you need to remove depends on their order,
# and if you did cbind(tabei_details, tabei_climbs) it will change
# You can't remove them by name here, because that would remove them all
# (not just the duplicates).

  # Tip: There is a function to detect duplicates we could use:
  tabei <- tabei[ , -which(duplicated(names(tabei)))]


### I-3

tabei$age <- as.numeric(tabei$MYEAR) - as.numeric(tabei$YOB)
  # These were both character data, which we can't do math on

########## Part II

### II-1
dim(peaks1)
dim(peaks2)
dim(peaks3)

peaks <- rbind(peaks1, peaks2, peaks3)

### II-2
tabei <- merge(tabei, peaks)

### II-3
tabei$percent_hgt <- tabei$MPERHIGHPT / tabei$HEIGHTM

########## Part III

### III-1
aggregate(PEAKID ~ PYEAR, data=peaks, FUN=length)

### III-2
aggregate(HEIGHTM ~ PYEAR, data=peaks, FUN=max)

### III-3
topbot <- function(x) { min(x) / max(x) }
aggregate(HEIGHTM ~ PYEAR, data=peaks, FUN=topbot)
