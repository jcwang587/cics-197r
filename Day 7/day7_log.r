aggregate(PEAKID ~ LNAME * FNAME, data=climbers, FUN=length)

countunique <- function(x) {
  length(unique(x))
}

aggregate(PEAKID ~ LNAME * FNAME, data=climbers, FUN=countunique)


aggregate(PEAKID ~ LNAME * FNAME, data=climbers,
  FUN = function(x) { length(unique(x)) }
)

# Anonymous Function

####

# Number of climbs per person
count <- aggregate(PEAKID ~ LNAME * FNAME, data=climbers, FUN=length)
names(count)[3] <- "CLIMB_COUNT"

dim(merge(climbers, count)) # Oops! Lost rows

climbers <- merge(climbers, count, all=T)

####

# Exercise:
# Calculate the first year of a climb per person
# Calculate the n years between each climb and first

climbers$MYEAR <- as.integer(climbers$MYEAR)

firstyear <- aggregate(MYEAR ~ LNAME * FNAME, data=climbers, FUN = min)
names(firstyear)[3] <- "FIRSTYEAR"

climbers <- merge(firstyear, climbers, all=T)

climbers$YEARDIFF <- climbers$MYEAR - climbers$FIRSTYEAR


######

rates <- runif(20)

diff <- function(x) {
  if(is.logical(x)) { x <- as.numeric(x) }
  if(!is.numeric(x)) { stop("x must be a number") }
  if(length(x) < 2) { warning("x is length 1, 2 or more is expected")  }
  
  x[-length(x)] - x[2:length(x)]
}


