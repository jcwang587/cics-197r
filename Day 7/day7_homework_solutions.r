###### Day 7 Homework Solutions

load("climbers.rdata")

### 1 

wasleader <- aggregate(LEADER ~ FNAME * LNAME, data=climbers, FUN = any)
sum(wasleader$LEADER) # 5,321 people were leaders!
mean(wasleader$LEADER) # which is 14% of all (non-hired) climbers

### 2 

gotto8k <- aggregate(MPERHIGHPT ~ FNAME * LNAME, data=climbers, FUN=function(x) {
  any(x >= 8000) 
})

mean(gotto8k$MPERHIGHPT) # 24% ever attains an 8-thousander peak

### 3

reachedpt <- function(x) {
  any(x >= 8000)
}

aggregate(MPERHIGHPT ~ FNAME * LNAME, data=climbers, FUN=reachedpt)

### 4

reachedpt <- function(x, point=8000) {
  any(x >= point)
}

# To test, we'll look at the percentage TRUE, all in one line:
mean(aggregate(MPERHIGHPT ~ FNAME * LNAME, data=climbers, FUN=reachedpt)$MPERHIGHPT)
mean(aggregate(MPERHIGHPT ~ FNAME * LNAME, data=climbers, FUN=reachedpt, point=2000)$MPERHIGHPT)
mean(aggregate(MPERHIGHPT ~ FNAME * LNAME, data=climbers, FUN=reachedpt, point=0)$MPERHIGHPT)
mean(aggregate(MPERHIGHPT ~ FNAME * LNAME, data=climbers, FUN=reachedpt, point=10000)$MPERHIGHPT)

### 5

reachedpt <- function(x, point=8000) {
  if(!is.numeric(x)) { stop("x must be a number") }
  any(x >= point)
}

reachedpt(c(5000, 2000, 85000))
reachedpt(c("Low", "High", "Very High"))

### 6

reachedpt <- function(x, point=8000) {
  if(!is.numeric(x)) { stop("x must be a number") }
  if(any(x < 0)) { stop("x should be positive only") }
  
  any(x >= point)
}

reachedpt(c(5000, 2000, 85000))
reachedpt(c(0, 5, -5))
