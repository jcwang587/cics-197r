########## Some Details on Aggregate

aggregate(MSUCCESS ~ SEX, data=evmem, FUN=length)

aggregate(MSUCCESS ~ SEX, data=evmem, FUN=mean, na.rm=T)

aggregate(MSUCCESS ~ SEX, data=evmem, FUN=quantile, probs=c(.4, .7))


years <- aggregate(MSUCCESS ~ MYEAR, data=evmem, FUN=length)

aggregate(MPERHIGHPT ~ MYEAR, data=evmem, FUN=quantile)
cbind(years, aggregate(MPERHIGHPT ~ MYEAR, data=evmem, FUN=quantile) )


rbind()

# What if don't know order?

merge(tabei_climbs, tabei_details, on=c("LNAME", "FNAME"))

# Exercise

years <- aggregate(HIRED ~ MYEAR, data=evmem, FUN=length)
names(years)[2] <- "TOTAL"

yearscountry <- aggregate(HIRED ~ MYEAR * CITIZEN, data=evmem, FUN=length)
names(yearscountry)[3] <- "N"

yearmerge <- merge(years, yearscountry)
yearmerge$PERCENT <- yearmerge$N / yearmerge$TOTAL



######### Writing Functions

aggregate(MPERHIGHPT ~ MYEAR, data=evmem, FUN=???? ) # max - min


# name <- function(parameters) { code }

name <- function() { print("Hello World!") }

add2 <- function(x) { x + 2 }

diff <- function(x) {
  mx <- max(x)
  mn <- min(x)
  value <- mx - mn
  return(value)
}

aggregate(MPERHIGHPT ~ FNAME * LNAME, data=evmem, FUN=diff ) # max - min


add <- function(a, b=1) {
  a + b
}

b <- 1

bad <- function(a) {
  a + b # confusing!
}





