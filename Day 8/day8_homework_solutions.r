##############################
# Day 8 Homework
##############################

load("furniture_store.rdata")

##############################
# Part I

##### 1. Join people

person <- rbind(person1, person2)
levels(person$sex) <- c('F', 'M', 'O', 'F', 'M')
levels(person$eth)[5] <- NA

  # Note: We could/should remove duplicates here. Not explicitly required in HW though.


##### 2. Combined Names

person$name <- paste(person$first_name, person$last_name)


##### 3 Shortest and Longest

pnames <- unique(person$name) # better to work with unique, so we avoid any duplicates

## One approach to longest and shortest:

short <- min(nchar(pnames)) 
long <- max(nchar(pnames))

pnames[nchar(pnames) == short] # those with the shortest length
pnames[nchar(pnames) == long] # longest

  # Note that there are ties for both shortest and longest!


## A different approach to longest and shortest:

pnames <- pnames[order(nchar(pnames))] # rearrange by character length
pnanes[1] # first and "shortest"
pnanes[length(pnames)] # last and "longest"

  # Note that you miss the ties with this method; but if a single shortest
  # and longest is desired, this will do that by resolving the tie
  # (by default, order uses the original row order, so the first element wins.)


##### 3. Initials

person$initials <- paste(
  substr(person$first_name, 1, 1), # first initial
  substr(person$last_name, 1, 1),# last initial
  sep = "" # no space
)

any(duplicated(person$initials))  # any repeats? Yes!


##############################
# Part II

##### 1. Clean up prices

pricelist$price <- substr(pricelist$price, 2, 999) # remove $
pricelist$price <- gsub(",", "", pricelist$price) # remove commas (thousands)
pricelist$price <- as.numeric(pricelist$price) # convert type

##### 2. Join prices to transactions

trx <- merge(trx, pricelist, by='item')

##### 3. Find highest grossing date

trx_days <- aggregate(price ~ date, data=trx, FUN=sum) # totals per day

max(trx_days$price) # most made per day
trx_days[which(trx_days$price == max(trx_days$price)), "date"] # day it occurred on
# or a slightly shorter method with a new function
trx_days[which.max(trx_days$price), "date"]

##### 4. Best sales day for desks only

# First we need to be able to look just at desks. We'll try grepl.
trx_desks <- trx[grepl("DESK", trx$item), ] # get a TRUE for desks, and index by it

# Now we can do the same thing as above, with the new desk-only data frame
# For kicks, we'll find the best day via another method:

trx_desk_days <- aggregate(price ~ date, data=trx_desks, FUN=sum)

trx_desk_days <- trx_desk_days[rev(order(trx_desk_days$price)),] # reorder by total sales
trx_desk_days[1, "date"] # highest day for desks



##############################
# Part III

##### 1. Affect of sex on purchase price

# First we need to merge personal details onto the transactions
# And this requires making the IDs be the same type

trx$ID <- as.character(trx$ID) 
all <- merge(person, trx, by="ID")

# Now we can aggregate by sex:

aggregate(price ~ sex, data=all, FUN=mean)
  # Men's purchases are typically more expensive.


##### 2. How does sex relate to type of item purchased?

# We need a column denoting item type to do the aggregation:
all$type <- substr(all$item, 1, nchar(all$item) - 4)

aggregate(item ~ type * sex, data=all, FUN=length)
  # Women have made more purchases overall.
  # Women purchase chairs more than desks; men the reverse.

##### 3. Interaction?

aggregate(price ~ type * sex, data=all, FUN=mean)

  # Men prefer cheaper models of chairs when they buy chairs; and the same is true of desks.
  # But a greater % of men's puchases is desks, which always cost more than chairs,
  # inflating their average.


  
