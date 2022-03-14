######################
### Day 10 Homework
######################

### Load the Himalaya data.

load("himalaya.Rdata")

### Create a subset of data from `exped` for just Everest (use the PEAKID)

evexp <- exped[exped$PEAKID == 'EVER',]

##########
### Part I

### We want to relate the HIGHPOINT achieved by each expedition
### to other variables. At first, TOTDAYS taken.

### Create a linear model relating high point to total days, and look at the summary output.

mod_totdays <- lm(HIGHPOINT ~ TOTDAYS, data=evexp)
summary(mod_totdays)

### Plot the same data with plot() -- the formula notation works.
### THen use abline() to add the model line. You can use "col = 'red'" to make the line stand out.

plot(HIGHPOINT ~ TOTDAYS, data=evexp)
abline(mod_totdays, col='red')

###########
### Part II

### Make another model that adds the variable O2CLIMB, with interaction effects. (TOTDAYS should still be included.)

mod_O2 <- lm(HIGHPOINT ~ TOTDAYS * O2CLIMB, data=evexp)

### Looking at the summary output, which variables are significant?

summary(mod_O2)
# All are significant (p < 2.2e-16).

### The "Estimate" column gives the slope of the line. Do more days, and using O2, increase or decreases height achieved?
### What about the combination of the two?

summary(mod_O2)
# Using O2 brings more than 3000 meters!
# Each day taken improves height by 45 meters.
# But more days actually decreases height when O2 is used; making it days taken a wash then (or slightly negative).


