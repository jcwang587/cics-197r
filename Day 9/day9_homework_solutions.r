##########
### Part I

### 1. Load the data

evex_1 <- read.csv("ev_exp_1of2.csv")
evex_2 <- read.csv("ev_exp_2of2.csv", na.strings = "NOTAVAIL")

### 2. Combine

evex_1$BCDATE <- as.Date(evex_1$BCDATE) # don'' need to specify format since it's the default format
evex_1$SMTDATE <- as.Date(evex_1$SMTDATE)

evex_2$BCDATE <- as.Date(evex_2$BCDATE, format="%d %b, %Y")
evex_2$SMTDATE <- as.Date(evex_2$SMTDATE, format="%d %b, %Y")

evex <- rbind(evex_1, evex_2)

### 3. Mean difference

mean(evex$SMTDATE - evex$BCDATE, na.rm=T)

##########
### Part II

### In 2019

is2019 <- format(evex$BCDATE, "%Y") == "2019" 
sum(is2019, na.rm=T)

### 2. In April

isapril <- format(evex$BCDATE, "%b") == "Apr" 
sum(isapril, na.rm=T)

### 3. Which are in 2019 and also in April?

sum(is2019 & isapril, na.rm=T)

### 4. How many unique occur on each date?

evex_apr19 <- evex[is2019 & isapril, ]
table(evex_apr19$BCDATE)

### 5. How many occur in each week?

table(cut(evex_apr19$BCDATE, "week"))
  # or:
table(format(evex_apr19$BCDATE, "%w"))
  # or:
aggregate(EXPID ~ format(BCDATE, "%w"), data=evex_apr19, FUN=length)
  # etc.

#############
### Part III

### 1. Load data

evext <- read.csv("ev_exp_terminate.csv")

evext$TERMDATE <- paste(evext$YEAR, evext$MONTH, evext$DAY, sep="-")
evext$TERMDATE <- as.Date(evext$TERMDATE, format="%Y-%m-%d")

### 2. Combine

evex <- merge(evex, evext)

### 3. Same-day termination

sum(evex$SMTDATE == evex$TERMDATE, na.rm=T)

### 4. 1-Day After

sum((evex$SMTDATE + 1) == evex$TERMDATE, na.rm=T)



