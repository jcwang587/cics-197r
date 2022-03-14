load("furniture.rdata")

furn$date <- as.Date(furn$date)

furn$date[1]
furn$date[1] + 2
furn$date[1] - furn$date[2]

furn$date[1] == furn$date[2]
furn$date[1] > furn$date[2]

z <- c("Jan-05-2019", "Dec-12-2017")

?strptime

as.Date(z, format="%b-%d-%Y")

z <- c("Jan. 05, 2019", "Dec. 12, 2017")
z <- as.Date(z, format="%b. %d, %Y")

sort(z)

### Sequences

seq(3, 6, by=.5)

seq(z[2], z[1], 7)
seq(z[2], z[1], "month")
seq(z[2], z[1], "quarter")

cut(furn$date, "month")
cut(furn$date, 7)

### Exercise

# Get Everest "expeditions" (exped)
# Make plots of number of climbs over time
# Do so by year, month, week, day
# Using the summit date (SMTDATE)

exped$SMTDATE <- as.Date(exped$SMTDATE)

table(cut(exped$SMTDATE, "year"))

exped$SMT_YEAR <- cut(exped$SMTDATE, "year")
aggregate(EXPID ~ SMT_YEAR, data=exped, FUN=length)


exped$SMT_MONTH <- cut(exped$SMTDATE, "month")
# compare:
exped$SMT_MONTH <- format(exped$SMTDATE, "%b")

aggregate(EXPID ~ SMT_MONTH, data=exped, FUN=length)


###### Absent Rows (Non-events)


possible_years <- seq(
  min(exped$SMTDATE, na.rm=T),
  max(exped$SMTDATE, na.rm=T),
  by="year")

data.frame(YEAR = possible_years, climbs = 0)





