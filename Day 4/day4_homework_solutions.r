### Day 4 Homework
### Part I Prep

# 1. Load the data

lead <- read.csv("Everest_leaders.csv")
dim(lead) # look at size (dimensions)
names(lead) # names of columns
lead[1:5,] # first 5 rows; can also be done with: head(lead)

### Part II Top Climbers

# 1. Typical climbs

quantile(lead$CLIMBS)

# 2. Highest 3 number of climbs

top_3_climbs <- rev(sort(lead$CLIMBS))[1:3]

# 3. Top 3 climbers

top_3_index <- which(lead$CLIMBS %in% top_3_climbs)
lead[top_3_index, ]

# 4. Reorder by N climbs

index <- rev(order(lead$CLIMBS))
lead <- lead[index, ]

# 5. Country of top 25

lead[1:25, ]
countries <- table(lead[1:25, "CITIZEN"])
countries[countries > 0]

### Part III. Success Rate

# 1. Add success rate

lead$rate <- lead$SUCCESS / lead$CLIMBS

# 2. USA versus The World

mean(lead[lead$CITIZEN == "USA", "rate"])
mean(lead[lead$CITIZEN != "USA", "rate"])

# 3. Injuries

sum(lead$INJURIES > 0) # number
mean(lead$INJURIES > 0) # percentage

# 4. Injured but Victorious?

inj_succeed <- lead$INJURIES > 0 & lead$SUCCESS > 0
lead[inj_succeed, c("FNAME", "LNAME") ]

# 5. Comparing Success Rate

mean(lead[inj_succeed, "rate"])
mean(lead[, "rate"])

