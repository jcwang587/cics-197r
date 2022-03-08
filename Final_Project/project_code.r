### CICS 197R - Homework 11

#######################
# 1.Load the seedling growth data

plant <- read.csv("seedling_growth.csv")

#######################
# 1. Using aggregate() or data.table, confirm that there are 20 rows for every combination of the variables exposure, water, and recorded.

aggregate(plant$replicate, by=list(plant$exposure, plant$water, plant$recorded), FUN=length)

#######################
# 2. Convert the data to long format with melt, so that the three cultivars (Badenough, Gripus, and Nogo) are held in one grouping column.

plant.long <- melt(plant, measure.vars=c('Badenough', 'Gripus', 'Nogo'),
  variable.name = "cultivar", value.name = "height")
plant.long$X <- NULL

#######################
# 3. Aggregate across replicates (trials), grouping by exposure, water, time recorded, and cultivar, to find mean height.
# Again, use aggregate() or data.table. Store the result.

# Using aggregate
# (separating into multiple lines because the call is getting long)
# Also we can name items in a list, which is what aggregate uses in its grouping argument

plant.long.mean <- aggregate(
  height ~ exposure + water + recorded + cultivar,
  data=plant.long,
  FUN=mean, na.rm=T
)

gf_col(height ~ exposure | water * recorded, data = plant.long.mean)

#######################
# 4. Widen the data so that the three recorded values are their own columns: initial, intermediate, final; as well as replicate

plant.wide <- dcast(exposure + water + cultivar + replicate ~ recorded, data=plant.long, value.var='height')

#######################
# 5. Using the wide data, calculate the change in height from initial to final.
# Replace NA changes your calculate with zeroes.

plant.wide$change <- plant.wide$final - plant.wide$initial
plant.wide[is.na(plant.wide$change), 'change'] <- 0

#######################
# 6. Plot the change in heights using gf_boxplot().

lapply(plant.wide, class)
plant.wide$exposure <- as.factor(plant.wide$exposure)

gf_boxplot(change ~ exposure | cultivar * water, data=plant.wide)

# Hypothesis 1: Water increases height for Badenough and Gripus, but harms it in Nogo.
# Hypothesis 2: Higher exposure helps Badenough and Nogo, but harms Gripus.

#######################
# 7. Using a linear model, try to explain the height differences.

plant.lm <- lm(change ~ exposure * cultivar * water, data=plant.wide)
summary(plant.lm)
aov(plant.lm)

#######################
# 8. Using your model, predict the final height of a Badenough cultivar exposed at 2.9 with 4X water treatment, with initial height of 5.0.

test <- data.frame(exposure=factor(2.9), water='4X', initial=5, cultivar='Badenough')
test$change <- predict(plant.lm, test) # the model was based on the change, so that's what's predicted
test$final <- test$initial + test$change # add to initial to get the final
