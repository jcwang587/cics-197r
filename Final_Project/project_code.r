### CICS 197R - Final Project - Jiacheng Wang ###

# 1. Load the lithium batteries data
libattery <- read.csv("lithium-ion batteries.csv")
head(libattery)

# 2. Temporary save of the raw data, and drop "Material Id" (identifier)
lbdata <- libattery[ -c(1)]

# 3. Rename the Data Frame column (delete the dot and unit)
names(lbdata)[names(lbdata) == "Formation.Energy..eV."] <- "E_Formation"
names(lbdata)[names(lbdata) == "E.Above.Hull..eV."] <- "E_Above_Hull"
names(lbdata)[names(lbdata) == "Band.Gap..eV."] <- "Band_Gap"
names(lbdata)[names(lbdata) == "Density..gm.cc."] <- "Density"
names(lbdata)[names(lbdata) == "Has.Bandstructure"] <- "Bandstructure"
names(lbdata)[names(lbdata) == "Crystal.System"] <- "Crystal"

# 4. Find the total count of climbs in each season
aggregate(E_Formation ~ Spacegroup, data = lbdata, FUN = mean)

# . Linear regression model for Volume to Nsite
df <- data.frame(x = lbdata$Nsites, y = lbdata$Volume)

# create multiple linear model
linear_model <- lm(y ~ x, data=df)

# save predictions of the model in the new data frame 
predicted_df <- data.frame(pred = predict(lm_fit, df), hp=df$hp)
print(summary(linear_model))

require(ggplot2)
ggplot(df, aes(x = lbdata$Nsites, y = y)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")



