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

# 5. Linear regression model for Volume to Nsite
data_VN <- data.frame(Volume = lbdata$Volume, Nsites = lbdata$Nsites)

# 6. Fit linear regression model to dataset and view model summary
linear_model <- lm(Volume ~ Nsites, data=data_VN)
summary(linear_model)

# 7. Create plot to visualize fitted linear regression model
library(ggplot2)
p <- ggplot(data,aes(x, y)) + 
  geom_point() + 
  geom_smooth(formula = y ~ x, method='lm', color='turquoise4') +
  theme_minimal() +
  labs(x='Nsites', y='Volume', title='Linear Regression Plot') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 
print(p)

# 8. Add new column of Band
data_VNB <- cbind(data_VN, Bandstructure = lbdata$Bandstructure)

