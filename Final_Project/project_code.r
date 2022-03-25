### CICS 197R - Final Project - Jiacheng Wang ###

library(ggplot2)

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

# 4. Aggregate Energy data by Spacegroup based with max or min method
aggregate(E_Formation ~ Spacegroup, data = lbdata, FUN = max)
aggregate(E_Above_Hull ~ Spacegroup, data = lbdata, FUN = min)

# 5. Plot distribution for Band Gap
print(ggplot(lbdata, aes(x=Band_Gap)) + geom_histogram(binwidth=.5))

# 6. Linear regression model for Volume to Nsite
data_VN <- data.frame(Volume = lbdata$Volume, Nsites = lbdata$Nsites)

# 7. Fit linear regression model to dataset and view model summary
linear_model <- lm(Volume ~ Nsites, data=data_VN)
print(summary(linear_model))

# 8. Create plot to visualize fitted linear regression model
p1 <- ggplot(data_VN,aes(Nsites, Volume)) + 
  geom_point() + 
  geom_smooth(formula = y ~ x, method='lm', color='turquoise4') +
  theme_minimal() +
  labs(x='Nsites', y='Volume', 
       title='Linear Regression Fit for Volume by Nsites') +
  theme(plot.title = element_text(hjust=0.5, size=15, face='bold')) 
print(p1)

# 9. Add new column of Density
data_VND <- cbind(data_VN, Density = lbdata$Density, Crystal = lbdata$Crystal)

# 10. Function for calculating the average density and converting the unit
gmcc2kgmcm <- function(gmcc){
  kgmcm <- mean(gmcc) * 1000
  return(kgmcm)
}

# 11. Aggregate Density by Crystal System with the defined function
print(aggregate(data_VND$Density, by = list(data_VND$Crystal), FUN=gmcc2kgmcm))



