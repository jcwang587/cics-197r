### CICS 197R - Final Project - Jiacheng Wang ###

# 1.Load the seedling growth data
libattery <- read.csv("lithium-ion batteries.csv")
head(libat)

# 2.Temporary save of the raw data, and drop "Material Id" (identifier)
data <- libattery[ -c(1)]

