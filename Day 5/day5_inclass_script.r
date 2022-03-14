evmem$SEASON_NAME <- as.factor(evmem$MSEASON)

# https://people.cs.umass.edu/~jmcchesney/197R/everest_members.csv

# Calculate per-decade percentage who are hired

evmem$DECADE <- cut(
  evmem$MYEAR,
  breaks = seq(1970, 2030, by=10),
  labels=seq(1970, 2020, by=10)
)

aggregate(HIRED ~ DECADE, data=evmem, FUN=mean)

## 

z <- aggregate(MSUCCESS ~ SEX * MYEAR, data=evmem, FUN=mean)

# first install packages below 
require(ggplot2)
require(ggformula)

gf_line(MSUCCESS ~ MYEAR | SEX, data=z)
gf_line(MSUCCESS ~ MYEAR, color = ~ SEX, data=z)
