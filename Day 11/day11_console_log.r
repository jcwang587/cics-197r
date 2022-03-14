papers <- read.csv("papers.csv")
conf <- read.csv("conference.csv")

require(reshape2)

### Long

conflong <- melt(conf)
names(conflong) <- c("status", "year", "N")
conflong$year <- as.numeric(substr(conflong$year, 2, 5))


melt(papers) # oops!
paperslong <- melt(papers, id.vars=c("year"))
names(paperslong) <- c("year", "status", "N")

all <- rbind(paperslong, conflong)

aggregate(N ~ status, data=all, FUN=mean)

### Wide

# Change conflong to match papers
confmiddle <- dcast(year ~ status , data = conflong, value.var="N")

# Add an identifying column for type of paper
confmiddle$type <- "conference"
papers$type <- "journal"
papers$given <- NA

allwide <- rbind(papers, confmiddle)

allwide$rejected <- allwide$submitted - allwide$accepted

### Too wide
toowide <- read.csv("toowide.csv")

nottoowide <- melt(toowide, id.vars="status")
nottoowide <- nottoowide[-(1:3), ]
nottoowide$variable <- as.character(nottoowide$variable)

nottoowide$year <- as.numeric(substr(nottoowide$variable, nchar(nottoowide$variable) - 3, 999) )
nottoowide$type <- as.factor(substr(nottoowide$variable, 1, nchar(nottoowide$variable) - 5))
nottoowide$variable <- NULL

#######################

model <- lm(value ~ year * type, data=nottoowide, subset = status=='submitted')

scenario <- data.frame(
  status = "submitted", 
  type = c("conference", "journal"),
  year = 2040,
  value = NA)

scenario$value <- predict(model, scenario)

# predict will not use NAs -- must impute
# exact match of data types

########### Lists!

# A list if a vector -- can have anything in its elements

z <- list( c("hello world"), c(56:34), c(TRUE), list(
  "inseide list"), all)

mode(model)
names(model)
model$coefficients
model[[1]][1]

mode(all)
length(all)
lapply(all, class)

lapply(z, length)
str(z)

unlist(z, recursive=FALSE)