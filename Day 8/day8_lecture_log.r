read.csv("animals.tsv")

# Look at the file

animals <- read.csv(file="animals.tsv",
  header=F,
  sep="\t",
  col.names = c("ID", "species", "value"),
  na.strings = c("UNK", "NIL"),
  colClasses = c("character", "factor", "numeric")
)

############
load("furniture_store.rdata")

### Exercise: join person1 and person2

person <- rbind(person1, person2)
# fix sex
levels(person$eth)[5] <- NA

length(person$ID)
length(unique(person$ID))
duplicated(person$ID)

person[duplicated(person$ID), ]
dupIDs <- person[duplicated(person$ID), ID]
person[person$ID %in% dupIDs, ]
 # looks okay to remove duplicates

person <- person[!duplicated(person$ID), ]

### Let's fix trx and pricelist

trx$ID <- as.character(trx$ID)

# Price
pricelist$price <- substr(pricelist$price, 2, 999)

# Merge persons with trx

perstrx <- merge(person, trx)
aggregate(item ~ ID, data=perstrx, FUN=length)
aggregate(ID ~ item, data=perstrx, FUN=length)

perstrx$item
perstrx$item_id <- substr(perstrx$item, nchar(perstrx$item)-2, nchar(perstrx$item))

perstrx$item_type <- factor(substr(perstrx$item, 1, nchar(perstrx$item)-4))

aggregate(ID ~ item_type, data=perstrx, FUN=length)

# reverse operation:
paste(perstrx$item_type, perstrx$item_id, sep="-")

# Collect all item IDs for a person in one text element:
paste(perstrx[perstrx$ID == "738", item], collapse= ", ")

# Coming back to prices
pricelist$price <- as.numeric(gsub("," , "", pricelist$price))


# Now we can combine it all
all <- merge(perstrx, pricelist, by="item")

aggregate(price ~ ID, data=all, FUN=sum)