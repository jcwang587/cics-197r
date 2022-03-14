as.numeric(members$MYEAR) -> members$MYEAR

index <- members$LNAME == "Tabei"
tabeiyears <- members[index & !is.na(index), "MYEAR"]

tabeiyears

# First test with one case, 1990:

last <- data[ members$MYEAR > (1990 - 5) & members$MYEAR < (1990 + 5) & members$SEX == "F", "LNAME"]
length(unique(last))

# Now we'll make a function to handle any possible case:

getyearrange <- function(y) {
  last <- members[ members$MYEAR > (y - 5) & members$MYEAR < (y + 5) & members$SEX == "F", "LNAME"]
  return(length(unique(last)))
}

# But it's bad form to use data from the environment. We'll make it an argument:

getyearrange <- function(y, data) {
  last <- data[ data$MYEAR > (y - 5) & data$MYEAR < (y + 5) & data$SEX == "F", "LNAME"]
  return(length(unique(last)))
}

# Test with the years of interest:

getyearrange(1970, members)
getyearrange(1970, members)
getyearrange(1975, members)
getyearrange(1990, members)
getyearrange(2006, members)

# But this gets tiresome, and isn't stored usefully either.
# sapply() will iterate for us

sapply(tabeiyears, FUN = getyearrange, data = members)

# We can write simple anonymous functions in an sapply too:

sapply(1:10, FUN = function(x) { print(x) } )

sapply(1:10, FUN = function(x) { x * 2 } )

1:10 * 2 # of course, vectorization means we don't need loops as much as in other languages

# Often the x isn't a real value but a counter or an index, e.g.:

sapply(1:5, FUN = function(x) { tabeiyears[x] } )



