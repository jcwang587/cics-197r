### Day 3 Quiz
# (Indexing etc.)

v <- 1:26
v <- v[c(T,F)]
v[7] <- 26 # Ideally: v[ceiling(length(v)/2)] <- 26
v[1:5] <- v[1:5] * 2
v <- v[length(v):1]
LETTERS[v]

