input <- read.table("./day-01/input.txt", sep = " ")

first <- sort(input[,1])
second <- sort(input[,4])

diff <- abs(first - second)

print(paste("difference between lists is", sum(diff)))

score <- 0

for (i in seq_along(first)) {
   
   score <- score + first[i] * sum(first[i] == second)
   
} 
   

print(paste("similarity score is", score))
