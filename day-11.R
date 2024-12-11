input <- readLines("./input.txt")

#input <- c("125 17")

# part one

xvector <- as.numeric(unlist(strsplit(split = " ", paste0(input, collapse = ""))))


blink <- function(stones) {
   
   res <- c()
   
   for (i in seq_along(stones)) {
      
      if (stones[i] == 0) {
         
         res <- c(res, 1)
         
      } else {
         
         if (nchar(stones[i]) %% 2 == 1) res <- c(res, 2024 * stones[i])
         
         if (nchar(stones[i]) %% 2 == 0) res <- c(res, 
                                                  as.numeric(substring(stones[i], 1, nchar(stones[i])/2)),
                                                  as.numeric(substring(stones[i], nchar(stones[i])/2+1, nchar(stones[i]))))
         
         
      }
      
   }
   
   res
   
}

# part one

stones <- xvector

for (blinks in 1:25) {
   
   stones <- blink(stones)
   

   print(paste("result after", blinks, "blinks:", length(stones)))
   
}

# part two

