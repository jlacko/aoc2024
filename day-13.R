input <- readLines("./input.txt")

# input <- c("Button A: X+94, Y+34",
#            "Button B: X+22, Y+67",
#            "Prize: X=8400, Y=5400",
#            "",
#            "Button A: X+26, Y+66",
#            "Button B: X+67, Y+21",
#            "Prize: X=12748, Y=121",
#            "",
#            "Button A: X+17, Y+86",
#            "Button B: X+84, Y+37",
#            "Prize: X=7870, Y=6450",
#            "",
#            "Button A: X+69, Y+23",
#            "Button B: X+27, Y+71",
#            "Prize: X=18641, Y=10279")

# part one

# clean data 

input <- input[input != ""]

numbers <- as.numeric(unlist(stringr::str_extract_all(input, "\\d+")))


# part one

library(lpSolve)

result <- 0

for (machine in 1:(length(numbers) / 6)-1) {
   
   # objective function = cost of tokens
   objective.fn <- c(3, 1) 
   
   const.mat <- matrix(c(numbers[6 * machine + 1], # A moves in X direciton
                         numbers[6 * machine + 3], # B moves in X direction
                         numbers[6 * machine + 2], # A moves in Y direction
                         numbers[6 * machine + 4], # B moves in Y direction
                         0, 1, 
                         1, 0) , ncol=2 , byrow=TRUE) # 
   const.dir <- c("=", "=", "<=", "<=")
   const.rhs <- c(numbers[6 * machine + 5], # target in X direciton
                  numbers[6 * machine + 6], # target in Y direction 
                  100, 100) # How else would someone be expected to play?
   

   # let linear programming work its magic!
   lp.solution <- lp("min", objective.fn, const.mat, all.int = TRUE,
                     const.dir, const.rhs)
   
   result <- result + lp.solution$objval
   
}


print(paste("minimum tokens to win is", result))

# part two

result <- 0

for (machine in 1:(length(numbers) / 6)-1) {
   
   # objective function = cost of tokens
   objective.fn <- c(3, 1) 
   
   const.mat <- matrix(c(numbers[6 * machine + 1], # A moves in X direciton
                         numbers[6 * machine + 3], # B moves in X direction
                         numbers[6 * machine + 2], # A moves in Y direction
                         numbers[6 * machine + 4], # B moves in Y direction
                         0, 1, 
                         1, 0) , ncol=2 , byrow=TRUE) # 
   const.dir <- c("=", "=", ">", ">")
   const.rhs <- c(10000000000000 + numbers[6 * machine + 5], # target in X direciton
                  10000000000000 + numbers[6 * machine + 6], # target in Y direction 
                  100, 100) # it will take many more than 100 presses to do so
   

   # let linear programming work its magic!
   lp.solution <- lp("min", objective.fn, const.mat, all.int = TRUE,
                     const.dir, const.rhs, scale = 0)
   
   result <- result + lp.solution$objval
   
}


print(paste("adjusted minimum tokens to win is", result))