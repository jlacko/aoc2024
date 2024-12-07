input <- readLines("./input.txt")


# input <- c("190: 10 19",
#            "3267: 81 40 27",
#            "83: 17 5",
#            "156: 15 6",
#            "7290: 6 8 6 15",
#            "161011: 16 10 13",
#            "192: 17 8 14",
#            "21037: 9 7 18 13",
#            "292: 11 6 16 20")

# part one
calibration_equations <- stringr::str_extract_all(input, "\\d+")

calculate <- function(x, y, fun = ...) {
   as.numeric(do.call(fun, list(x, y)))
}


test_calibration <- function(equation) {
   
   test_output <- as.numeric(equation[1])
   test_input <- as.numeric(equation[-1])

   operations <- length(test_input) -1
   
   operators <- c("+", "*")
   
   variants <- gtools::permutations(2, operations, operators, repeats.allowed = T)
   
   for (i in 1:nrow(variants)) {
      
      result <- purrr::reduce2(test_input, variants[i, ], calculate)
      
      if (identical(result, test_output)) {
         
         # happy ending - return early
         return(test_output)
      }

   }
   
   # sad panda...
   0
   
}

total_result <- 0

for (i in seq_along(input)) {
   
   if (i %% 100 == 0){
      
      # ET call home - we are not stuck, just slooooow...
      print(paste(Sys.time(), 
                  "processed", 
                  i,
                  "inputs, ",
                  length(input) - i,
                  "to go...")) 
      
   } 

   total_result <- total_result + test_calibration(calibration_equations[[i]])
      
}


print(paste("total calibration result is", total_result))

# part two

# infix operator for concatenation
`%copr%` <- function(...) {
   paste0(...)
}


test_calibration_copr <- function(equation) {
   
   test_output <- as.numeric(equation[1])
   test_input <- as.numeric(equation[-1])
   
   operations <- length(test_input) -1
   
   operators <- c("+", "*", "%copr%")
   
   variants <- gtools::permutations(3, operations, operators, repeats.allowed = T)
   
   for (i in 1:nrow(variants)) {
      
      result <- purrr::reduce2(test_input, variants[i, ], calculate)
      
      if (identical(result, test_output)) {
         
         # happy ending - return early
         return(test_output)
      }
      
   }
   
   # sad panda...
   0
}

total_result <- 0

for (i in seq_along(input)) {
   
   if (i %% 100 == 0){
      
      # ET call home - we are not stuck, just slooooow...
      print(paste(Sys.time(), 
                  "processed", 
                  i,
                  "inputs, ",
                  length(input) - i,
                  "to go...")) 
      
   } 
   
   total_result <- total_result + test_calibration_copr(calibration_equations[[i]])
   
}

print(paste("total calibration result with copr is", total_result))
