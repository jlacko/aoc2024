input <- readLines("./input.txt")

# part one

mul_strings <- unlist(stringr::str_extract_all(input, "mul\\(\\d+,\\d+\\)"))

mul <- function(...)  `*`(...)

numbers <- sapply(mul_strings, \(x) eval(parse(text=x))) 

print(paste("result of corrupted multiplication", sum(numbers)))

# part two

mul_strings <- unlist(stringr::str_extract_all(input, "mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)"))

active <- TRUE

# iterate over mul_strings, removing 1) not active multiplication and 2) do / don't commands
for (i in seq_along(mul_strings)) {
   
   if (mul_strings[i] == "don't()") {
      active <- FALSE
   }
   
   if (mul_strings[i] == "do()") {
      active <- TRUE
   }
   
   if(!active | stringr::str_starts(mul_strings[i], "do")) {
      mul_strings[i] <- "0"
   } 
      
}


numbers <- sapply(mul_strings, \(x) eval(parse(text=x))) 

print(paste("result of enhanced multiplication", sum(numbers)))