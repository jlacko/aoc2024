input <- readLines("./input.txt")

# part one

rules <- input[1:which(input == "")-1]
updates <- input[(which(input == "")+1):length(input)]


rule_check <- function(rule, update_vect) {
   
   
   first_page <- stringr::str_split(rule, "\\|")[[1]][1]
   second_page <- stringr::str_split(rule, "\\|")[[1]][2]
   
   # are both pages present? if not then all clear...
   if(!all(is.element(c(first_page, second_page), update_vect))) return(TRUE)
   
   first_loc <- which(update_vect == first_page)
   second_loc <- which(update_vect == second_page)
   
   # first page before second
   result <-  first_loc < second_loc
   
   result

   
}

sum_of_middles <- 0

for (i in seq_along(updates)) {
   
   # updates as a vector
   update <- stringr::str_split(updates[i], "\\,")[[1]]
   
   correct_rules <- sapply(rules, \(x) rule_check(x, update))
   
   if(all(correct_rules)) {
      
      mid_string <- update[.5 + length(update)/2]
      
      sum_of_middles <- sum_of_middles + as.numeric(mid_string)
   }
   
}

print(paste("sum of correct middles", sum_of_middles))


# part two

sum_of_middles <- 0


for (i in seq_along(updates)) {
   
   # updates as a vector
   update <- stringr::str_split(updates[i], "\\,")[[1]]
   
   correct_rules <- sapply(rules, \(x) rule_check(x, update))
   
   # at least one rule is incorrect...
   if(!all(correct_rules)) {
      
      # until all rules have been corrected...
      while(!all(correct_rules)) {
         
         # first offender - there are likely to be more of them
         offender <- rules[!correct_rules][1]
         
         first_page <- stringr::str_split(offender, "\\|")[[1]][1]
         second_page <- stringr::str_split(offender, "\\|")[[1]][2]
         
         first_loc <- which(update == first_page)
         second_loc <- which(update == second_page)
         
         # correct the rule
         update[second_loc] <- first_page
         update[first_loc] <- second_page
         
         # do a recheck of rules (in case of multiple offences, oh horror!)
         correct_rules <- sapply(rules, \(x) rule_check(x, update))
         
      }
      
      
      mid_string <- update[.5 + length(update)/2]
      
      sum_of_middles <- sum_of_middles + as.numeric(mid_string)
   }
   
}

print(paste("sum of corrected middles", sum_of_middles))
