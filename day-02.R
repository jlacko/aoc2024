input <- readLines("./input.txt")

# part one

safe_reports <- rep(FALSE, length(input))

for (i in seq_along(input)) {
   
   report <- as.numeric(unlist(stringr::str_extract_all(input[i], "\\d+")))
   
   increasing_report <- rep(FALSE, length(report)-1)
   decreasing_report <- rep(FALSE, length(report)-1)
   
   for (j in seq_along(increasing_report)) {
   
      # expected increase
      increasing_report[j] <-  report[j] == report[j+1] - 1 | report[j] == report[j+1] - 2 | report[j] == report[j+1] - 3
         
      # expected decrease
      decreasing_report[j] <-  report[j] == report[j+1] + 1 | report[j] == report[j+1] + 2 | report[j] == report[j+1] + 3
   
   }
   
   # either all increase or all decrease *expectedly*
   safe_reports[i] <- all(increasing_report) | all(decreasing_report)
   
} 

print(paste("count of safe reports is", sum(safe_reports)))

# part two

safe_reports <- rep(TRUE, length(input))

# function to check report safety
safe_report <- function(report) {
   
   increasing_report <- rep(FALSE, length(report)-1)
   decreasing_report <- rep(FALSE, length(report)-1)
   
   for (j in seq_along(increasing_report)) {
      
      # expected increase
      increasing_report[j] <-  report[j] == report[j+1] - 1 | report[j] == report[j+1] - 2 | report[j] == report[j+1] - 3
      
      # expected decrease
      decreasing_report[j] <-  report[j] == report[j+1] + 1 | report[j] == report[j+1] + 2 | report[j] == report[j+1] + 3
      
   }
   
   # either all increase or all decrease *expectedly*
   safe_reports[i] <- all(increasing_report) | all(decreasing_report)
   
}


for (i in seq_along(input)) {
   
   report <- as.numeric(unlist(stringr::str_extract_all(input[i], "\\d+")))
   
   # init
   dampener <- FALSE
   
   # try safety with dampener, removing iteratively each level
   for (j in seq_along(report)) {

      if (safe_report(report[-j])) dampener <- TRUE
      
   }
   
   # either the report is safe by itself, or dampener has saved the day
   safe_reports[i] <- safe_report(report) | dampener   
   
} 

print(paste("count of safe reports with dampener is", sum(safe_reports)))