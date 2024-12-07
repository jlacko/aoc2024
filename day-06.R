print(paste(Sys.time(), "work started"))

input <- readLines("./input.txt")

# input <- c("....#.....",
#            ".........#",
#            "..........",
#            "..#.......",
#            ".......#..",
#            "..........",
#            ".#..^.....",
#            "........#.",
#            "#.........",
#            "......#...")

# part one

xvector <- unlist(strsplit(split = "", paste0(input, collapse = ""))) 

xmatrix <- matrix(xvector, sqrt(length(xvector)), byrow = T)

# starting position & direction
position <- which(xmatrix == "^")
direction <- "T"

# directions in matrix
moves <- c("R" = dim(xmatrix)[2],
           "B" = +1,
           "L" = -dim(xmatrix)[1],
           "T" = -1)
# note the bold *Including the guard's starting position* :)
positions_visited <- c(position)


repeat {

   # is the road ahead clear?
   if (xmatrix[position + moves[direction]] != "#") {
      
      # then move!
      position <- position + moves[direction]
      positions_visited <- c(positions_visited, position)
         
   } else {
      
      # turn 90` - cycling over the directions vector
      direction <- names(moves)[which(names(moves) == direction) + 1 - 4 * (direction == "T")]
      
   }
   

   if(# exit conditions 
         (position %in% which(col(xmatrix) == 1) & direction == "L") |
         (position %in% which(row(xmatrix) == dim(xmatrix)[2]) & direction == "B") |
         (position %in% which(col(xmatrix) == dim(xmatrix)[1]) & direction == "R") |
         (position %in% which(row(xmatrix) == 1) & direction == "T")
   ) break

}


print(paste(Sys.time(), "count of positions visited is", length(unique(positions_visited))))


# part two

# target positions to process 
target <- length(unique(positions_visited))

xvector <- unlist(strsplit(split = "", paste0(input, collapse = ""))) 

xmatrix <- matrix(xvector, sqrt(length(xvector)), byrow = T)

# starting position & direction
position <- which(xmatrix == "^")
direction <- "T"

# directions in matrix
moves <- c("R" = dim(xmatrix)[2],
           "B" = +1,
           "L" = -dim(xmatrix)[1],
           "T" = -1)
positions_visited <- c(position)
loop_locs <- c()

repeat {
   
   # is the road clear?
   if (xmatrix[position + moves[direction]] != "#") {
     
      # move!
      position <- position + moves[direction]
      positions_visited <- c(positions_visited, position)
      
      if (length(unique(positions_visited)) %% 250 == 0){
         
         # ET call home - we are not stuck, just slooooow...
         print(paste(Sys.time(), 
                     "positions processed", 
                     length(unique(positions_visited)),
                     "-",
                     target - length(unique(positions_visited)),
                     "to go...")) 
         
      } 
   } else {
      
      # turn 90` - cycling over the directions vector
      direction <- names(moves)[which(names(moves) == direction) + 1 - 4 * (direction == "T")]
      
   }
   
   # send a probe in alternative direction = FAFO
   alt_xmatrix <- xmatrix
   
   # try setting up an obstalcle, unless doing so would break the matrix / maze 
   if (position + moves[direction] <= length(xmatrix)) {
      alt_xmatrix[position + moves[direction]] <- "#"
   }
   
   alt_position <- position
   alt_direction <- names(moves)[which(names(moves) == direction) + 1 - 4 * (direction == "T")]
   alt_positions_visited <- data.frame(position = c(),
                                   direction = c())
   
   repeat {
      
      # is the road ahead clear?
      if (alt_xmatrix[alt_position + moves[alt_direction]] != "#") {
         
         # move!
         alt_position <- alt_position + moves[alt_direction]
         alt_positions_visited <- rbind(alt_positions_visited,
                                        data.frame(position = alt_position,
                                                   direction = alt_direction))
         
      } else {
         
         # turn 90` - cycling over the directions vector
         alt_direction <- names(moves)[which(names(moves) == alt_direction) + 1 - 4 * (alt_direction == "T")]
         
      }
      

      # probe stuck in infinity loop - exit with a bang!
      if (any(alt_position + moves[alt_direction]  == alt_positions_visited$position & alt_direction == alt_positions_visited$direction))  {
         loop_locs <- c(loop_locs, position)
         break
      }
      
      # exit the probe gracefuly
      if(
         (alt_position %in% which(col(alt_xmatrix) == 1) & alt_direction == "L") |
         (alt_position %in% which(row(alt_xmatrix) == dim(xmatrix)[2]) & alt_direction == "B") |
         (alt_position %in% which(col(alt_xmatrix) == dim(xmatrix)[1]) & alt_direction == "R") |
         (alt_position %in% which(row(alt_xmatrix) == 1) & alt_direction == "T")
      ) break
      }
   
   
   if(# exit conditions 
      (position %in% which(col(xmatrix) == 1) & direction == "L") |
      (position %in% which(row(xmatrix) == dim(xmatrix)[2]) & direction == "B") |
      (position %in% which(col(xmatrix) == dim(xmatrix)[1]) & direction == "R") |
      (position %in% which(row(xmatrix) == 1) & direction == "T")
   ) break
}


print(paste(Sys.time(), "loop locations found", length(unique(loop_locs))))