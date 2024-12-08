input <- readLines("./input.txt")

# input <- c("............",
#            "........0...",
#            ".....0......",
#            ".......0....",
#            "....0.......",
#            "......A.....",
#            "............",
#            "............",
#            "........A...",
#            ".........A..",
#            "............",
#            "............")

# input <- c("T.........",
#            "...T......",
#            ".T........",
#            "..........",
#            "..........",
#            "..........",
#            "..........",
#            "..........",
#            "..........",
#            "..........")

xvector <- unlist(strsplit(split = "", paste0(input, collapse = ""))) 
xmatrix <- matrix(xvector, sqrt(length(xvector)), byrow = T)

# part one

antennas <- data.frame(
   frequency = xmatrix[which(xmatrix != ".")],
   row = which(xmatrix != ".", arr.ind = T)[,"row"],
   col = which(xmatrix != ".", arr.ind = T)[,"col"])

antinodes <- data.frame(frequency = c(),
                        row = c(),
                        col = c())

unique_freqs <- unique(antennas$frequency)

for (freq in unique_freqs) {
   
   freq_ant <- subset(antennas, frequency == freq) 
      
   for (base in seq_along(freq_ant$frequency)) {
      
      # create candidates
      cand <- data.frame(frequency = rep(freq, nrow(freq_ant)))
      cand$col <-  freq_ant$col[base] + (freq_ant$col[base] - freq_ant$col)
      cand$row <- freq_ant$row[base] + (freq_ant$row[base] - freq_ant$row)
      
      # prune candidates: eliminate base antenna
      cand <- subset(cand, !(cand$row == freq_ant$row[base] & cand$col == freq_ant$col[base]))
      
      # prune candidates: off bounds
      cand <- subset(cand, !(cand$row < 1 | cand$col < 1 | cand$col > dim(xmatrix)[1] | cand$row > dim(xmatrix)[1]))
      
      antinodes <- rbind(antinodes, cand)
   
      }

}

print(paste("count of unique antinodes is", lengths(unique(antinodes[, -1 ]))[1]))

# part one

for (freq in unique_freqs) {
   
   freq_ant <- subset(antennas, frequency == freq) 
   
   for (base in seq_along(freq_ant$frequency)) {
      
      for (target in seq_along(freq_ant$frequency)) {
         
         # create candidates
         cand <- data.frame(frequency = rep(freq, dim(xmatrix)[1]))
         cand$col <-  freq_ant$col[base] + seq(from = 0, by = (freq_ant$col[base] - freq_ant$col[target]), length.out = dim(xmatrix)[1])
         cand$row <-  freq_ant$row[base] + seq(from = 0, by = (freq_ant$row[base] - freq_ant$row[target]), length.out = dim(xmatrix)[1])
   
         # prune candidates: eliminate base antenna
         cand <- subset(cand, !(cand$row == freq_ant$row[base] & cand$col == freq_ant$col[base]))
         
         # prune candidates: off bounds
         cand <- subset(cand, !(cand$row < 1 | cand$col < 1 | cand$col > dim(xmatrix)[1] | cand$row > dim(xmatrix)[1]))
         
         antinodes <- rbind(antinodes, cand)
         
      }
      
   }
   
}

# ...including the antinodes that appear on every antenna
antinodes <- rbind(antinodes, antennas)

print(paste("count of resonant antinodes is", lengths(unique(antinodes[, -1 ]))[1]))
