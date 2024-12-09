input <- readLines("./input.txt")

#input <- "2333133121414131402"

#input <- "12345"

# common preprocess

disk_map <- as.numeric(unlist(strsplit(split = "", paste0(input, collapse = ""))))

last_file <- length(disk_map) %/% 2

# init
blocks <- c()
file <- 0
pos <- 1

repeat {

   # add file
   blocks <- c(blocks, rep(file, disk_map[pos]))

   # encode -1 as "space" / since dot would not be numeric, and we want numerics!
   blocks <- c(blocks, rep(-1, disk_map[pos + 1]))
   
   file <- file + 1
   pos <- pos + 2
   # check exit condition
   if(file == last_file) break
}

# add last file
blocks <- c(blocks, rep(file, disk_map[pos]))

# first part

first <- blocks


repeat {
   
   spaces <- which(first == -1)
   files <- which(first != -1)
   
   # are all spaces sorted?
   if(max(files) <= which(first == -1)[1]) break
      
   first[min(spaces)] <- first[max(files)]
   first[max(files)] <- -1
   
}


# discard spaces
first[first == -1] <- 0

checksum <- first %*% (seq_along(first)-1)

print(paste("file checksum is", checksum))


# second part

second <- blocks

for (file in rev(unique(setdiff(second, c(-1))))) {

   system <- data.frame(files = rle(second)["values"][[1]],
                        sizes = rle(second)["lengths"][[1]])
   
   system$start <- cumsum(system$sizes) - system$sizes +1
   
   file_pos <- min(which(second == file))
   file_size <- system$sizes[system$files == file]

   # start of first space big enough to fit file
   nice_space <- min(system$start[system$files == -1 & system$sizes >= file_size])
   
   if (nice_space < file_pos) {
      
      # let's move!
      second[seq(from = nice_space, length.out = file_size)] <- file
      second[seq(from = file_pos, length.out = file_size)] <- -1
      
   }

}

# discard spaces
second[second == -1] <- 0

checksum <- second %*% (seq_along(second)-1)

print(paste("whole file checksum is", checksum))
