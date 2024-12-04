input <- readLines("./input.txt")

# part one

xvector <- unlist(strsplit(split = "", paste0(input, collapse = ""))) 

xmatrix <- matrix(xvector, sqrt(length(xvector)), byrow = T)


# 4 directions for XMAS or SAMX

rows <- row(xmatrix)
cols <- col(xmatrix)
diag1 <- row(xmatrix) - col(xmatrix) + dim(xmatrix)
diag2 <- row(xmatrix) + col(xmatrix) -1


count_xmas <- 0


# run the directions over xmatrix   
for (i in 1:sum(dim(xmatrix))) {
   
   count_xmas <- count_xmas + sum(stringr::str_count(paste0(xmatrix[rows == i], collapse = ""), c("XMAS", "SAMX")))
   count_xmas <- count_xmas + sum(stringr::str_count(paste0(xmatrix[cols == i], collapse = ""), c("XMAS", "SAMX")))
   count_xmas <- count_xmas + sum(stringr::str_count(paste0(xmatrix[diag1 == i], collapse = ""), c("XMAS", "SAMX")))
   count_xmas <- count_xmas + sum(stringr::str_count(paste0(xmatrix[diag2 == i], collapse = ""), c("XMAS", "SAMX")))

}

print(paste("count of XMAS (or SAMX)", count_xmas))


# part two

xdim <- dim(xmatrix)[1]

count_xmas <- 0

# position of letter A in "inside" matrix (= not on edge)
a_idx <- which(c(xmatrix=="A") & !row(xmatrix) %in% c(1, 140) & !col(xmatrix) %in% c(1, 140))

for (i in a_idx) {
   
   # two candidates (crosses)
   candidate_one <- paste0(xmatrix[i - xdim -1], xmatrix[i], xmatrix[i + xdim +1])
   candidate_two <- paste0(xmatrix[i - xdim +1], xmatrix[i], xmatrix[i + xdim -1])
   
   count_xmas <- count_xmas + all(is.element(c(candidate_one, candidate_two), c("MAS", "SAM")), na.rm = T) 

}

print(paste("count of X-MAS", count_xmas))
