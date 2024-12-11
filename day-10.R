input <- readLines("./input.txt")

# input <- c("89010123",
#            "78121874",
#            "87430965",
#            "96549874",
#            "45678903",
#            "32019012",
#            "01329801",
#            "10456732")



# part one

xvector <- unlist(strsplit(split = "", paste0(input, collapse = ""))) 

xmatrix <- matrix(xvector, sqrt(length(xvector)), byrow = T)

trailmtx <- data.frame(digit = c(),
                       row = c(),
                       col = c())

# build trail matrix as a dataframe
build_tmtx <- function(digit) {
   
   trailmtx <<- rbind(trailmtx,
                     data.frame(digit = as.numeric(digit),
                                row = which(xmatrix == digit, arr.ind = T)[, "row"],
                                col = which(xmatrix == digit, arr.ind = T)[, "col"],
                                id = which(xmatrix == digit)))
   
}

sapply(0:9, build_tmtx)



# find position of next highest value - vectorized (not really)
next_highest <- function(values, row, col) {
   
   res <- data.frame()   

   for (i in seq_along(values)) {

      row_nbrs <- unique(pmax(1, pmin(c(row[i] +1, row[i] -1), dim(trailmtx)[1])))
      col_nbrs <- unique(pmax(1, pmin(c(col[i] +1, col[i] -1), dim(trailmtx)[1])))
      
      
      neighbors <- ((trailmtx$row %in% row_nbrs) & (trailmtx$col == col[i])) |
         ((trailmtx$col %in% col_nbrs) & (trailmtx$row %in% row[i]))
      
      candidate <- neighbors & trailmtx$digit == (values +1)
      
      res <- rbind(res, trailmtx[candidate,])
   }
   
   unique(res)
   
}

trailhead_score <- 0


for (trailhead in 1:sum(trailmtx$digit==0)) {
   
   # start of trail
   trail <- subset(trailmtx, digit == 0)[trailhead,]

   for (i in 1:9) {
      
      trail <- next_highest(trail$digit, trail$row, trail$col)  

   }
   
   trailhead_score <- trailhead_score + nrow(trail)
   
}

print(paste("sum of trailhead scores", trailhead_score))

# part two
