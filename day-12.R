input <- readLines("./input.txt")

input <- c("RRRRIICCFF",
           "RRRRIICCCF",
           "VVRRRCCFFF",
           "VVRCCCJFFF",
           "VVVVCJJCFE",
           "VVIVCCJJEE",
           "VVIIICJJEE",
           "MIIIIIJJEE",
           "MIIISIJEEE",
           "MMMISSJEEE")

# part one

xvector <- unlist(strsplit(split = "", paste0(input, collapse = ""))) 

library(sf)

baseline <- st_polygon(list(matrix(c(0,0,
                                     sqrt(length(xvector)), 0,
                                     sqrt(length(xvector)), sqrt(length(xvector)),
                                     0, sqrt(length(xvector)),
                                     0,0),ncol=2, byrow=TRUE)))

grid <- st_make_grid(baseline, n = sqrt(length(xvector))) %>% 
   st_as_sf()

grid$value <- xvector

library(ggplot2)
library(dplyr)

plots <- group_by(grid, value) %>% 
   summarise() %>% 
   st_geometry() %>% 
   st_cast("MULTIPOLYGON") %>% 
   st_cast("POLYGON") %>% 
   st_as_sf() %>% 
   mutate(area = st_area(.),
          lengths = st_perimeter(.))

ggplot() +
   geom_sf(data = plots, aes(fill = as.factor(1:nrow(plots)))) +
   geom_sf_text(data = grid, aes(label = value)) +
   scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1", nrow(plots), type = "continuous")) +
   theme_void() + theme(legend.position="none")


print(paste("cost of fencing is", plots$area %*% plots$lengths))


# part two


for (i in 1:nrow(plots)) {
   
   plot <- st_simplify(st_geometry(plots[i, ]))
   
   corners <- unique(st_coordinates(plot, "POINT"))
   
   plots$sides[i] <- nrow(corners)
}


print(paste("cost of fencing is", plots$area %*% plots$sides))
