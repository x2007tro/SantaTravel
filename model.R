##
# Model for finding shortest path
##
source("Helper/func.R")
library(magrittr)
options(scipen = 999)

##
# apply dijkstra algorithm
# sample_data <- list(
#   data.frame(
#     CityId = c(2,3),
#     dist = c(7,3)
#   ),
#   data.frame(
#     CityId = c(1,3,4,5),
#     dist = c(7,1,2,6)
#   ),
#   data.frame(
#     CityId = c(1,2,4),
#     dist = c(3,1,2)
#   ),
#   data.frame(
#     CityId = c(3,2,5),
#     dist = c(2,2,4)
#   ),
#   data.frame(
#     CityId = c(2,4),◙
#     dist = c(6,4)
#   )
# )
# shortest_dist_sample <- ModDijkstra_SampleData(1:5, sample_data, 1, 5)

##
# load data
city_coords <- read.csv("Source Data/cities.csv")
city_coords_ss <- city_coords[1:5, ]
shortest_dist <- Algo1_MinDistPerStep(city_coords, 0, nrow(city_coords) - 1)
shortest_dist <- shortest_dist[order(shortest_dist$visit_step),]
write.csv(shortest_dist, file = paste0("Submission/sub-", format(Sys.Date(), "%y%m%d_%H%M%S"), ".csv"))
