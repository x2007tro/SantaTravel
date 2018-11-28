##
# Helper functions
##

# find shortest path
ModDijkstra_SampleData <- function(all_cities, dists, from, to){
  
  # preparation
  visited <- c()
  unvisited <- all_cities
  track_record <- data.frame(
    CityId = unvisited,
    shortest_dist = 1000000,
    prev_CityId = -1,
    stringsAsFactors = FALSE
  )
  track_record[track_record$CityId == 1, "shortest_dist"] <- 0
  
  # start the algo
  i <- 1
  while(length(unvisited) > 0){
    # determine mini distance
    min_dist <- min(track_record[track_record$CityId %in% unvisited, "shortest_dist"])
    
    # determine the next visit CityId
    tmp_id <- track_record[track_record$shortest_dist == min_dist, "CityId"]
    
    # add CityId to the visited list
    visited <- c(visited, tmp_id)
    
    # remove CityId from the unvisited list
    unvisited <- unvisited[!(unvisited %in% tmp_id)]
    
    # calculate distance of the tmp_id
    tmp_dist <- dists[[tmp_id]]
    tmp_dist <- tmp_dist %>% 
      dplyr::mutate(dist = dist + min_dist)
    
    # determine the shortest distance for each neigbhour
    tmp_comp <- lapply(1:nrow(tmp_dist), function(i, prev_id, old_dist, new_dist){
      cid <- new_dist[i, "CityId"]
      shortest_dist <- old_dist[old_dist$CityId == cid, "shortest_dist"]
      new_dist <- new_dist[new_dist$CityId == cid, "dist"]
      
      if(new_dist < shortest_dist){
        res <- data.frame(
          CityId = cid,
          shortest_dist = new_dist,
          prev_CityId = prev_id,
          stringsAsFactors = FALSE
        )
      } else {
        res <- NULL
      }
      return(res)
    }, tmp_id, track_record, tmp_dist)
    tmp_comp <- dplyr::bind_rows(tmp_comp)
    
    # update track record with new distance
    if(nrow(tmp_comp) > 0){
      track_record[track_record$CityId %in% tmp_comp$CityId, c("shortest_dist", "prev_CityId")] <-
        tmp_comp[,c("shortest_dist", "prev_CityId")]
    }
    
    i <- i + 1
  }
  
  return(track_record)
}

# Find distance
CalcAllDistance <- function(dataset){
  
  res <- lapply(1:nrow(dataset), function(i){
    print(i)
    pos_end <- dataset[i:nrow(dataset),]
    
    cid <- dataset$CityId[i]
    pos_beg <- dataset %>%
      dplyr::slice(i) %>% 
      dplyr::mutate(X0 = X, Y0 = Y) %>% 
      dplyr::select(X0, Y0) %>% 
      dplyr::slice(rep(1, each = nrow(pos_end)))
    
    pos_final <- cbind.data.frame(pos_end, pos_beg)
    pos_final$dist <- sqrt((pos_final$Y - pos_final$Y0)^2 + (pos_final$X - pos_final$X0)^2)
    res <- pos_final[,c("CityId", "dist"), drop = FALSE]
    
    return(res)
  })
  names(res) <- dataset$CityId
  
  return(res)
}

# find distance
CalcDistance <- function(dataset, ori, tgt){
  
  pos_end <- dataset %>% 
    dplyr::filter(CityId %in% tgt)
  
  pos_beg <- dataset %>%
    dplyr::filter(CityId == ori) %>% 
    dplyr::mutate(X0 = X, Y0 = Y) %>% 
    dplyr::select(X0, Y0) %>% 
    dplyr::slice(rep(1, each = nrow(pos_end)))
  
  pos_final <- cbind.data.frame(pos_end, pos_beg)
  pos_final$dist <- sqrt((pos_final$Y - pos_final$Y0)^2 + (pos_final$X - pos_final$X0)^2)
  pos_final$is_prime <- primes::is_prime(pos_final$CityId)
  res <- pos_final[,c("CityId", "dist", "is_prime"), drop = FALSE]
  
  return(res)
}

# find shortest path algo 1
ModDijkstra <- function(dataset, from, to){
  
  # preparation
  visited <- c()
  unvisited <- dataset$CityId
  track_record <- data.frame(
    CityId = unvisited,
    shortest_dist = 1000000,
    prev_CityId = -1,
    stringsAsFactors = FALSE
  )
  track_record[track_record$CityId == 0, "shortest_dist"] <- 0
  
  # start the algo
  i <- 1
  while(length(unvisited) > 1){
    # determine mini distance
    min_dist <- min(track_record[track_record$CityId %in% unvisited, "shortest_dist"])
    
    # determine the next visit CityId
    tmp_id <- track_record[track_record$shortest_dist == min_dist, "CityId"]
    
    # add CityId to the visited list
    visited <- c(visited, tmp_id)
    print(visited)
    
    # remove CityId from the unvisited list
    unvisited <- unvisited[!(unvisited %in% tmp_id)]
    
    # calculate distance of the tmp_id
    tmp_dist <- CalcDistance(dataset, tmp_id, unvisited)
    tmp_dist <- tmp_dist %>% 
      dplyr::mutate(dist = dist + min_dist)
    
    # determine the shortest distance for each neigbhour
    tmp_comp <- lapply(1:nrow(tmp_dist), function(i, prev_id, old_dist, new_dist){
      cid <- new_dist[i, "CityId"]
      shortest_dist <- old_dist[old_dist$CityId == cid, "shortest_dist"]
      new_dist <- new_dist[new_dist$CityId == cid, "dist"]
      
      if(new_dist < shortest_dist){
        res <- data.frame(
          CityId = cid,
          shortest_dist = new_dist,
          prev_CityId = prev_id,
          stringsAsFactors = FALSE
        )
      } else {
        res <- NULL
      }
      return(res)
    }, tmp_id, track_record, tmp_dist)
    tmp_comp <- dplyr::bind_rows(tmp_comp)
    
    # update track record with new distance
    if(nrow(tmp_comp) > 0){
      track_record[track_record$CityId %in% tmp_comp$CityId, c("shortest_dist", "prev_CityId")] <-
        tmp_comp[,c("shortest_dist", "prev_CityId")]
    }
      
    i <- i + 1
  }
  
  return(track_record)
}

# find shortest path algo 2
Algo1_MinDistPerStep <- function(dataset, from, to){
  
  # preparation
  visited <- c()
  unvisited <- dataset$CityId
  track_record <- data.frame(
    CityId = unvisited,
    shortest_dist = 1000000,
    visit_step = 0,
    is_prime = primes::is_prime(unvisited),
    stringsAsFactors = FALSE
  )
  track_record[track_record$CityId == 0, "shortest_dist"] <- 0
  tmp_cid <- 0

  # start the algo
  i <- 1
  while(length(unvisited) > 1){
    print(i)
    
    # add CityId to the visited list
    visited <- c(visited, tmp_cid)
    
    # remove CityId from the unvisited list
    unvisited <- unvisited[!(unvisited %in% tmp_cid)]
    
    # calculate distance of the tmp_cid
    tmp_dist <- CalcDistance(dataset, tmp_cid, unvisited)
    if(i %% 10 == 0){
      if(primes::is_prime(tmp_cid)){
        tmp_dist$dist <- tmp_dist$dist * 1.1
      }
    }
    
    # find shortest
    if((i+1) %% 10 == 0){
      # only go to prime in this case
      tmp_dist1 <- tmp_dist %>% 
        dplyr::filter(is_prime == TRUE)
      
      if(nrow(tmp_dist) != 0){
        tmp_dist <- tmp_dist1
      } 
    }
    shortest_dist <- tmp_dist[tmp_dist$dist == min(tmp_dist$dist), "dist"][1]
    tmp_cid <- tmp_dist[tmp_dist$dist == min(tmp_dist$dist), "CityId"][1]
    
    # update track record
    track_record[track_record$CityId == tmp_cid, "visit_step"] <- i
    track_record[track_record$CityId == tmp_cid, "shortest_dist"] <- shortest_dist

    # update step
    i <- i + 1
  }
  
  return(track_record)
  
}