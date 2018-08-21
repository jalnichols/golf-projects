
library(dplyr)
library(tidyr)
library(readr)

# bring in prepared CSV showing data in form hole_no, score_to_par, count

scoring_data <- read_csv("pebble-beach-scoring-2010.csv")

# players qualifying, (just one player advancing here) and field size (# in playoffs)

PQ <- 1
FIELD <- 24

# calculates percentages of birdie, par, bogey, etc.

setup_scoring <- scoring_data %>%
  
  group_by(hole_no) %>%
  
  mutate(perc = count / sum(count)) %>%
  
  ungroup() %>%
  
  select(-count) %>%
  
  spread(score_to_par, perc) %>%
  
  gather(score_to_par, perc, -hole_no) %>%
  
  mutate(perc = ifelse(is.na(perc), 0, perc),
         score_to_par = as.numeric(score_to_par)) %>%
  
  spread(score_to_par, perc) %>%
  
  # we'll use a random function later on so define which parts of the 0 to 1 continuum reflects probability of birdie, par, etc.
  
  mutate(eagle = `-2`,
         birdie = `-1` + eagle,
         par = `0` + birdie,
         bogey = `1` + par,
         worse = 1 - bogey) %>%
  
  select(hole_no, eagle:worse)

# we now enter the for loop hacks zone
# create a data frame with a row for each hole_no and competitor (24 players)

competitors <- vector("list", FIELD)

for(p in 1:FIELD) {
  
  d <- setup_scoring %>%
    
    mutate(comp = p)
  
  competitors[[p]] <- d
  
}

competitors <- bind_rows(competitors)

# link consecutive holes including #18 to #1
# with some knowledge of realistic back to back holes you could expand to cover all options (for example #3 to #17 or #16 to #4)

two_holes <- vector("list", 18)

for(h in 1:18) {
  
  d <- competitors %>%
    
    filter((hole_no == h | hole_no == h + 1) | (h == 18 & hole_no %in% c(1, 18)))
  
  #
  
  two_holes[[h]] <- d %>%
    
    mutate(start_hole = h)
  
}

two_holes <- bind_rows(two_holes)

# run the main simulation for loop

tictoc::tic()

#

it <- 1000

holes_data <- vector("list", 18)

#

for(h in 1:18) {
  
  data <- two_holes %>%
    filter(start_hole == h)
  
  sim_data <- vector("list", it)
  
  for(i in 1:it) {
    
    # the logic here is that we're just simulating a single run of the first hole & removing anyone who does not earn the best score
    # we then filter the data for the next hole and continue on
    # this can be for looped as well
    
    first_hole <- data %>%
      
      filter(hole_no == h) %>%
      
      mutate(s = runif(n(), min = 0, max = 1),
             
             s = ifelse(s < eagle, -2,
                        ifelse(s < birdie, -1,
                               ifelse(s < par, 0,
                                      ifelse(s < bogey, 1, 2))))) %>%
      
      mutate(rk = rank(s, ties.method = "min")) %>%
      
      filter(rk == 1) %>%
      
      select(comp) %>%
      
      as.list() %>%
      .[[1]]
    
    #
    
    left_after_1 <- length(first_hole)
    
    #
    
    second_hole <- data %>%
      
      filter(hole_no != h & comp %in% first_hole) %>%
      
      mutate(s = runif(n(), min = 0, max = 1),
             
             s = ifelse(s < eagle, -2,
                        ifelse(s < birdie, -1,
                               ifelse(s < par, 0,
                                      ifelse(s < bogey, 1, 2))))) %>%
      
      mutate(rk = rank(s, ties.method = "min")) %>%
      
      filter(rk == 1) %>%
      
      select(comp) %>%
      
      as.list() %>%
      .[[1]]
    
    #
    
    left_after_2 <- length(second_hole)
    
    #
    
    third_hole <- data %>%
      
      filter(hole_no == h & comp %in% second_hole) %>%
      
      mutate(s = runif(n(), min = 0, max = 1),
             
             s = ifelse(s < eagle, -2,
                        ifelse(s < birdie, -1,
                               ifelse(s < par, 0,
                                      ifelse(s < bogey, 1, 2))))) %>%
      
      mutate(rk = rank(s, ties.method = "min")) %>%
      
      filter(rk == 1) %>%
      
      select(comp) %>%
      
      as.list() %>%
      .[[1]]
    
    #
    
    left_after_3 <- length(third_hole)
    
    #
    
    fourth_hole <- data %>%
      
      filter(hole_no != h & comp %in% third_hole) %>%
      
      mutate(s = runif(n(), min = 0, max = 1),
             
             s = ifelse(s < eagle, -2,
                        ifelse(s < birdie, -1,
                               ifelse(s < par, 0,
                                      ifelse(s < bogey, 1, 2))))) %>%
      
      mutate(rk = rank(s, ties.method = "min")) %>%
      
      filter(rk == 1) %>%
      
      select(comp) %>%
      
      as.list() %>%
      .[[1]]
    
    #
    
    left_after_4 <- length(fourth_hole)
    
    #
    
    fifth_hole <- data %>%
      
      filter(hole_no == h & comp %in% fourth_hole) %>%
      
      mutate(s = runif(n(), min = 0, max = 1),
             
             s = ifelse(s < eagle, -2,
                        ifelse(s < birdie, -1,
                               ifelse(s < par, 0,
                                      ifelse(s < bogey, 1, 2))))) %>%
      
      mutate(rk = rank(s, ties.method = "min")) %>%
      
      filter(rk == 1) %>%
      
      select(comp) %>%
      
      as.list() %>%
      .[[1]]
    
    #
    
    left_after_5 <- length(fifth_hole)
    
    #
    
    sixth_hole <- data %>%
      
      filter(hole_no != h & comp %in% fifth_hole) %>%
      
      mutate(s = runif(n(), min = 0, max = 1),
             
             s = ifelse(s < eagle, -2,
                        ifelse(s < birdie, -1,
                               ifelse(s < par, 0,
                                      ifelse(s < bogey, 1, 2))))) %>%
      
      mutate(rk = rank(s, ties.method = "min")) %>%
      
      filter(rk == 1) %>%
      
      select(comp) %>%
      
      as.list() %>%
      .[[1]]
    
    #
    
    left_after_6 <- length(sixth_hole)
    
    #
    
    results <- tibble::tibble(a1 = left_after_1,
                              a2 = left_after_2,
                              a3 = left_after_3,
                              a4 = left_after_4,
                              a5 = left_after_5,
                              a6 = left_after_6,
                              total = (24 + a1 + a2 + a3 + a4 + a5 + a6),
                              ends_by = ifelse(a1 == PQ, 1,
                                               ifelse(a2 == PQ, 2,
                                                      ifelse(a3 == PQ, 3,
                                                             ifelse(a4 == PQ, 4,
                                                                    ifelse(a5 == PQ, 5,
                                                                           ifelse(a6 == PQ, 6, 7)))))),
                              start_hole = h)
    
    sim_data[[i]] <- results
    
  }
  
  holes_data[[h]] <- bind_rows(sim_data)
  
}

#

results <- bind_rows(holes_data)

tictoc::toc()

# calculate results based on the starting hole

hole_results <- results %>%
  
  group_by(start_hole) %>%
  
  summarize(median_holes = median(total),
         mean_holes = mean(total),
         
         ends_in_1 = mean(ends_by < 2),
         ends_in_2 = mean(ends_by < 3),
         ends_in_3 = mean(ends_by < 4),
         ends_in_4 = mean(ends_by < 5),
         
         fewer_31 = mean(total < 31),
         fewer_41 = mean(total < 41),
         fewer_51 = mean(total < 51)) %>%
  
  ungroup()
