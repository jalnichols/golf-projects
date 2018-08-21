
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(RPostgreSQL)
library(rjson)
library(RMySQL)

con <- dbConnect(MySQL(),host='localhost',dbname='all_rounds',user='jalnichols',password='braves')

# SCRAPE OWGR RANK FROM PLAYER PAGES

all_events <- dbGetQuery(con, "SELECT player_id, event_id 
                         FROM performance_rounds 
                         GROUP BY player_id, event_id") %>%
  
  # grab all players who have played in 2018
  
  filter(event_id > 6873) %>%
  
  group_by(player_id) %>%
  count() %>%
  ungroup()

# and just those playing 10+ events in 2018

all_players <- all_events %>%
  
  filter(n > 9) %>%
  .[,1] %>%
  as.list() %>%
  .[[1]]

# loop to extract all json rank data from OWGR

start <- Sys.time()

player_list <- vector("list", length(all_players))

for(p in 1:length(all_players)) {
  
  player_id <- all_players[[p]]
  
  url <- paste0("http://www.owgr.com/layouts/OWGR/PlayerRankingsForGraph.aspx?playerID=",player_id)
  
  xyz <- readLines(url, warn = FALSE)
  
  abc <- capture.output(cat(gsub("\\\\","",xyz)))
  
  edh <- stringr::str_sub(abc,2,nchar(abc)-1)
  
  if(nchar(edh) < 100) {
    
  }
  
  else {
    
    data <- fromJSON(edh) %>%
      .[[1]]
    
    name <- data[[1]]$name
    
    data <- data[2:length(data)]
    
    rank_list <- vector("list", length(data))
    
    for(w in 1:length(data)) {
      
      week <- data[[w]]$week
      
      rank <- data[[w]]$rank
      
      player_line <- tibble::tibble(
        date = week,
        owgr = rank,
        name = name,
        player_id = player_id
      ) %>% mutate(owgr = as.numeric(owgr), 
                   date = as.Date(date))
      
      rank_list[[w]] <- player_line
      
    }
    
    player_data <- bind_rows(rank_list)
    
    player_list[[p]] <- player_data
    
  }
  
  Sys.sleep(sample(seq(1, 3, by = 0.001), 1))
  
  print(p)
  
}

all_ranks <- bind_rows(player_list)

end <- Sys.time()

#