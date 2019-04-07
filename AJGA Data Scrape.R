
library(tidyverse)
library(rvest)

# Extract Tournament
# 2003

x2003 <- 'https://www.ajga.org/tournresults/2003res.asp' %>%
  read_html() %>%
  html_nodes('a') %>%
  html_attr(name = 'href')

# 2004-2011

list_tourns <- vector("list", 10)

for(y in 1:8) {

x0411 <- paste0("https://www.ajga.org/tournresults/", y + 2003, "res.asp") %>%
  read_html() %>%
  html_nodes('table') %>%
  html_nodes('td') %>%
  html_nodes('a') %>%
  html_attr(name = "href") %>% 
  as_tibble() %>%
  mutate(trail = str_sub(value, nchar(value), nchar(value)), 
         lead = str_sub(value, 1, nchar(value) - 1)) %>%
  group_by(lead) %>% 
  mutate(final_round = max(as.numeric(trail))) %>%
  ungroup() %>%
  
  mutate(lead = str_replace(value, "TN=", "xxx"),
         lead = str_replace(lead, "TNo=", "xxx")) %>%
  
  separate(lead, c("dump", "keep"), sep = "xxx") %>%
  
  separate(keep, c("tourn_id", "keep"), sep = "&R") %>%
  
  filter(!((str_sub(tourn_id, 1, 1) == "T") & y > 6)) %>%

  mutate(year = y + 2003) %>%

  select(tourn_id, year) %>%
  
  unique() %>%
  
  filter(!is.na(tourn_id))

#

d0411 <- paste0("https://www.ajga.org/tournresults/", y + 2003, "res.asp") %>%
  read_html() %>%
  html_nodes('table') %>%
  html_nodes('td') %>%
  html_nodes('strong') %>%
  html_text() %>%
  as_tibble() %>% 
  mutate(dummy = "x") %>%
  mutate(rk = rank(dummy, ties.method = "first")) %>% 
  select(-dummy) %>%
  mutate(ty = ifelse((rk %% 2) == 0, "tourn_name", "date"),
         rk = 1 + floor((rk - 1) / 2)) %>%
  spread(ty, value) %>%
  filter(!(str_detect(tourn_name, "Canon Cup") & y + 2003 == 2004)) %>%
  filter(!(str_detect(tourn_name, "Canon Cup") & y + 2003 == 2005)) %>%
  filter(!(str_detect(tourn_name, "Canon Cup") & (y + 2003) < 2010)) %>%
  filter(!(str_detect(tourn_name, "PING Junior Solheim Cup") & y + 2003 == 2005)) %>%
  filter(!(str_detect(tourn_name, "PING Junior Solheim Cup") & y + 2003 == 2007)) %>%
  cbind(x0411)

#

v0411 <- paste0("https://www.ajga.org/tournresults/", y + 2003, "res.asp") %>%
  read_html() %>%
  html_nodes('table') %>%
  html_nodes('td') %>%
  html_nodes('a') %>%
  html_attr(name = "href") %>% 
  as_tibble() %>%
  mutate(trail = str_sub(value, nchar(value), nchar(value)), 
         lead = str_sub(value, 1, nchar(value) - 1)) %>%
  group_by(lead) %>% 
  mutate(final_round = max(as.numeric(trail))) %>%
  ungroup() %>%
  
  mutate(lead = str_replace(value, "TN=", "xxx")) %>%
  
  separate(lead, c("dump", "keep"), sep = "xxx") %>%
  
  separate(keep, c("tourn_id", "keep"), sep = "&R") %>%
  
  mutate(year = y + 2003) %>%
  
  filter(str_detect(value, "/Results.asp") | (str_detect(value, "nresults.asp") & y > 6)) %>%
  
  select(rounds = final_round, tourn_id, year) %>%
  
  unique()

#

all <- d0411 %>%
  
  inner_join(v0411) %>%
  select(-rk)

list_tourns[[y]] <- all

}

#

tourns0411 <- bind_rows(list_tourns)

#

list_leaderboards <- vector("list", 700)

for(t in 1:length(tourns0411$tourn_id)) {
  
  u <- paste0('https://www.ajga.org/TournResults/', tourns0411$year[[t]], '/Results.asp?TN=', tourns0411$tourn_id[[t]], '&RD=', tourns0411$rounds[[t]])
  
  valid <- RCurl::url.exists(u)
  
  if(valid == TRUE) {
  
  l <- u %>%
    read_html() %>%
    html_nodes('td') %>%
    html_nodes('a') %>%
    html_attr(name = "onclick") %>%
    as_tibble() %>%
    mutate(valid = TRUE,
           event_id = tourns0411$tourn_id[[t]],
           year = tourns0411$year[[t]])
  
  } else {
    
    l <- tibble::tibble(value = NA,
                        valid = FALSE,
                        year = tourns0411$year[[t]],
                        tourns0411$tourn_id[[t]]
                        )
    
  }
  
  list_leaderboards[[t]] <- l
  
}

x2004 <- bind_rows(list_leaderboards) %>%
  
  mutate(value = str_replace(value, "&UID=", "xxx")) %>%
  mutate(value = str_replace(value, "&MID=", "xxx")) %>%
  mutate(value = str_replace(value, "&CNo=", "xxx")) %>%
  
  separate(value, c("drop", "player_id", "drop2"), sep = "xxx")



#
#
#
#
#
#
#

# 2012-
  
  x <- 'https://www.ajga.org/TournResults/archived_schedule.asp?year=2012' %>%
  read_html() %>%
  html_nodes('table') %>%
  html_nodes('td') %>%
  html_nodes('a') %>%
  html_attr(name = "href") %>% 
  as.tibble() %>%
  mutate(trail = str_sub(value, nchar(value), nchar(value)), 
         lead = str_sub(value, 1, nchar(value) - 1)) %>%
  group_by(lead) %>% 
  mutate(final_round = max(as.numeric(trail))) %>%
  ungroup()

# Extract the largest trailing number in "RN=X"

