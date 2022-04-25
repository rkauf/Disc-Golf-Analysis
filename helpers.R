library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)
library(ggrepel)

get_pdga_rounds <- function(pdga_number, start_date="1990-01-01") {
  url <- paste0("https://www.pdga.com/player/", pdga_number, "/details")
  
  profile_html <- read_html(url)
  
  rating <- profile_html %>%
    html_nodes(".round-rating") %>%
    html_text() %>% 
    as_tibble() %>% 
    filter(value != 'Rating') %>% 
    rename(rating = value)
  
  eval <- profile_html %>%
    html_nodes(".evaluated") %>%
    html_text() %>% 
    as_tibble() %>% 
    filter(value %in% c("Yes", "No")) %>% 
    rename(evaluated = value)
  
  incl <- profile_html %>%
    html_nodes(".included") %>%
    html_text() %>% 
    as_tibble() %>% 
    filter(value %in% c("Yes", "No")) %>% 
    rename(included = value)
  
  tourney <- profile_html %>%
    html_nodes(".tournament") %>%
    html_text() %>% 
    as_tibble() %>% 
    filter(value != 'Tournament') %>% 
    rename(tournament_name = value)
  
  date_raw <- profile_html %>%
    html_nodes(".date") %>%
    html_text() %>% 
    as_tibble() %>% 
    rename(date_raw = value)
  
  tier <- profile_html %>%
    html_nodes(".tier") %>%
    html_text() %>% 
    as_tibble() %>% 
    rename(tier = value) %>% 
    filter(tier != 'Tier')
  
  div <- profile_html %>%
    html_nodes(".division") %>%
    html_text() %>% 
    as_tibble() %>% 
    rename(division = value) %>% 
    filter(division != 'Division')
  
  ratings_cln <- rating %>% 
    bind_cols(eval, incl, tourney, date_raw, tier, div) %>% 
    mutate(rating = as.numeric(rating),
           year = as.numeric(str_sub(date_raw, -4,-1)),
           month = as.integer(factor(str_sub(date_raw, -8, -6), levels = month.abb)),
           day = as.numeric(str_sub(date_raw, -11,-10)),
           date = ymd(paste(year,month,day, sep="-"))
    ) %>% 
    filter(date > start_date)
  
  crt_rating <- profile_html %>%
    html_nodes(".current-rating") %>%
    html_text()
  
  h1 <- profile_html %>%
    html_nodes("h1") %>%
    html_text()
  
  min_label <- floor(min(ratings_cln$rating)/10)*10
  max_label <- ceiling(max(ratings_cln$rating)/10)*10
  
  
  ratings_graph <- ratings_cln %>% 
    ggplot(aes(date, rating)) +
    geom_point(aes(color = included), size = 3, alpha = 0.7) +
    geom_text_repel(data=ratings_cln %>% filter(rating == max(rating)), aes(date, rating, label=rating)) +
    labs(color='Incl in Current Rating') +
    geom_smooth(formula = y ~ x, method = "loess", se=FALSE) +
    theme_minimal() +
    scale_color_manual(breaks = c("Yes", "No"),
                       values=c("#00BA38", "#F8766D")) +
    scale_y_continuous(breaks = seq(860, 1000, by = 10),
                       labels = seq(860, 1000, by = 10)) +
    ggtitle(h1[1], str_trim(crt_rating)) 
  
  print(ratings_graph)
  
  return(ratings_cln)
}


get_pdga_rounds(128870)
get_pdga_rounds(164015)
get_pdga_rounds(151059)

