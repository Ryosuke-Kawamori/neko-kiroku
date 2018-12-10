library(tidyverse)
library(lubridate)
library(rvest)

`%!in%` <- function(x,y){return(!x%in%y)}

neko_raw_df <- read_html("https://chouseisan.com/s?h=ae75a68c77df431a996bc0fdbb3496e9") %>%
  html_nodes(".schedule") %>%
  html_table(header = TRUE) %>%
  `[[`(1) 

neko_df <- neko_raw_df %>% 
  as_tibble() %>% 
  gather(key=key, value=value, -日程) %>% 
  filter(日程 %!in% c("天気", "コメント")) %>% 
  mutate(key=ymd(paste("2018/",key))) %>% 
  mutate(value = case_when(value == "○" ~ 1,
                           value == "△" ~ 0,
                           value == "×" ~ -1)) 

neko_df %>% 
  ggplot(aes(key, value, fill=日程))+
  geom_point()+
  geom_line()
