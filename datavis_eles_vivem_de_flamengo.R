#Eles vivem de flamengo
library(tidyverse)
library(rendas.brasileirao)

renda_2022 <- rendas.brasileirao::baixa_rendas_brasileirao(1:38, 2022)
renda_2019 <- rendas.brasileirao::baixa_rendas_brasileirao(1:38, 2019)
renda_2018 <- rendas.brasileirao::baixa_rendas_brasileirao(1:38, 2018)

vivem_de_flamengo <- function(df){
  df %>% 
  group_by(clubem) %>% 
    select(clubem, pagante, clubev, data) %>% 
    mutate(media_publico_pagante = mean(pagante, na.rm = TRUE),
           data = lubridate::year(data),
           title_graph = str_glue("Eles Vivem de Flamengo - Média de Público x Público contra Flamengo {data}")) %>% 
           filter(clubev == "Flamengo-RJ") %>% 
    drop_na(pagante)
}
vis_vivem_de_fla <- function(df){
  titulo <- df %>% 
            ungroup %>% 
            select(title_graph) %>% 
            slice(1)
  
  df %>% 
    ggplot(aes(y = clubem))+
    ggalt::geom_dumbbell(aes(x = media_publico_pagante,
                             xend = pagante),
                         size = 1, 
                         size_x = 4, colour_x  = "black",
                         size_xend = 4, colour_xend  = "red")+
    theme_minimal()+
    labs(x = NULL, y = NULL,
         caption = "@baruqrodrigues",
         title = titulo,
         subtitle = "Em preto média de público, em vermelho público contra o mais Querido."
         
         )+
    theme(plot.caption = element_text(hjust = .1),
          plot.title = element_text(hjust = .5))
}


fun_datavis <- function(df){
  df %>% 
  vivem_de_flamengo() %>% 
    vis_vivem_de_fla()
}

map(list(renda_2018, renda_2019, renda_2022), fun_datavis)

