library(tidyverse)
url <- "https://github.com/FABdadosabertos/GABAER/blob/master/VOO_DE_AUTORIDADES_ABR19-DEZ20.csv"

voos_autoridades <- url %>% rvest::read_html() %>% 
  rvest::html_table() %>% 
  pluck(1) %>% 
  select(-X1) %>% 
  separate(X2,into = c("autoridades", "origem",
                       "hora_decolagem", "destino",
                       "hora_pouso", "motivo",
                       "passageiros"), sep = ";") %>% 
  separate(hora_decolagem, into = c("data_decolagem", "hora_decolagem"), sep = " - ") %>% 
  separate(hora_pouso, into = c("data_pouso", "hora_pouso"), sep = "( - )| ") %>% 
  mutate(data_decolagem = lubridate::dmy(data_decolagem),
         data_pouso = lubridate::dmy(data_pouso),
         passageiros = as.numeric(passageiros)) %>% 
  mutate(origem_rec = str_remove_all(origem, "\\([^()]+\\)") %>% str_trim(),
         destino_rec = str_remove_all(destino, "\\([^()]+\\)") %>% str_trim())

#pegando info dos municipios
dados_mun <- geobr::read_municipal_seat() %>% 
  select(name_muni, geom)


#Capturando as coordenadas de origem
voos_autoridades <- voos_autoridades %>%
  left_join(dados_mun, by = c("origem_rec" = "name_muni")) %>% 
  mutate(coord_origem = geom) %>% 
  separate(geom, into = c("origem_lat", "origem_lon"), sep = ", ") %>%
  mutate(origem_lat = str_remove_all(origem_lat, "c\\(") %>% as.numeric(),
         origem_lon = str_remove_all(origem_lon, "\\)") %>% as.numeric()) 

#Capturando as coordenadas de destino
voos_autoridades <- voos_autoridades %>%
  left_join(dados_mun, by = c("destino_rec" = "name_muni")) %>%
  mutate(coord_destino = geom) %>%
  separate(geom, into = c("destino_lat", "destino_lon"), sep = ", ") %>%
  mutate(destino_lat = str_remove_all(destino_lat, "c\\(") %>% as.numeric(),
         destino_lon = str_remove_all(destino_lon, "\\)") %>% as.numeric()) 


dados_est<- geobr::read_country()

#Autoridades que mais viajam em vôos da fab 
ggplot() +
  geom_sf(data = dados_est, fill = "black", color = "white") +
  geom_point(data = voos_autoridades, aes(x = origem_lat,
                                          y = origem_lon),
             color = "white", alpha = .1) +
  ggdark::dark_theme_void()+
  scale_colour_distiller()+
  theme(#panel.background = element_rect(fill ="#3B3B3B" )
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(hjust = .1))+
  labs(title = "Vôos de Autoridades em Aviões da FAB - 2020 a 2021",
       caption = "@baruqrodrigues")+
  geom_curve(data = voos_autoridades,
               aes(
                 x = origem_lat,
                 y = origem_lon,
                 xend = destino_lat,
                 yend = destino_lon
               ), alpha = .1,
             curvature = -0.2,
             arrow = arrow(length = unit(0.01, "npc"))
             ) 
  
  

# Número de Viagens  em vôos da FAB - 2020 e 2021
voos_autoridades %>% 
  count(autoridades) %>% 
  ggplot(aes(y=reorder(autoridades, n), x = n))+
  geom_col(fill = "white", color = "black")+
  hrbrthemes::theme_ipsum() +
  theme(panel.grid.major.y = element_blank())+
  labs(x = NULL, y = NULL, 
       title = "Autoridades que mais Viajam em vôos da FAB - 2020 e 2021",
       caption = "@baruqrodrigues")
  

  # Origens de Vôos mais Frequentes
voos_autoridades %>% 
  mutate(origem_rec = fct_lump(origem_rec, 35) %>% fct_infreq()
         ) %>% 
  count(origem_rec) %>% 
  ggplot(aes(y= reorder(origem_rec, n),
             x = n))+
  geom_col(fill = "white", color = "black")+
  hrbrthemes::theme_ipsum()+
  labs(x= NULL,
       y=NULL,
       title = "Origens de Vôos mais frequentes em Vôos da FAB - 2020 a 2021",
       caption = "@baruqrodrigues")

# Destinos de Vôos mais Frequentes
voos_autoridades %>% 
  mutate(destino_rec = fct_lump(destino_rec, 40) %>% fct_infreq()
  ) %>% 
  count(destino_rec) %>% 
  ggplot(aes(y= reorder(destino_rec, n),
             x = n))+
  geom_col(fill = "white", color = "black")+
  hrbrthemes::theme_ipsum()+
  labs(x= NULL,
       y=NULL,
       title = "Destinos de Vôos mais frequentes em Vôos da FAB - 2020 a 2021",
       caption = "@baruqrodrigues")

# Motivos de Voos mais frequentes
voos_autoridades %>%
  mutate(motivo = str_replace(motivo, "\\/ ", "\\/")) %>% 
  count(motivo) %>% 
  ggplot(aes(y = reorder(motivo, n),
             x = n))+
  geom_col(fill = "white", color = "black")+
  hrbrthemes::theme_ipsum()+
  labs(x= NULL,
       y=NULL,
       title = "Motivos de Vôos mais frequentes em Vôos da FAB - 2020 a 2021",
       caption = "@baruqrodrigues")

voos_autoridades %>% 
  ggplot(aes(x = passageiros))+
  geom_rect( fill = "grey", alpha = .5,
             aes(xmin = (mean(passageiros) - 1*sd(passageiros)),
                 xmax =(mean(passageiros) +  1*sd(passageiros)),
                 ymin = 0, ymax = Inf))+
  geom_rect( fill = "grey", alpha = .02,
             aes(xmin = (mean(passageiros) - 2*sd(passageiros)),
                 xmax =(mean(passageiros) +  2*sd(passageiros)),
                 ymin = 0, ymax = Inf))+
  geom_histogram(fill = "white",
                 color = "black")+
  geom_vline(xintercept = mean(voos_autoridades$passageiros),
             color = "black",
             linetype = "dashed")+
  geom_vline(xintercept = median(voos_autoridades$passageiros),
             color = "red",
             linetype = "dashed")+
  hrbrthemes::theme_ipsum()+
  labs(x=NULL,
       y=NULL,
       title = "Distribuição do Número de Passageiros em Vôos da FAB - 2020 a 2021",
       caption = "Linha Tracejada Preta = Média  
       Linha Tracejada Vermelha = Mediana
       Areas em Cinza = Desvios Padrão
       @baruqrodrigues")
  


