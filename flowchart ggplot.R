#feito com base em https://nrennie.rbind.io/blog/2022-06-06-creating-flowcharts-with-ggplot2/
#fluxograma base em https://i.imgur.com/q9Xjv4p.jpeg

# packages ----------------------------------------------------------------
library(tidyverse)
library(igraph)
library(showtext)
library(rcartocolor)
#ENCODING WINDOWS-1252


# get the data ------------------------------------------------------------

tibble::tribble(
                            ~inicio,                          ~direcao,
                           "Inicio",           "Qual sua preferência?",
            "Qual sua preferência?",             "Tipo o anime Avatar",
            "Qual sua preferência?",            "Um faroeste espacial",
            "Qual sua preferência?",      "Jogos Mentais Sobrenatural",
            "Qual sua preferência?",                  "Algo Diferente",
              "Tipo o anime Avatar", "FullMetal Alchemist Brotherhood",
             "Um faroeste espacial",                    "Cowboy Bebop",
       "Jogos Mentais Sobrenatural",                      "Death note",
                   "Algo Diferente",      "Precisa de Uma boa Risada?",
       "Precisa de Uma boa Risada?",                  "Comédia e Ação",
       "Precisa de Uma boa Risada?",           "Comédia Romance Drama",
       "Precisa de Uma boa Risada?",               "Comédia +Trabalho",
       "Precisa de Uma boa Risada?",              "Ação com Aventura?",
                   "Comédia e Ação",                    "Angel Beats!",
            "Comédia Romance Drama",                       "Toradora!",
                "Comédia +Trabalho",       "The Devil is a Part-Timer",
               "Ação com Aventura?",              "Algo dark e serio?",
               "Algo dark e serio?", "Policial Psicologico Ciberpunk?",
               "Algo dark e serio?",              "Superpoderes X-MEN",
               "Algo dark e serio?",          "Terror Viagem no Tempo",
               "Algo dark e serio?",                     "Não Exagere",
  "Policial Psicologico Ciberpunk?",                     "Psycho-Pass",
               "Superpoderes X-MEN",               "Darker than Black",
           "Terror Viagem no Tempo",                     "steins gate",
                      "Não Exagere",               "Algo bem japonês?",
                "Algo bem japonês?",               "Samurais e HipHop",
                "Algo bem japonês?",                 "Robos Gigantes?",
                "Algo bem japonês?",           "Tipo Filmes da Ghibli",
                "Algo bem japonês?",             "algo mais ocidental",
                "Samurais e HipHop",                "Samurai Champloo",
                  "Robos Gigantes?",      "TENGEN TOPPA GURREN LAGANN",
            "Tipo Filmes da Ghibli",                        "Mushishi",
              "algo mais ocidental",          "Fantasia Ação Aventura",
           "Fantasia Ação Aventura",          "Steampunk Música Épica",
           "Fantasia Ação Aventura",    "Tipo Avatar Deserto Aventura",
           "Fantasia Ação Aventura",        "Piratas do Caribe Diablo",
           "Fantasia Ação Aventura",                 "MAIS ME DÊ MAIS",
           "Steampunk Música Épica",                 "Attack on Titan",
     "Tipo Avatar Deserto Aventura",    "Magi: the Labyrinth of Magic",
         "Piratas do Caribe Diablo",     "Shingeki no Bahamut Genesis",
                  "MAIS ME DÊ MAIS",           "só na versão completa"
  )


# Preparando os dados para a datavis ----------------------------------------------------------


g <- graph_from_data_frame(fluxograma, directed = TRUE)

coordenadas <- layout_as_tree(g)

colnames(coordenadas) <- c("x","y")

dataset_produto <- as_tibble(coordenadas) %>% 
  mutate(passo = vertex_attr(g, "name"),
         label = gsub("\\d+$", "", passo),
         x = x*-1)

plot_nodes <- dataset_produto %>% 
  mutate(xmin = x-.49,
         xmax = x+.49,
         ymin = y-.3,
         ymax = y+.3)

plot_edges <- fluxograma %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(cols = c("inicio", "direcao"),
               names_to = "s_e",
               values_to = "passo") %>% 
  left_join(plot_nodes, by = "passo") %>% 
  select( -c(label, y, xmin, xmax)) %>% 
  mutate(y = ifelse(s_e == "inicio", ymin, ymax)) %>%
  select(-c(ymin, ymax))

font_add_google(name = "Henny Penny", family = "henny")
showtext_auto()



# Datavis -----------------------------------------------------------------


ggplot() +
  geom_rect(
    data = plot_nodes,
    mapping = aes(
      xmin = xmin,
      ymin = ymin,
      xmax = xmax,
      ymax = ymax,
      fill = "#f2e4c1"
    ),
    alpha = 0.5
  ) +
 geom_text(
    data = plot_nodes,
    mapping = aes(x = x, y = y, label = label),
    family = "henny",
    color = "#585c45",
    size = 3.5) +
  geom_path(
    data = plot_edges,
    mapping = aes(x = x, y = y, group = id),
    colour = "#585c45",
    arrow = arrow(length = unit(0.3, "cm"), type = "closed")
  )+
  scale_fill_carto_d(palette = "Antique") +
  scale_colour_carto_d(palette = "Antique")+
  theme_void()+
  labs(title = "Fluxograma Weebo pra Escolher um Anime",
       caption = "@baruqrodrigues")+
  theme(plot.title = element_text(hjust = .2,
                                  vjust = -10,
                                  size = 20,
                                  family = "henny",
                                  color = "#585c45"),
        plot.caption = element_text(hjust = .1,
                                    vjust = 4,
                                    size =12,
                                    family = "henny",
                                    color = "#585c45"),
        legend.position = "none"
  )

,
plot.background = element_rect(colour = "#f2e4c1",
                               fill = "#f2e4c1"),
panel.background = element_rect(colour = "#f2e4c1",
                                fill = "#f2e4c1")
  
