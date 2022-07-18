# Modelo de previsão dos jogos do brasileirão
library(tidyverse)

# get the data ------------------------------------------------------------

serie_a <- worldfootballR::get_match_results("BRA",
                                        gender = "M",
                                        season_end_year = 2014:2022) %>% 
  janitor::clean_names() %>% 
  tibble()


glimpse(serie_a)

# tidy --------------------------------------------------------------------

serie_a <- serie_a %>% 
  mutate(
    home_pont = case_when(
    home_goals == away_goals ~ 1,
    home_goals > away_goals ~ 3,
    home_goals < away_goals ~ 0),
    
    away_pont = case_when(
      home_goals == away_goals ~ 1,
      home_goals > away_goals ~ 0,
      home_goals < away_goals ~ 3)) %>%
  
  group_by(home, season_end_year) %>% 
  arrange(date) %>% 
  mutate(wk = as.numeric(wk),
         pont_game_home = cumsum(home_pont),
         pont_game_away = cumsum(away_pont),
         pont_disp = 3*wk,
         pont_earn_home = pont_disp/pont_game_home,
         pont_earn_away = pont_disp/pont_game_home,
         sit_home = recode(home_pont, 
                           `1`="draw",
                           `3`="win",
                           `0`="lose"),
         sit_away = recode(away_pont, 
                           `1`="draw",
                           `3`="win",
                           `0`="lose")) %>% 
  #ungroup() %>% 
  mutate_all(function(x) ifelse(is.infinite(x), 0, x))
  
 

# modeling ----------------------------------------------------------------

dataset_treino <- serie_a %>%
  filter(season_end_year %in% 2014:2021)

model <- nnet::multinom(data = dataset_treino,
                        sit_home~pont_game_home+pont_game_away+pont_earn_home+pont_earn_away)

dataset_alvo <- serie_a %>% filter(season_end_year == 2022)

table(predict(modelo, dataset_alvo))

dataset_alvo$previsao <- predict(modelo, dataset_alvo)
dataset_alvo <- dataset_alvo %>% 
  mutate(#previsao = predict(modelo, dataset_alvo),
         acuracia_previsao = case_when(sit_home == previsao ~"acerto",
                                       TRUE ~"erro"))

dataset_alvo %>% 
  filter(!is.na(home_goals)) %>% 
  pull(acuracia_previsao) %>% 
  table()

dataset_alvo %>% 
  group_by(wk) %>% 
  drop_na(home_goals) %>% 
  count(acuracia_previsao) %>% 
  View()

dataset_alvo %>% 
  group_by(wk) %>% 
  drop_na(home_goals) %>% 
  count(acuracia_previsao) %>% 
  ggplot(aes(x = wk,
             y = n,
             color = acuracia_previsao))+
  geom_line(size = .5)+
  scale_color_manual(values=c( "blue", "red"),
                     labels= c("Acerto","Erro"))+
  scale_y_discrete(limits=seq(2,8,1))+
  scale_x_discrete(limits=seq(1,38,1))+
  labs(x = "Rodada",
       y = "Previsão do Modelo",
       title = "",
       caption = "@baruqrodrigues",
       color = "Acurácia da Previsão")+
  theme_minimal()+
  theme(legend.position = "top",
        plot.title = element_text(hjust =.5),
        plot.caption = element_text(hjust = .1),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color ="black", size = 1)
  )
  