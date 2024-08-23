# Carregar pacotes
library(tidyverse)

# Definir o número total de jogadas consecutivas que serão avaliadas
max_rolls <- 20

# Calcular a probabilidade de obter 6 consecutivamente com um dado justo
prob_seis_consecutivo <- function(n) {
  (1/6)^n
}

# dataset com as probabilidades para cada número de jogadas
dataset <- tibble::tibble(
  rolagens = 1:max_rolls,
  probabilidade = sapply(1:max_rolls, prob_seis_consecutivo),
  # Calcula a chance do dado ser viciado
  chance_viciado = 1 - probabilidade
)

# Plotar o gráfico
dataset %>% 
ggplot(aes(x = rolagens, y = chance_viciado)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
 
  labs(
    title = "Chance do Dado Ser Viciado com Base em Sorteios de 6 Consecutivos",
    x = "Número de Sorteios Consecutivos de 6",
    y = "Chance do Dado Ser Viciado"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = seq(0, max_rolls, 1)) +
  theme_minimal()

# abordagem bayesiana -----------------------------------------------------



# Parâmetros do modelo
p_viciado <- 0.01  # Probabilidade a priori de que o dado esteja viciado
p_justo <- 1 - p_viciado  # Probabilidade a priori de que o dado seja justo
p_dados_dado_viciado <- 1  # Se o dado é viciado, assume-se que ele sempre dá 6

# Definir o número total de jogadas consecutivas que serão avaliadas
max_rolls <- 20

# Função para calcular a probabilidade posterior de que o dado esteja viciado
posterior_probability <- function(n) {
  p_dados_dado_justo <- (1/6)^n
  p_viciado_dado_dados <- (p_dados_dado_viciado * p_viciado) / 
    ((p_dados_dado_viciado * p_viciado) + (p_dados_dado_justo * p_justo))
  return(p_viciado_dado_dados)
}

# Criar um data frame com as probabilidades para cada número de jogadas
dataset <- tibble(
  rolagens = 1:max_rolls,
  chance_viciado = purrr::map_dbl(1:max_rolls, ~posterior_probability(.x))
)

# Plotar o gráfico
dataset %>% 
ggplot(aes(x = rolagens, y = chance_viciado)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Probabilidade do Dado Estar Viciado - Abordagem Bayesiana",
    x = "Número de Sorteios Consecutivos de 6",
    y = "Chance do Dado Ser Viciado (Bayesiana)"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = seq(0, max_rolls, 1)) +
  theme_minimal()

