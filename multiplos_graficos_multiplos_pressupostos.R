### Como Rodar Multiplos modelos e Gerar Multiplos gráficos de pressupostos

library(tidyverse)
library(performance)

# Múltiplos modelos para múltiplos grupos
modelos <- mpg %>%
  summarise(
    # agrupando os dados por modelo
    .by = model,
    # criando um modelo de regressão por modelo de carro
    modelo = list(
      lm(hwy-cty~displ, data = .)
    ),
  )

# usamos o map para construir o plot de coeficientes do modelo
datavis = map(modelo, sjPlot::plot_model)
# usamos o map para construir o plot dos pressupostos do modelo
pressupostos = map(modelo, check_model)

# Para exportar os gráficos é muito simples
walk2(
  # indicamos a variável de grupo
  .x = modelos$model,
  # indicamos a variável que as visualizações
  .y = modelos$datavis,
  # Usamos o ggsave para salvar o modelo externamente
  ggsave(
    # indicamos o nome do arquivo que será a variável de grupo
    filename = paste0(.x, ".pdf"),
    plot = .y,
    device = "pdf",
    width = 12,
    height = 9
  )
)