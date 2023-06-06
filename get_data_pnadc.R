library(tidyverse)
library(PNADcIBGE)

# Get data ----------------------------------------------------------------


dados <- map_dfr(2019:2023, 
                      ~PNADcIBGE::get_pnadc(.x, quarter = 1,
                                   vars = c(
                                     "UF", 
                                     "V2001", #Número de pessoas no domicilio
                                     "V2005", #Condição do Domicilio
                                     "V2007", #Sexo,
                                     "V2009", #Idade
                                     "V2010", #Cor ou Raça
                                     "V3007", #Curso Superior
                                     "VD3004", #Nível de Instrução mais elevado da familia
                                     "VD4001", #Trabalho
                                     "VD4019", #Remuneração Trabalho
                                     "VD4020" #Rendimento Mensal
                                   )
                                  ) $variables %>%  
                        select(c("UF", 
                                 n_pessoas_dom ="V2001", #Número de pessoas no domicilio
                                 condicao_dom ="V2005", #Condição do Domicilio
                                 sexo ="V2007", #Sexo,
                                 idade ="V2009", #Idade
                                 raca ="V2010", #Cor ou Raça
                                 curso_superior ="V3007", #Curso Superior
                                 nivel_instrucao_mais_alto_fam ="VD3004", #Nível de Instrução mais elevado da familia
                                 trabalho ="VD4001", #Trabalho
                                 rendimento_mensal ="VD4020" #Rendimento Mensal
                        )) %>% 
                   mutate(ano_pnad = .x) %>% 
  tibble()
)

# Datavis -----------------------------------------------------------------


theme_set(hrbrthemes::theme_ipsum_es())

#Alagoas
dados %>%
  filter(UF == "Alagoas") %>% 
  #calcula o centil por Estado e por Ano da PnadC
  mutate(
    centil_renda = ntile(rendimento_mensal, 100) %>% as.factor(),
    .by = c(ano_pnad, UF)
  ) %>%
  #Calcula o Piso da renda para cada UF e para cada ano da pnadc
  mutate(
    piso_renda = min(rendimento_mensal, na.rm = TRUE),
    .by = c(centil_renda, ano_pnad, UF)
  ) %>%
  filter(centil_renda %in% c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99)) %>%
  summarise(
    piso_renda = mean(piso_renda, na.rm = TRUE),
    .by = c(centil_renda, ano_pnad, UF)
  ) %>%
  ggplot(aes(
    x = reorder(centil_renda,-piso_renda),
    y = round(piso_renda, 2)
  )) +
  geom_col(fill = "#1d2951") +
  geom_text(aes(label = piso_renda), vjust = -.05) +
  facet_wrap(~ ano_pnad) +
  labs(
    x = "Centil de Renda",
    y = "Rendimento Mensal",
    title = "Rendimento Mensal para estar entre os % mais Ricos de Alagoas",
    caption = "Fonte: PNADC (2023) \n @baruqrodrigues"
  )

# Pernambuco
  
  dados %>%
    filter(UF == "Pernambuco") %>% 
  #calcula o centil por Estado e por Ano da PnadC
  mutate(
    centil_renda = ntile(rendimento_mensal, 100) %>% as.factor(),
    .by = c(ano_pnad, UF)
  ) %>%
    #Calcula o Piso da renda para cada UF e para cada ano da pnadc
    mutate(
      piso_renda = min(rendimento_mensal, na.rm = TRUE),
      .by = c(centil_renda, ano_pnad, UF)
    ) %>%
    filter(centil_renda %in% c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99)) %>%
    summarise(
      piso_renda = mean(piso_renda, na.rm = TRUE),
      .by = c(centil_renda, ano_pnad, UF)
    ) %>%
    ggplot(aes(
      x = reorder(centil_renda,-piso_renda),
      y = round(piso_renda, 2)
    )) +
    geom_col(fill = "#1d2951") +
    geom_text(aes(label = piso_renda), vjust = -.05) +
    facet_wrap(~ ano_pnad) +
    labs(
      x = "Centil de Renda",
      y = "Rendimento Mensal",
      title = "Rendimento Mensal para estar entre os % mais Ricos de Pernambuco",
      caption = "Fonte: PNADC (2023) \n @baruqrodrigues"
    )
  
#São Paulo
  
  dados %>%
    filter(UF == "São Paulo") %>% 
  #calcula o centil por Estado e por Ano da PnadC
  mutate(
    centil_renda = ntile(rendimento_mensal, 100) %>% as.factor(),
    .by = c(ano_pnad, UF)
  ) %>%
    #Calcula o Piso da renda para cada UF e para cada ano da pnadc
    mutate(
      piso_renda = min(rendimento_mensal, na.rm = TRUE),
      .by = c(centil_renda, ano_pnad, UF)
    ) %>%
    filter(centil_renda %in% c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99)) %>%
    summarise(
      piso_renda = mean(piso_renda, na.rm = TRUE),
      .by = c(centil_renda, ano_pnad, UF)
    ) %>%
    ggplot(aes(
      x = reorder(centil_renda,-piso_renda),
      y = round(piso_renda, 2)
    )) +
    geom_col(fill = "#1d2951") +
    geom_text(aes(label = piso_renda), vjust = -.05) +
    facet_wrap(~ ano_pnad) +
    labs(
      x = "Centil de Renda",
      y = "Rendimento Mensal",
      title = "Rendimento Mensal para estar entre os % mais Ricos de São Paulo",
      caption = "Fonte: PNADC (2023) \n @baruqrodrigues"
    )


