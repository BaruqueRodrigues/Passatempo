### Calculando a Representação da notação de analise de sentimento

# Xm = [1, sum(positive_freqs), sum(negative_freqs)]

# Criação do dicionário de palavras positivas e negativas
dicionario_palavras <- tibble(
  positive = c("bom", "ótimo", "incrível", "fantastico", "espetacular"),
  negative = c("ruim", "terrível", "péssimo", "pior", "ódio")
)

# Função para limpar o texto e remover a pontuação
clean_text <- function(text) {
  text %>% 
    stringr::str_to_lower() %>% 
    stringr::str_remove_all("[[:punct:]]") %>%
    stringr::str_remove_all("[[:cntrl:]]") %>%
    stringr::str_squish()
  }

# Função para contar as frequências das palavras positivas ou negativas em um tweet
freqs <- function(words, tweet) {
  # Limpa o texto do tweet
  tweet <- clean_text(tweet)
  # Divide o tweet em palavras.
  tweet_words <- unlist(stringr::str_split(tweet, "\\s+"))
  # Aplica a contagem de frequência para cada palavra no vetor
  frequencias <- sapply(words, function(word) sum(tweet_words == word))
  # Retorna a soma das frequências
  sum(frequencias)
}
# Calculas as frequencias

calcula_frequencias <- function(dicionario_palavras, tweet, bias = 1) {
  # Calcula a frequência de palavras positivas
  freqs_positivas <- freqs(dicionario_palavras$positive, tweet)
  # Calcula a frequência de palavras negativas
  freqs_negativas <- freqs(dicionario_palavras$negative, tweet)
  # Retorna o vetor de frequências
  c(bias, freqs_positivas, freqs_negativas)
}

# Exemplo de uso da função freqs com palavras positivas em um tweet.
tweet <- "Esse filme é bom, simplesmente fantástico e incrível!"

# Mostra a função
calcula_frequencias(dicionario_palavras, tweet)
