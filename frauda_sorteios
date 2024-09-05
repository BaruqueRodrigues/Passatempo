# Numero total de tickets a serem sorteados
total_tickets <- 100

# Tickets que eu comprei
tickets_desejados <- c(10)  

# Número total de tickets que vão ser sorteados
quantidade_sorteados <- 5  

# Numero de seeds que eu vou verificar
num_seeds <- 1000

# Função para verificar se os tickets desejados foram sorteados
verifica_semente <- function(seed) {
  base::set.seed(seed)
  tickets_sorteados <- base::sample(1:total_tickets, quantidade_sorteados)
  base::any(tickets_desejados %in% tickets_sorteados)
}

# Usando purrr::keep para retornar apenas as seeds que satisfazem a condição
purrr::keep(1:num_seeds, verifica_semente)
