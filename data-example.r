library(datasets)
library(jsonlite)
library(readxl)

df <- cars[1:6, ]

mediana <- function(vetor) {
    vetor <- sort(vetor)
    vetor_length <- length(vetor)

    if (vetor_length %% 2 == 0) {
        pos <- vetor_length / 2
        return((vetor[pos] + vetor[pos + 1]) / 2)
    } else {
        pos <- (vetor_length + 2) / 3
        return(vetor[pos])
    }
}

mediana(cars$speed)
mediana(cars$dist)

medianas <- c()
nomes <- c()

# Gerar um vetor de sequência numérica
# seq ( nrow ( cars ) )
# seq_len ( nrow ( cars ) )
# 1 : nrow ( cars )

for (index in seq_len(ncol(cars))) {
    nomes <- c(nomes, colnames(cars)[index])
    medianas <- c(medianas, mediana(cars[, index]))
}

cars_medianas <- data.frame("Variaveis" = nomes, "Medianas" = medianas)

# Leitura de planilhas tipo CSV, XLSX e consumo de API

df <- read.csv("data/dados.csv", sep = ";", dec = ",")

df <- data.frame(read_xlsx("data/dados.xlsx"))

df <- fromJSON(
    "http://educacao.dadosabertosbr.com/api/escolas/buscaavancada?
    situacaoFuncionamento=1&
    energiaInexistente=on&
    aguaInexistente=on&
    esgotoInexistente=on&
    cozinha=on")
