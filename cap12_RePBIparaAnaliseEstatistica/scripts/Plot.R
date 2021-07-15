# Definindo a pasta de trabalho
# Substitua o caminho abaixo pela pasta no seu computador
setwd("C:/Users/Bruno202021/Desktop/Outros/VER DEPOIS/DSA/MS PBI para DS, v2.0/cap12_RePBIparaAnaliseEstatistica/dados")
getwd()

# Carregando o dataset
?read.csv
vendas <- read.csv("Vendas.csv", fileEncoding = "windows-1252")

# Carrega o ggplot2
library(ggplot2)

# Cria o grafico
?qplot
qplot(Valor, Custo, data = vendas)
