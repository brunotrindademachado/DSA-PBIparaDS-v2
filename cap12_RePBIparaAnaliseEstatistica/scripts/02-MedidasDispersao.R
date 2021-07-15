# Estatistica Basica 

# Parte 2 - Medidas de Dispersão 

# Definindo a pasta de trabalho
# Substitua o caminho abaixo pela pasta no seu computador

setwd("C:/Users/Bruno202021/Desktop/Outros/VER DEPOIS/DSA/MS PBI para DS, v2.0/cap12_RePBIparaAnaliseEstatistica/dados")
getwd()

# Carregando o dataset
vendas <- read.csv("Vendas.csv", fileEncoding = "windows-1252")

# Resumo do dataset
View(vendas)
str(vendas)
summary(vendas$Valor)
summary(vendas$Custo)

# Variância
var(vendas$Valor)

# Desvio Padrão
sd(vendas$Valor)
