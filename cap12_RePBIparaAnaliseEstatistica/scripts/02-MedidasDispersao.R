# Estatistica Basica 

# Parte 2 - Medidas de Dispers�o 

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

# Vari�ncia
var(vendas$Valor)

# Desvio Padr�o
sd(vendas$Valor)
