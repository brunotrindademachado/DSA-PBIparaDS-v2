# Estatistica Básica 

# Exercício Estatística Básica na Linguagem R

# Definindo a pasta de trabalho
# Substitua o caminho abaixo pela pasta no seu computador
setwd("C:/Users/Bruno202021/Desktop/Outros/VER DEPOIS/DSA/MS PBI para DS, v2.0/cap12_RePBIparaAnaliseEstatistica/dados")
getwd()

# Carregando o dataset
notas <- read.csv("Notas.csv", fileEncoding = "windows-1252")

# Exercício 1: Apresente um resumo de tipos de dados e estatísticas do dataset
# Resumo do dataset
View(notas)
str(notas)
head(notas)
tail(notas)

# Exercício 2: Qual a média de cada turma?
# Medidas de Tendência Central
summary(notas$TurmaA)
summary(notas$TurmaB)

# Média
?mean
mean(notas$TurmaA)
mean(notas$TurmaB)

# Média Ponderada
?weighted.mean
weighted.mean(notas$TurmaA, w = notas$TurmaB)

# Exercício 3: Qual turma apresentou maior variabilidade de notas? Justifique sua resposta.
# Desvio Padrão
sd(notas$TurmaA)
sd(notas$TurmaB)
# A turma A. Em média, as notas da turma A estão 14 pontos acima ou abaixo da média, enquanto na turma B estão em média 6 pontos.

# Exercício 4: Calcule o coeficiente de variação das 2 turmas.
media_ta <- mean(notas$TurmaA)
media_tb <- mean(notas$TurmaB)

sd_ta <- sd(notas$TurmaA)
sd_tb <- sd(notas$TurmaB)

cvA <- sd_ta / media_ta * 100
cvB <- sd_tb / media_tb * 100

cvA 
cvB

# Exercício 5: Qual nota apareceu mais vezes em cada turma?
# Moda
# Criando uma função
moda <- function(v) {
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v, valor_unico)))]
}

# Obtendo a Moda
moda(notas$TurmaA)
moda(notas$TurmaB)


### Extras

# Mediana
median(notas$TurmaA)
median(notas$TurmaB)

# Variância
var(notas$TurmaA)
var(notas$TurmaB)
