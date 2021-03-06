# Estatistica B�sica 

# Exerc�cio Estat�stica B�sica na Linguagem R

# Definindo a pasta de trabalho
# Substitua o caminho abaixo pela pasta no seu computador
setwd("C:/Users/Bruno202021/Desktop/Outros/VER DEPOIS/DSA/MS PBI para DS, v2.0/cap12_RePBIparaAnaliseEstatistica/dados")
getwd()

# Carregando o dataset
notas <- read.csv("Notas.csv", fileEncoding = "windows-1252")

# Exerc�cio 1: Apresente um resumo de tipos de dados e estat�sticas do dataset
# Resumo do dataset
View(notas)
str(notas)
head(notas)
tail(notas)

# Exerc�cio 2: Qual a m�dia de cada turma?
# Medidas de Tend�ncia Central
summary(notas$TurmaA)
summary(notas$TurmaB)

# M�dia
?mean
mean(notas$TurmaA)
mean(notas$TurmaB)

# M�dia Ponderada
?weighted.mean
weighted.mean(notas$TurmaA, w = notas$TurmaB)

# Exerc�cio 3: Qual turma apresentou maior variabilidade de notas? Justifique sua resposta.
# Desvio Padr�o
sd(notas$TurmaA)
sd(notas$TurmaB)
# A turma A. Em m�dia, as notas da turma A est�o 14 pontos acima ou abaixo da m�dia, enquanto na turma B est�o em m�dia 6 pontos.

# Exerc�cio 4: Calcule o coeficiente de varia��o das 2 turmas.
media_ta <- mean(notas$TurmaA)
media_tb <- mean(notas$TurmaB)

sd_ta <- sd(notas$TurmaA)
sd_tb <- sd(notas$TurmaB)

cvA <- sd_ta / media_ta * 100
cvB <- sd_tb / media_tb * 100

cvA 
cvB

# Exerc�cio 5: Qual nota apareceu mais vezes em cada turma?
# Moda
# Criando uma fun��o
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

# Vari�ncia
var(notas$TurmaA)
var(notas$TurmaB)
