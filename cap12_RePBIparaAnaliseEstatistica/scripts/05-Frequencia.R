# Estatistica Básica 

# Parte 4 - Tabela de Frequência

# Definindo a pasta de trabalho
# Substitua o caminho abaixo pela pasta no seu computador
setwd("C:/Users/Bruno202021/Desktop/Outros/VER DEPOIS/DSA/MS PBI para DS, v2.0/cap12_RePBIparaAnaliseEstatistica/dados")
getwd()

# Carregando o dataset
?read.table
dados <- read.table("Usuarios.csv",
                    dec = ".",
                    sep = ",",
                    h = T,
                    fileEncoding = "windows-1252")

# Resumo do dataset
View(dados)
str(dados)
head(dados)
tail(dados)

# Medidas de Tendência Central
summary(dados$idade_anos)
summary(dados$salario)

# Média
?mean
mean(dados$idade_anos)
mean(dados$salario)

# Tabela de Frequências Absoluta
freq <- table(dados$grau_instrucao)
View(freq)

# Tabela de Frequências Relativa
freq_rel <- prop.table(freq)
View(freq_rel)

# Porcentagem (100 * freq_rel_table)
p_freq_rel <- 100 * prop.table(freq_rel)
View(p_freq_rel)

# Adiciona linha de total
View(freq)
freq <- c(freq, sum(freq))
View(freq)
names(freq)[4] <- "Total"
View(freq)

# Tabela final com todos os valores

# Calculamos frequência relativa e proporcional
freq_rel <- c(freq_rel, sum(freq_rel))
p_freq_rel <- c(p_freq_rel, sum(p_freq_rel))

# Tabela final com todos os vetores 
tabela_final <- cbind(freq,
                      freq_rel = round(freq_rel, digits = 2),
                      p_freq_rel = round(p_freq_rel, digits = 2))
View(tabela_final)
