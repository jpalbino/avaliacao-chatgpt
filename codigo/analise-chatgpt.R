# Instalar pacotes necessários (se não tiverem sido instalados)
if (!("ggplot2") %in% installed.packages()) install.packages("ggplot2")
if (!("dplyr") %in% installed.packages()) install.packages("dplyr")
if (!("readxl") %in% installed.packages()) install.packages("readxl")

# Carregar pacotes
library(readxl)
library(dplyr)
library(ggplot2)

# Carregar os dados
df <- read_excel("./dados/avaliacao-chatgpt.xlsx", sheet = "Respostas ao formulário 1")

# 1. Visualizar as primeiras linhas do dataset
head(df)

# 2. Resumo das variáveis categóricas
summary(df)

# 3. Identificar valores ausentes
na_counts <- colSums(is.na(df))
print(na_counts)

# 4. Distribuição de variáveis categóricas
ggplot(df, aes(x = `faixa-etaria`)) + geom_bar() + theme_minimal() + labs(title = "Distribuição de Faixa Etária")

ggplot(df, aes(x = genero)) + geom_bar() + theme_minimal() + labs(title = "Distribuição de Gênero")

ggplot(df, aes(x = escolaridade)) + geom_bar() + theme_minimal() + labs(title = "Distribuição de Escolaridade")

# 5. Visualização de variáveis numéricas
ggplot(df, aes(x = `grau-entendimento-perguntas`)) + 
  geom_histogram(bins = 5) + 
  theme_minimal() + 
  labs(title = "Distribuição do Grau de Entendimento")

# 6. Análise de correlação entre variáveis numéricas (exemplo com grau de entendimento)
correlacao <- cor(df$`grau-entendimento-perguntas`, df$`quanto-ia-substituir`, use="complete.obs")
print(correlacao)
