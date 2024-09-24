# Instalar pacotes necessários (se não tiverem sido instalados)
if (!("ggplot2") %in% installed.packages()) install.packages("ggplot2")
if (!("dplyr") %in% installed.packages()) install.packages("dplyr")
if (!("readxl") %in% installed.packages()) install.packages("readxl")
if (!("readr") %in% installed.packages()) install.packages("readr")
if (!("readODS") %in% installed.packages()) install.packages("readODS")
if (!("tidyverse") %in% installed.packages()) install.packages("tidyverse")
if (!("corrplot") %in% installed.packages()) install.packages("corrplot")

# Importando dados utilizando a função "Import Dataset" no RStudio
# Importando dados de um arquivo CSV com a biblioteca "readr"
library(readr)
avaliacao_chatgpt_2 <- read_delim("dados/avaliacao-chatgpt-2.csv",
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Importando dados de um arquivo CSV com a biblioteca "base"
`avaliacao-chatgpt.2` <- read.csv("~/Downloads/GitHub/avaliacao-chatgpt/dados/avaliacao-chatgpt-2.csv", sep=";")

# Importando dados de um arquivo EXCEL com a biblioteca "readxl"
library(readxl)
avaliacao_chatgpt2 <- read_excel("dados/avaliacao-chatgpt.xlsx")

# Importando dados de um arquivo ODS com a biblioteca "readODs"
library(readODS)
avaliacao_chatgpt3 <-read_ods("dados/avaliacao-chatgpt.ods")

#_________________________________________________________________________________________________________
library(readxl)
library(tidyverse)
dados <- read_excel("dados/avaliacao-chatgpt.xlsx")

# Converter algumas das variáveis do tipo character em fatores, exceto a variável 'expectativa'
dados <- dados %>%
  mutate(across(.cols = where(is.character) & 
                  !c("atividade", "vantagem-desvantagem-outras-ia", "o-que-deveria-propiciar", "quais-substituidos-ia", "comentarios"), as.factor)) 

# Identificar as variáveis categóricas (fatores ou do tipo character)
categorical_vars <- dados %>% select(where(is.factor))

# Gerar gráficos de barras para cada variável categórica
for (var_name in names(categorical_vars)) {
  # Selecionar a variável atual
  var_data <- categorical_vars[[var_name]]
  
  # Título do gráfico
  title <- paste("Distribuição da variável:", var_name)
  
  # Gerar o gráfico de barras
  plot(var_data, 
       main = title,          # Título do gráfico
       xlab = var_name,       # Rótulo do eixo X
       ylab = "Frequência",   # Rótulo do eixo Y
       col = "lightblue",     # Cor das barras
       las = 2)               # Rotacionar os rótulos do eixo X para facilitar a leitura
  
  # Pausar a exibição do gráfico para visualizar um gráfico de cada vez
  readline(prompt="Pressione [Enter] para continuar para o próximo gráfico.")

}

# Identificar as variáveis categóricas (fatores ou do tipo character)
categorical_vars_st <- dados %>% select(where(is.factor))

# Gerar gráficos de barras para cada variável categórica
for (var_name in names(categorical_vars_st)) {
  # Gerar o gráfico de barras usando ggplot2
  p <- ggplot(dados, aes_string(x = var_name)) +
    geom_bar(fill = "lightblue", color = "black") +
    labs(title = paste("Distribuição da variável:",var_name),
#         x = var_name,
         y = "Frequência") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotacionar os rótulos do eixo X
  
  # Exibir o gráfico
  print(p)
  
  # Pausar a exibição do gráfico para visualizar um gráfico de cada vez
  readline(prompt="Pressione [Enter] para continuar para o próximo gráfico.")
}

# Encontrar as correlações
# Carregar pacotes necessários
library(corrplot)
library(tidyverse)
# Excluir a variável expectativa
data_no_chr <- dados %>%
  select(-c("atividade", "vantagem-desvantagem-outras-ia", "o-que-deveria-propiciar", "quais-substituidos-ia", "comentarios"))

# Verificar se as variáveis categóricas foram transformadas em fatores
# Se não, transformamos as variáveis categóricas em números usando as.numeric
data_numeric <- data_no_expectativa %>%
  mutate(across(where(is.factor), as.numeric))

# Calcular a matriz de correlação
cor_matrix <- cor(data_numeric, use = "complete.obs")

# Exibir a matriz de correlação
corrplot(cor_matrix, method = "color", tl.cex = 0.8)

# Resumo das principais correlações
summary(cor_matrix)