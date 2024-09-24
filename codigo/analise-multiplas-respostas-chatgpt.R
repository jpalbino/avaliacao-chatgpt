# Instalar pacotes necessários
if (!("ggplot2") %in% installed.packages()) install.packages("ggplot2")
if (!("tidyverse") %in% installed.packages()) install.packages("tidyverse")
if (!("readxl") %in% installed.packages()) install.packages("readxl")

library(tidyverse)
library(ggplot2)
library(readxl)

# Função para análise de múltiplas respostas
analisar_multiplas_respostas <- function(text_data, var_name, delimiter = ",") {
  # 1. Separar as múltiplas respostas em várias linhas (usando o delimitador, por exemplo, vírgula)
  resposta_separada <- str_split(text_data, delimiter)
  
  # 2. Transformar a lista em um vetor de respostas individuais
  respostas <- unlist(resposta_separada)
  
  # 3. Remover espaços em branco extras
  respostas <- str_trim(respostas)
  
  # 4. Contar as frequências das respostas
  freq_respostas <- as.data.frame(table(respostas))
  freq_respostas <- freq_respostas %>% arrange(desc(Freq))
  
  # 5. Exibir as respostas mais frequentes
  print(paste("Respostas mais frequentes para a variável:", var_name))
  print(head(freq_respostas, 10))  # Exibir as 10 respostas mais frequentes
  
  # 6. Gerar um gráfico de barras com as respostas mais frequentes
  ggplot(freq_respostas, aes(x = reorder(respostas, Freq), y = Freq)) +
    geom_bar(stat = "identity", fill = "blue") +
    coord_flip() +
    labs(title = paste("Distribuição de respostas em", var_name), x = "Respostas", y = "Frequência")
}

# Carregar os dados
df <- read_excel("./dados/avaliacao-chatgpt.xlsx", sheet = "Respostas ao formulário 1")

# Analisar cada variável com múltiplas respostas separadamente

# 1. Análise da variável 'melhorias'
analisar_multiplas_respostas(df$melhorias, "Melhorias", delimiter = ",")

# 2. Análise da variável 'quais-ia'
analisar_multiplas_respostas(df$`quais-ia`, "Quais IA", delimiter = ",")

# 3. Análise da variável 'beneficios-ia-profissao'
analisar_multiplas_respostas(df$`beneficios-ia-profissao`, "Benefícios da IA na Profissão", delimiter = ",")

# 4. Análise da variável 'outra-ia-facilite-trabalho'
analisar_multiplas_respostas(df$`outra-ia-facilite-trabalho`, "Outras IA que Facilitam o Trabalho", delimiter = ",")

# 5. Análise da variável 'porque-utilizou-chatgpt'
analisar_multiplas_respostas(df$`porque-utilizou-chatgpt`, "Por que Utilizou o ChatGPT", delimiter = ",")

# 6. Análise da variável 'paraque-utilizou-chatgpt'
analisar_multiplas_respostas(df$`paraque-utilizou-chatgpt`, "Para que Utilizou o ChatGPT", delimiter = ",")
