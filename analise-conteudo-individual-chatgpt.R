# Instalar pacotes necessários (se não tiverem sido instalados)
install.packages("tm")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("dplyr")

# Carregar pacotes
library(readxl)
library(tm)
library(wordcloud)
library(ggplot2)
library(dplyr)

# Carregar os dados
df <- read_excel("./dados/avaliacao-chatgpt.xlsx", sheet = "Respostas ao formulário 1")

# Função para análise de conteúdo de uma variável textual
analisar_texto <- function(text_data, var_name) {
  # 1. Criar um corpus de textos
  corpus <- Corpus(VectorSource(text_data))
  
  # 2. Pré-processamento do texto
  corpus <- tm_map(corpus, content_transformer(tolower))  # Colocar tudo em letras minúsculas
  corpus <- tm_map(corpus, removePunctuation)  # Remover pontuação
  corpus <- tm_map(corpus, removeNumbers)  # Remover números
  corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))  # Remover stopwords em português
  corpus <- tm_map(corpus, stripWhitespace)  # Remover espaços em branco desnecessários
  
  # 3. Criar uma matriz de termos (Term-Document Matrix)
  tdm <- TermDocumentMatrix(corpus)
  tdm_matrix <- as.matrix(tdm)
  
  # 4. Contagem das palavras mais frequentes
  word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
  word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)
  
  # 5. Exibir as 10 palavras mais frequentes
  top_words <- head(word_freq_df, 10)
  print(paste("Palavras mais frequentes na variável", var_name))
  print(top_words)
  
  # 6. Visualização das palavras mais frequentes
  ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
    geom_bar(stat = "identity", fill = "blue") +
    coord_flip() +
    labs(title = paste("Palavras mais frequentes em", var_name), x = "Palavras", y = "Frequência")
  
  # 7. Nuvem de palavras
  set.seed(1234)
  wordcloud(words = word_freq_df$word, freq = word_freq_df$freq, min.freq = 2,
            max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
}

# Analisar cada variável textual separadamente

# Análise da variável 'atividade'
analisar_texto(df$atividade, "Atividade")

# Análise da variável 'vantagem-desvantagem-outras-ia'
analisar_texto(df$`vantagem-desvantagem-outras-ia`, "Vantagem/Desvantagem de outras IA")

# Análise da variável 'o-que-deveria-propiciar'
analisar_texto(df$`o-que-deveria-propiciar`, "O que deveria propiciar")

# Análise da variável 'quais-substituidos-ia'
analisar_texto(df$`quais-substituidos-ia`, "Quais substituídos pela IA")

# Análise da variável 'comentarios'
analisar_texto(df$comentarios, "Comentários")
