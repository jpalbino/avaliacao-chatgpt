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

# Selecionar as colunas de interesse (variáveis textuais mencionadas)
text_columns <- df %>% select(atividade, `vantagem-desvantagem-outras-ia`, `o-que-deveria-propiciar`, `quais-substituidos-ia`, comentarios)

# 1. Combinar o conteúdo de todas as variáveis textuais em um só corpus
combined_text <- apply(text_columns, 1, paste, collapse = " ")

# 2. Criar um corpus de textos
corpus <- Corpus(VectorSource(combined_text))

# 3. Pré-processamento do texto
corpus <- tm_map(corpus, content_transformer(tolower))  # Colocar tudo em letras minúsculas
corpus <- tm_map(corpus, removePunctuation)  # Remover pontuação
corpus <- tm_map(corpus, removeNumbers)  # Remover números
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))  # Remover stopwords em português
corpus <- tm_map(corpus, stripWhitespace)  # Remover espaços em branco desnecessários

# 4. Criar uma matriz de termos (Term-Document Matrix)
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)

# 5. Contagem das palavras mais frequentes
word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

# 6. Exibir as 10 palavras mais frequentes
top_words <- head(word_freq_df, 10)
print(top_words)

# 7. Visualização das palavras mais frequentes
ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Palavras mais frequentes nas variáveis textuais", x = "Palavras", y = "Frequência")

# 8. Nuvem de palavras
set.seed(1234)
wordcloud(words = word_freq_df$word, freq = word_freq_df$freq, min.freq = 2,
          max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
