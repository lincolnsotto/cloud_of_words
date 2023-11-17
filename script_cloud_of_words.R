library(tidyverse)

# Exemplo de carregamento de texto (substitua com seu próprio conjunto de dados)
textos <- c("Este é um exemplo de texto.",
            "Neste exemplo, vamos criar um gráfico de bag of words usando R.",
            "Bag of Words é uma técnica comum em processamento de linguagem natural.",
            "Este é um exemplo de texto.",
            "Neste exemplo, vamos criar um gráfico de bag of words usando R.")

# Pré-processamento do texto
library(tm)
corpus <- Corpus(VectorSource(textos))
corpus <- tm_map(corpus, content_transformer(tolower))  # Converter para minúsculas
corpus <- tm_map(corpus, removePunctuation)            # Remover pontuações
corpus <- tm_map(corpus, removeNumbers)                # Remover números
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))  # Remover stopwords
corpus <- tm_map(corpus, stripWhitespace)              # Remover espaços em branco desnecessários

# Criar a matriz de termos do documento (Bag of Words)
dtm <- DocumentTermMatrix(corpus)

# Converter a DTM em um data frame
dtm_df <- as.data.frame(as.matrix(dtm))
dtm_df$total <- rowSums(dtm_df)  # Adicionar uma coluna com a contagem total de palavras

# Ordenar as palavras pela contagem total
dtm_df <- dtm_df[order(dtm_df$total, decreasing = TRUE), ]






# Plotar as palavras mais frequentes
library(ggplot2)

# Número de palavras para exibir no gráfico (altere conforme necessário)
num_palavras <- 10

# Converter a DTM em um formato tidy
library(tidytext)
dtm_tidy <- tidy(dtm)
top_palavras <- dtm_tidy %>%
  group_by(term) %>%
  summarize(total = sum(count)) %>%
  top_n(num_palavras, total) %>%
  arrange(desc(total))

# Plotar o gráfico de barras
ggplot(top_palavras, aes(x = reorder(term, total), y = total)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Palavras", y = "Frequência", title = "Top Palavras no Bag of Words") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Plotar a nuvem de palavras
library(RColorBrewer)
library(wordcloud)
# Criar a nuvem de palavras
set.seed(123)  # Definir uma semente para a reprodução dos resultados
wordcloud(words = top_palavras$term, freq = top_palavras$total,
          min.freq = 1, max.words = 100, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))  # Definir cores (opcional)






library(wordcloud2)
# Converter top_palavras para um formato adequado para wordcloud2
# Aqui, assume-se que top_palavras tem as colunas 'term' e 'total' representando as palavras e suas frequências
top_palavras <- top_palavras[order(-top_palavras$total), ]  # Ordenar por frequência (do maior para o menor)

# Criar a nuvem de palavras
wordcloud2(data = top_palavras, size = 1, shape = "cloud")
