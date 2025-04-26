
#Carrega bibliotecas necessárias
library(dplyr)
library(ggplot2)

#Carrega dataset
movies_dataset = read.csv("tmdb_5000_movies.csv")

#Filtrando informações irrelevantes no dataset e tratando dados
columns_to_ignore = c("homepage", "id", "keywords", "overview", "spoken_languages", "status", "tagline", "original_title")
movies_dataset_filtred <- movies_dataset[, !(names(movies_dataset) %in% columns_to_ignore)]
movies_dataset_filtred <- movies_dataset_filtred[movies_dataset_filtred$budget != 0, ]

table(movies_dataset_filtred$original_language)

# 
# library(tidyverse)
# 
# movies_dataset_filtred <- movies_dataset_filtred |>
#   mutate(
#     genre_names = map_chr(genres, \(g) {
#       paste(map_chr(g, "name"), collapse = ", ")
#     })
#   )

# Cria gráfico comparando classificando filmes pela língua do filme
ggplot(movies_dataset_filtred, aes(x = original_language, y = after_stat(prop),
                                group = 1)) +
  geom_bar(color = "blue", fill = "pink") +
  labs(title = "Gráfico de quantidade de filmes produzidos por língua", x = "Língua", y = "Porcentagem") +
  scale_y_continuous(labels = scales::percent_format())

