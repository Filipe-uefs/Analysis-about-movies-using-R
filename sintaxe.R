
#Carrega bibliotecas necessárias
library(dplyr)
library(ggplot2)
library(jsonlite)


#Carrega dataset
movies_dataset = read.csv("tmdb_5000_movies.csv")

set.seed(15112000) # data de nascimento como parametro para criar amostra
movies_dataset = movies_dataset[sample(nrow(movies_dataset), 350), ]

#Filtrando informações irrelevantes no dataset e tratando dados
columns_to_ignore = c("homepage", "id", "keywords", "overview", "spoken_languages", "status", "tagline", "original_title")
movies_dataset_filtred <- movies_dataset[, !(names(movies_dataset) %in% columns_to_ignore)]
movies_dataset_filtred <- movies_dataset_filtred[movies_dataset_filtred$budget != 0, ]

movies_dataset_filtred <- movies_dataset_filtred |>
  mutate(
    genre = as.character(sapply(lapply(genres, fromJSON), function(x) x$name[1])),
    production_company = as.character(sapply(lapply(production_companies, fromJSON), function(x) x$name[1])),
    production_country = as.character(sapply(lapply(production_countries, fromJSON), function(x) x$name[1])),
  )

movies_dataset_filtred <-
  movies_dataset_filtred[, !(names(movies_dataset_filtred) %in% c("genres", "production_companies", "production_countries"))]

#Tabela com os dados usando da língua usada nos filmes
table(movies_dataset_filtred$original_language)

#Tabela com os dados usando do Genêro usada nos filmes
table(movies_dataset_filtred$genre)

#Tabela com os dados usando do Empresas que produziram os filmes
table(movies_dataset_filtred$production_company)

#Tabela com os dados usando dos paises que produziram os filmes
table(movies_dataset_filtred$production_country)

#Tabela com os dados relacionando línguas com genêro dos filmes
table(movies_dataset_filtred$genre, movies_dataset_filtred$production_company)


# Cria gráfico comparando classificando filmes pela língua do filme
ggplot(
  movies_dataset_filtred, aes(x = original_language, y = after_stat(prop), group = 1)
  ) +
  geom_bar(color = "blue", fill = "pink") +
  labs(title = "Gráfico de quantidade de filmes produzidos por língua", x = "Língua", y = "Porcentagem") +
  scale_y_continuous(labels = scales::percent_format())

#Gráfico de 
ggplot(movies_dataset_filtred, aes(x = budget, y =
                                  after_stat(density))) +
  geom_density(color = "green", linewidth = 1) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",",
                                                   decimal.mark = "."))
