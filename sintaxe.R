
#Carrega bibliotecas necessárias
library(dplyr)
library(ggplot2)
library(jsonlite)


#Carrega dataset
movies_dataset = read.csv("tmdb_5000_movies.csv")

#Filtrando informações irrelevantes no dataset e tratando dados
columns_to_ignore = c("homepage", "id", "keywords", "overview", "spoken_languages", "status", "tagline", "original_title")
movies_dataset_filtred <- movies_dataset[, !(names(movies_dataset) %in% columns_to_ignore)]
movies_dataset_filtred <- movies_dataset_filtred[movies_dataset_filtred$budget != 0, ]
movies_dataset_filtred <- movies_dataset_filtred[movies_dataset_filtred$revenue != 0, ]

set.seed(15112000) # data de nascimento como parametro para criar amostra
movies_dataset_filtred = movies_dataset_filtred[sample(nrow(movies_dataset_filtred), 350), ]

movies_dataset_filtred <- movies_dataset_filtred |>
  mutate(
    genre = as.character(sapply(lapply(genres, fromJSON), function(x) x$name[1])),
    production_company = as.character(sapply(lapply(production_companies, fromJSON), function(x) x$name[1])),
    production_country = as.character(sapply(lapply(production_countries, fromJSON), function(x) x$name[1])),
  )

movies_dataset_filtred <-
  movies_dataset_filtred[, !(names(movies_dataset_filtred) %in% c("genres", "production_companies", "production_countries"))]

#Tabelas individuais

#Tabela com os dados usando da língua usada nos filmes
table(movies_dataset_filtred$original_language)

#Tabela com os dados usando do Genêro usada nos filmes
table(movies_dataset_filtred$genre)

#Tabela com os dados usando do Empresas que produziram os filmes
table(movies_dataset_filtred$production_company)

#Tabela com os dados usando dos paises que produziram os filmes
table(movies_dataset_filtred$production_country)


#Tabelas agrupando duas variáveis 

#Tabela com os dados relacionando países que produziram os filmes e a língua do do filme
table(movies_dataset_filtred$production_country, movies_dataset_filtred$original_language)

#Tabela com os dados gêneros dos filmes produzidos pela França
france_movies = movies_dataset_filtred[movies_dataset_filtred$production_country == "France", ]
table(france_movies$production_country, france_movies$genre)

#Tabela com os dados gêneros dos filmes produzidos pela Paramount Pictures
paroumunt_movies = movies_dataset_filtred[movies_dataset_filtred$production_company == "Paramount Pictures", ]
table(paroumunt_movies$production_company, paroumunt_movies$genre)


# Cria gráfico comparando classificando filmes pela língua do filme
ggplot(
  movies_dataset_filtred, aes(x = original_language, y = after_stat(prop), group = 1)
  ) +
  geom_bar(color = "blue", fill = "pink") +
  labs(title = "Gráfico de quantidade de filmes produzidos por língua", x = "Língua", y = "Porcentagem") +
  scale_y_continuous(labels = scales::percent_format())

#Gráfico de desnisdade para budget
ggplot(movies_dataset_filtred, aes(x = budget, y =
                                  after_stat(density))) +
  geom_density(color = "green", linewidth = 1) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",",
                                                   decimal.mark = "."))
#Gráfico de desensidade de revenue
ggplot(movies_dataset_filtred, aes(x = revenue, y =
                                     after_stat(density))) +
  geom_density(color = "green", linewidth = 1) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",",
                                                   decimal.mark = "."))

#Gráfico de desnsidade de média das notas do filmes
ggplot(movies_dataset_filtred, aes(x = vote_average, y =
                                     after_stat(density))) +
  geom_density(color = "green", linewidth = 1) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",",
                                                   decimal.mark = "."))

media_por_genero <- movies_dataset_filtred |>
  group_by(genre) |>
  summarise(
    media_vote = mean(vote_average, na.rm = TRUE),
    n_filmes = n()
  ) |>
  arrange(desc(media_vote))

