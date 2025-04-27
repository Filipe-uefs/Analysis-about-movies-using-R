
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

# Histograma com curva de densidade para budget
ggplot(movies_dataset_filtred, aes(x = budget, y = after_stat(density))) +
  geom_histogram() +
  geom_density(color = "green", linewidth = 1)

# Histograma com curva de densidade para revenue
ggplot(movies_dataset_filtred, aes(x = revenue, y = after_stat(density))) +
  geom_histogram() +
  geom_density(color = "red", linewidth = 1)

# Boxplot de budget
ggplot(movies_dataset_filtred, aes(y = budget)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Boxplot de Budget",
    y = "Budget"
  ) +
  theme_minimal()

# Boxplot de revenue
ggplot(movies_dataset_filtred, aes(y = revenue)) +
  geom_boxplot(fill = "lightpink", color = "black") +
  labs(
    title = "Boxplot de Revenue",
    y = "Revenue"
  ) +
  theme_minimal()



#Tabela com os dados usando da língua usada nos filmes
table(movies_dataset_filtred$original_language)

#Tabela com os dados usando do Genêro usada nos filmes
table(movies_dataset_filtred$genre)

#Tabela com os dados usando do Empresas que produziram os filmes
table(movies_dataset_filtred$production_company)

#Tabela com os dados usando dos paises que produziram os filmes
table(movies_dataset_filtred$production_country)





# medidas de resumo
summary(movies_dataset_filtred$budget)
summary(movies_dataset_filtred$revenue)


# Variância budget
variancia_budget <- var(movies_dataset_filtred$budget)
print(paste("Variância: ", variancia_budget))

# Desvio Padrão budget
desvio_padrao_budget <- sd(movies_dataset_filtred$budget)
print(paste("Desvio Padrão: ", desvio_padrao_budget))

# Coeficiente de Variação budget
coeficiente_variacao_budget <- desvio_padrao_budget / mean(movies_dataset_filtred$budget) * 100
print(paste("Coeficiente de Variação: ", coeficiente_variacao_budget, "%"))


# Variância revenue
variancia_revenue <- var(movies_dataset_filtred$revenue, na.rm = TRUE)
print(paste("Variância: ", variancia_revenue))

# Desvio Padrão revenue
desvio_padrao_revenue <- sd(movies_dataset_filtred$revenue, na.rm = TRUE)
print(paste("Desvio Padrão: ", desvio_padrao_revenue))

# Coeficiente de Variação 
coeficiente_variacao_revenue <- desvio_padrao_revenue / mean(movies_dataset_filtred$revenue, na.rm = TRUE) * 100
print(paste("Coeficiente de Variação: ", coeficiente_variacao_revenue, "%"))




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

# Gráfico Gênero vs Receita
media_receita_genero <- movies_dataset_filtred |>
  group_by(genre) |>
  summarise(media_receita = mean(revenue)) |>
  arrange(desc(media_receita))

ggplot(media_receita_genero, aes(x = reorder(genre, -media_receita), y = media_receita)) +
  geom_col(color = "blue", fill = "pink") +
  labs(title = "Média de Receita por Gênero", x = "Gênero", y = "Receita Média") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",", decimal.mark = "."))

# Gráfico Média de Votos vs Receita
ggplot(movies_dataset_filtred, aes(x = vote_average, y = revenue)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relação entre Média de Votos e Receita", x = "Média de Votos", y = "Receita") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",", decimal.mark = "."))

# Gráfico Gênero vs Média de Votos
media_votos_genero <- movies_dataset_filtred |>
  group_by(genre) |>
  summarise(media_votos = mean(vote_average)) |>
  arrange(desc(media_votos))

ggplot(media_votos_genero, aes(x = reorder(genre, -media_votos), y = media_votos)) +
  geom_col(color = "blue", fill = "pink") +
  labs(title = "Média de Votos por Gênero", x = "Gênero", y = "Média de Votos") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",", decimal.mark = "."))

# Gráfico Orçamento vs Média de Votos
ggplot(movies_dataset_filtred, aes(x = budget, y = vote_average)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relação entre Orçamento e Média de Votos", x = "Orçamento", y = "Média de Votos") +
  scale_x_continuous(labels = scales::label_number(big.mark = ",", decimal.mark = "."))

# Gráfico com linha de tendência entre budget e revenue
ggplot(movies_dataset_filtred, aes(x = budget, y = revenue)) +
  geom_point(alpha = 0.6, color = "darkorange") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Budget vs Revenue com Regressão Linear",
    x = "Budget",
    y = "Revenue"
  ) +
  theme_light()



