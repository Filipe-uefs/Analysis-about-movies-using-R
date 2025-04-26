
library(dplyr)
movies_data_set = read.csv("tmdb_5000_movies.csv")


columns_to_ignore = c("homepage", "id", "keywords", "overview", "spoken_languages", "status", "tagline", "original_title")
movies_dataset_filtred <- movies_data_set[, !(names(movies_data_set) %in% columns_to_ignore)]
movies_dataset_filtred <- movies_dataset_filtred[movies_dataset_filtred$budget != 0, ]
