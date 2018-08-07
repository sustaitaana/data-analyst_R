# Función para obtener la moda de una variable, si es caracter o factor y si la variable es numérica se redondea a los dos decimales más significativos
my_mode <- function(x){
  if (class(x) %in% c("character", "factor")) {
    table(x) %>%
      which.max() %>%
      names()
  }
  else {
    table(round(x, 2)) %>%
      which.max() %>%
      names()
  }
}

# Función para hacer profiling del set de datos por columna, dependiendo de si son categóricas o numéricas
profiling <- function(frame, type = "other"){
  # cardinalidad
  uniques <- data.frame(uniques = sapply(frame, function(x) unique(x) %>% length()))
  
  # Se buscan valores NA
  nan <- data.frame(nan = sapply(frame, function(x) sum(is.na(x))))
  
  if (type == "categorical"){
    
    # Se obtiene la moda
    mode <- data.frame(mode = sapply(frame, function(x) my_mode(x)))
    profiling_df <- cbind(uniques, nan, mode)
    profiling_df
  }
  else {
    profiling_df <- cbind(uniques, nan)
    profiling_df
  }
}
