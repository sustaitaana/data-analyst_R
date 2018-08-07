# función para obtener la moda, si la variable es numérica redondeamos a dos dígitos
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

# función para hacer profiling del set de datos por columna, dependiendo de si son categóricas o numéricas
profiling <- function(frame, type = "other"){
  # cardinalidad
  uniques <- data.frame(uniques = sapply(frame, function(x) unique(x) %>% length()))
  
  # buscamos valores NA
  nan <- data.frame(nan = sapply(frame, function(x) sum(is.na(x))))
  
  if (type == "categorical"){
    
    # obtenemos la moda
    mode <- data.frame(mode = sapply(frame, function(x) my_mode(x)))
    profiling_df <- cbind(uniques, nan, mode)
    profiling_df #Recordar que, al ser esta la última instrucción, se devuelve este por lo que se comporta como return
  }
  else {
    profiling_df <- cbind(uniques, nan)
    profiling_df
  }
}