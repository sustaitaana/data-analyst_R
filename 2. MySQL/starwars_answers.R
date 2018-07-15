library(dplyr)

# 1. ¿Qué personajes son droides?
starwars %>% 
  filter(species == "Droid")

# 2. Muestra el color de cabello, piel y ojos de los personajes femeninos
starwars %>% 
  filter(gender == "female") %>% 
  select(name, ends_with("color")) 

# 3. Calcula el índice de masa corporal de cada personaje
starwars %>% 
  mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
  select(name:mass, bmi)

# 4. ¿Cuáles son los 5 personajes más pesados?
starwars %>% 
  arrange(desc(mass)) %>% 
  head(5)

# 5. ¿Cuántos personajes de cada especie hay? ¿Cuál es el promedio de peso de cada especie? Muestra esta información de aquellas especies que tengan más de 1 personaje.
starwars %>%
  group_by(species) %>%
  summarise(
    n = n(),
    mass = mean(mass, na.rm = TRUE)
  ) %>%
  filter(n > 1, !is.na(species))