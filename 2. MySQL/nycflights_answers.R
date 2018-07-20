library(tidyverse)

# 1. Encuentra todos los vuelos hacia SFO ó OAK.
vuelos_sfo_oak <- nycflights13::flights %>%
  filter(dest %in% c('OAK','SFO'))
vuelos_sfo_oak

# 2. Los vuelos con un retraso mayor a una hora.
vuelos_retraso <- nycflights13::flights %>%
  filter(arr_delay > 60 | dep_delay > 60)
vuelos_retraso

# 3. En los que el retraso de llegada es más del doble que el retraso de salida.
vuelos_llegada_mas_salida <- nycflights13::flights %>%
  filter(dep_delay > 0 & arr_delay > 2*dep_delay)
vuelos_llegada_mas_salida

# 4. Ve la ayuda de select (`?select`) y escribe tres maneras de seleccionar las variables de retraso (delay).
flights_retrasos <- nycflights13::flights %>%
  select(ends_with('delay'))
flights_retrasos

flights_retrasos <- nycflights13::flights %>%
  select(contains('delay'))
flights_retrasos

flights_retrasos <- nycflights13::flights %>%
  select(c(dep_delay, arr_delay))
flights_retrasos

# 5. Ordena los vuelos por fecha de salida y hora.
flights_ordenado_salida_hora <- nycflights13::flights %>%
  arrange(date,hour)
flights_ordenado_salida_hora

# 6. ¿Cuáles son los diez vuelos con mayor retraso?
mayor_retraso <- nycflights13::flights %>%
  arrange(desc(dep_delay)) %>%
  head(10)
mayor_retraso

# 7. ¿Qué vuelos _ganaron_ más tiempo en el aire?
vuelos_ganaron <- nycflights13::flights %>%
  filter(arr_delay - dep_delay < 0) %>%
  mutate(ganancia = arr_delay - dep_delay) %>%
  arrange(ganancia) %>%
  head(20)
vuelos_ganaron

# 8. Calcula la velocidad en millas por hora a partir de la variable tiempo y la distancia (en millas). ¿Qué vuelo fue el más rápido?
flights_2 <- nycflights13::flights %>%
  mutate(millas_hora = dist/(time/60)) %>%
  arrange(desc(millas_hora))
flights_2

# 10. Calcula el retraso de llegada promedio por fecha.
retrasos_promedios <- nycflights13::flights %>%
  group_by(date) %>%
  dplyr::summarise(retraso_avg=mean(dep_delay,na.rm=T)) %>%
  arrange(desc(retraso_avg))
retrasos_promedios

# 11. ¿Qué otros resúmenes puedes hacer para explorar el retraso por fecha?
ggplot(retrasos_promedios,aes(x=date,y=retraso_avg))+
  geom_line()

retrasos_por_dia <- nycflights13::flights %>%
  mutate(date2 = ymd(date),
         dia_semana = as.factor(wday(date,label=TRUE))) %>%
  group_by(dia_semana) %>%
  dplyr::summarise(retraso_avg=mean(dep_delay,na.rm=T))
retrasos_por_dia

ggplot(retrasos_por_dia, 
       aes(x=dia_semana,y=retraso_avg,color=dia_semana,fill=dia_semana))+
  geom_bar(stat = "identity")

by_date <- nycflights13::flights %>%
  group_by(date)
no_miss <- by_date %>%
  filter(!is.na(dep))
delays <- no_miss %>%
  dplyr::summarise(
    mean_delay = mean(dep_delay),
    n = n())

ggplot(delays, aes(x=date,y=n))+
  geom_line()

# 12. ¿Qué destinos tienen el promedio de retrasos más alto?
nycflights13::flights %>% 
  group_by(dest) %>% 
  dplyr::summarise(avg_delay = mean(dep_delay + arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(avg_delay)) %>% 
  head(10)

# 13. ¿Qué vuelos (compañía + vuelo) ocurren diario?
nycflights13::flights %>% 
  mutate(day = day(date)) %>% 
  group_by(carrier, flight) %>% 
  dplyr::summarise(n = length(unique(day))) %>% 
  ungroup() %>% 
  filter(n > 29)

# 14. En promedio, ¿cómo varían a lo largo del día los retrasos de vuelos no cancelados? (pista: hour + minute / 60)
nycflights13::flights %>% 
  filter(cancelled == 0) %>% 
  mutate(decimal_time = (hour + minute / 60)) %>% 
  group_by(decimal_time) %>% 
  dplyr::summarise(avg_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = decimal_time, y = avg_delay)) +
  geom_point()

# 15. Ahora combinamos datos a nivel hora con condiciones climáticas, ¿cuál es el tipo de unión adecuado?
hourly_delay <- nycflights13::flights %>%
  group_by(date, hour) %>%
  filter(!is.na(dep_delay)) %>%
  dplyr::summarise(
    delay = mean(dep_delay),
    n = n() ) %>%
  filter(n > 10)

delay_weather <- hourly_delay %>% left_join(nycflights13::weather)

arrange(delay_weather, -delay)

# 16. ¿Qué condiciones climáticas están asociadad con retrasos en las salidas de Houston?
nycflights13::flights %>% 
  filter(dep_delay > 0) %>%
  left_join(nycflights13::airports, by=c("dest" = "iata")) %>% 
  left_join(nycflights13::weather) %>% 
  filter(city == "Dallas", !is.na(conditions)) %>% 
  select(conditions) %>% 
  unique

# 17. Explora si los aviones más viejos están asociados a mayores retrasos, responde con una gráfica.
nycflights13::flights %>% 
  left_join(nycflights13::planes) %>% 
  mutate(total_delay = dep_delay + arr_delay,
         age = year(today()) - year) %>% 
  select(total_delay, age) %>% 
  ggplot(aes(x = age, y = total_delay)) +
  geom_point()