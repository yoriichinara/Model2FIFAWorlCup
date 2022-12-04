###--------------------------------------------------------------------------###
#-------------MODELO DE POISSON PARA EL MUNDIAL DE QATAR 2022------------------#
###--------------------------------------------------------------------------###

# Directorio de trabajo 
setwd("/home/yoriichinara/Documents/Probability/FinalProyect/Entrega2")

# Enunciado y carga de los paquetes necesarios 
pkgs <- c("lubridate", "ggplot2", "purrr", "tidyr", "MASS", "magrittr", "AER", "dplyr", "readODS")
invisible(lapply(pkgs, require, character.only = TRUE))

###--------------------------------------------------------------------------###
#------FILTRADO Y CONSTRUCCIÓN DEL DATAFRAME QUE ALIMENTARÁ EL MODELO ---------#
###--------------------------------------------------------------------------###

# Read data base
df <- read.csv('results.csv')

# Variables para filtrado
equipos <- c("Qatar", "Ecuador", "Senegal", "Netherlands", "England", "Iran", "United States", "Wales", "Argentina", "Saudi Arabia", "Mexico", "Poland", "France", "Australia", "Denmark", "Tunisia", "Spain", "Costa Rica", "Germany", "Japan", "Belgium", "Canada", "Morocco", "Croatia", "Brazil", "Serbia", "Switzerland", "Cameroon", "Portugal", "Ghana", "Uruguay", "South Korea")

# Dataframe auxiliar que recoge, los partidos 'visitante'
# Filtro para para fecha y equipos del mundial unicamente, selección de la variables de interes
df2 <- df %>%
  select(date, home_team, away_team, home_score, away_score) %>%
  filter(date > "2017-01-01" & away_team %in% equipos) %>%
  select(date, away_score, away_team, home_team, home_score)

names(df2) = c("date", "goles_equipo", "equipo", "rival", "goles_rival")

# Dataframe que recoge resultados de local 
# Filtrado calcado al anterior (solo que sin reacomodar resultados local-visitante)
df <- df %>%
        select(date, home_team, away_team, home_score, away_score) %>%
        filter(date > "2017-01-01" & home_team %in% equipos) %>%
        select(date, home_score, home_team, away_team, away_score)

names(df) = c("date", "goles_equipo", "equipo", "rival", "goles_rival")

# Dataframe quasifinal (uniendo 'local' y 'visitante')
df <- rbind(df, df2)

# Cálculo de la variable asociada al Ranking FIFA
# Database 
dfrank <- read_ods('RankingFIFA.ods')
# Calculo de la variable asociada al Rankingo ELO 
dfelo <- read_ods('RankingELO.ods')
# view(dfrank)
rank <- c()
elo <- c()

# Recorrido por el df para agregar el nuevo dato a cada fila 
for (i in 1:nrow(df)) {
  # Índice del en equipo en dfrank & dfelo ** Estan ordenados alfabeticamente
  ind <- which(dfrank$team == df$equipo[i])
  # Asignación del valor, según la fecha del partido 
  if (year(df$date[i]) == 2017) {
    rank <- c(rank,dfrank$`2017`[ind])
    elo <- c(elo,dfelo$`2017`[ind])
  } 
  if (year(df$date[i]) == 2018) {
    rank <- c(rank,dfrank$`2018`[ind])
    elo <- c(elo,dfelo$`2018`[ind])
  } 
  if (year(df$date[i]) == 2019) {
    rank <- c(rank,dfrank$`2019`[ind])
    elo <- c(elo,dfelo$`2019`[ind])
  } 
  if (year(df$date[i]) == 2020) {
    rank <- c(rank,dfrank$`2020`[ind])
    elo <- c(elo,dfelo$`2020`[ind])
  } 
  if (year(df$date[i]) == 2021) {
    rank <- c(rank,dfrank$`2021`[ind])
    elo <- c(elo,dfelo$`2021`[ind])
  } 
  if (year(df$date[i]) == 2022) {
    rank <- c(rank,dfrank$`2022`[ind])
    elo <- c(elo,dfelo$`2022`[ind])
  } 
}

# Dataframe definitivo 
df <- cbind(df, 'rank' = rank, 'elo' = elo)
# view(df)

###--------------------------------------------------------------------------###
#----------------------------CÁLCULO DEL MODELO--------------------------------# 
###--------------------------------------------------------------------------###

# Calculo y guardado del modelo Poisson
# goles_equipo : Variable dependiente 
# rank, equipo y rival : Variables dummies (como se portan los goles según la posición en el ranking FIFA, el equipo que juega y el rival contra el que se juega)
poisson_model = glm(goles_equipo ~ elo + rank + equipo + rival, family=poisson(link = "log"), data = df) 
summary(poisson_model)

###--------------------------------------------------------------------------###
#---------------------------PREDICCIÓN DE RESULTADOS---------------------------#
###--------------------------------------------------------------------------###

# FUNCIÓN PARA SIMULAR UN (1) PARTIDO
# equipo1 : variable string equipo ---> ej "Qatar"
# equipo2 : variable string rival
# n : número de partidos a simular 
partido <- function(equipo1, equipo2, n) {
  
  # Cálculo de los índices para extraer el ranking FIFA (2022) de dfrank
  ind1 <- which(dfrank$team == equipo1)
  ind2 <- which(dfrank$team == equipo2)
  
  # Predección promedio goles para el equipo1 
  # rank : ranking del equipo1 en 2022
  goles_esperados <- predict(poisson_model,
                             data.frame(elo = dfelo$`2022`[ind1], rank = dfrank$`2022`[ind1], equipo = equipo1, 
                                        rival = equipo2), type="response")
  
  # Predicción promedio goles para el equipo2, rival  
  # rank: ---
  goles_esperados_rival <- predict(poisson_model, 
                                   data.frame(elo = dfelo$`2022`[ind2], rank = dfrank$`2022`[ind2], equipo = equipo2, 
                                              rival = equipo1), type="response")
  
  # Salida en consolar media de goles para equipo1 y para equipo2 estimadas con poisson_model
  tibble(equipo1 = goles_esperados, equipo2 = goles_esperados_rival)
  
  # Cálculo de probabilidad de marcar x goles, usando la distribución de poisson con la media estimada anteriormente
  tst <- tibble(prob = c(dpois(0:7, goles_esperados), dpois(0:7, goles_esperados_rival)), 
                equipo = c(rep(equipo1, 8), rep(equipo2, 8)),
                index = rep(c(0:7),2))
  
  # Plot: x goles, prob(x) para los dos equipos 
  prob_goles <- ggplot(data=tst, aes(x=as.factor(index), y = prob, color = equipo, group = equipo)) +
                  geom_line() + 
                  geom_point() +
                  labs(title = "Probabilidad estimada de marcar", x = "Goles", color = "Equipo") +
                  theme_minimal()
  
  # tst-i son variables auxiliares para estimar la probabilidad de los cruces, ej: marcador 3 - 1 
  tst1 <- tibble(prob = c(dpois(0:5, goles_esperados)), 
                 team = c(rep(equipo1, 6)),
                 index = 0:5)
  
  tst2 <- tibble(prob = c(dpois(0:5, goles_esperados_rival)), 
                 team = c(rep(equipo2, 6)),
                 index = 0:5)
  
  # Cálcula los cruces que mencionaba, ej: para Pr(3 - 1) == Pr(equipo1 marca 3) * Pr(equipo2 marca 1) 
  tst3 <- merge(x = tst1, y = tst2, by = NULL) %>% 
    mutate(prob = prob.x*prob.y)
  
  # Plot de marcadores con sus respectivas probabilidades 
  prob_marcador <- ggplot(tst3, aes(x = as.factor(index.x), y = as.factor(index.y), fill=prob)) +
                      geom_tile() +
                      theme_minimal() +
                      geom_text(aes(label = scales::percent(prob)), color = 'black') +
                      scale_fill_distiller(palette = "Blues", direction = +1, labels = scales::percent) +
                      labs(title = "Posibles resultados",
                      x = "Goles equipo1",
                      y = "Goles equipo2")
  
  # Vamos a tirar la moneda muchas veces a ver cuantos goles hace cada equipo 
  goles <- c(0:7)
  
  # Tira la moneda 
  # goles : de 0 a 7
  # prob : vector de probabilidades calculado previamente en tst con la dist de Poisson 
  marcador_equipo1 <- sample(goles, size = n, p = tst$prob[1:8], replace = TRUE) 
  marcador_equipo2 <- sample(goles, size = n, p = tst$prob[9:16], replace = TRUE) 
  
  # Recogiendo los datos y calculando las frecuencias 
  
  # No. de veces que gano el equipo1 
  victoria <- sum(marcador_equipo1 > marcador_equipo2)
  
  # No. de veces que empataron 
  empate <- sum(marcador_equipo1 == marcador_equipo2)
  
  # No. de veces que perdio equipo1, o también gano el equipo2 
  derrota <- sum(marcador_equipo1 < marcador_equipo2)
  
  # Cálculos de las frecuencias para victoria, empate, derrota
  prob_victoria = victoria / n
  prob_empate = empate / n 
  prob_derrota = derrota / n 
  
  # SALIDA COMPLETA CON GRÁFICAS 
  # return(list(tibble('Prob_Victoria' = prob_victoria, 'Prob_Empate' = prob_empate, 'Prob_Derrota' = prob_derrota), prob_goles, prob_marcador))
  
  ## SALIDA SI ES QUE QUIERES SIMULAR GRUPOS 
  return(tibble('Prob_Victoria' = prob_victoria, 'Prob_Empate' = prob_empate, 'Prob_Derrota' = prob_derrota))
  
}

# Ejemplo, usando la funcion partido()
equipo1 = "Brazil"
equipo2 = "Spain"
n = 1000000

partido(equipo1, equipo2, n)

# FUNCIÓN QUE CÁLCULA VECTOR DE PROBABILIDAD PARA PUNTOS OBTENIDOS EN EL GRUPOX 
# Esto es, tiene en cuenta todas las combinaciones posibles 
# Ej: Obtener 3 puntos ---> Pudo empatar los tres partidos o haber ganado uno y perdido los otros dos, lo hace desde 0 puntos hasta 9 puntos 
prob_points <- function(prob1, prob2, prob3) {
  prob <- c(prob1$Prob_Derrota * prob2$Prob_Derrota * prob3$Prob_Derrota, 
            prob1$Prob_Empate * prob2$Prob_Derrota * prob3$Prob_Derrota + prob1$Prob_Derrota * prob2$Prob_Empate * prob3$Prob_Derrota + prob1$Prob_Derrota * prob2$Prob_Derrota * prob3$Prob_Empate, 
            prob1$Prob_Empate * prob2$Prob_Empate * prob3$Prob_Derrota + prob1$Prob_Derrota * prob2$Prob_Empate * prob3$Prob_Empate + prob1$Prob_Empate * prob2$Prob_Derrota * prob3$Prob_Empate, 
            prob1$Prob_Empate * prob2$Prob_Empate * prob3$Prob_Empate + prob1$Prob_Victoria * prob2$Prob_Derrota * prob3$Prob_Derrota + prob1$Prob_Derrota * prob2$Prob_Victoria * prob3$Prob_Derrota + prob1$Prob_Derrota * prob2$Prob_Derrota * prob3$Prob_Victoria,
            prob1$Prob_Victoria * prob2$Prob_Empate * prob3$Prob_Derrota + prob1$Prob_Victoria * prob2$Prob_Derrota * prob3$Prob_Empate + prob1$Prob_Empate * prob2$Prob_Victoria * prob3$Prob_Derrota + prob1$Prob_Derrota * prob2$Prob_Victoria * prob3$Prob_Empate + prob1$Prob_Empate * prob2$Prob_Derrota * prob3$Prob_Victoria + prob1$Prob_Derrota * prob2$Prob_Empate * prob3$Prob_Victoria,
            prob1$Prob_Victoria * prob2$Prob_Empate * prob3$Prob_Empate + prob1$Prob_Empate * prob2$Prob_Victoria * prob3$Prob_Empate + prob1$Prob_Empate * prob2$Prob_Empate * prob3$Prob_Victoria,
            prob1$Prob_Victoria * prob2$Prob_Victoria * prob3$Prob_Derrota + prob1$Prob_Derrota * prob2$Prob_Victoria * prob3$Prob_Victoria + prob1$Prob_Victoria * prob2$Prob_Derrota * prob3$Prob_Victoria, 
            prob1$Prob_Victoria * prob2$Prob_Victoria * prob3$Prob_Empate + prob1$Prob_Empate * prob2$Prob_Victoria * prob3$Prob_Victoria + prob1$Prob_Victoria * prob2$Prob_Empate * prob3$Prob_Victoria,
            0, 
            prob1$Prob_Victoria * prob2$Prob_Victoria * prob3$Prob_Victoria)
  print(length(prob))
  return(prob)
}

# FUNCIÓN AUX PARA CÁLCULAR LA MODA
mode <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}

# FUNCIÓN PARA SIMULAR LA RONDA 1 (GRUPOS)
# k : No. del grupo que se desea simular 
# n : No. de veces que se simula cada partido 
ronda1 <- function (k,n) {
  
  # Extrae del vector equipos los integrantes del grupo, los guarda en Grupo 
  if (k != 1) {
    Grupo <- tibble('Equipo' = equipos[(4*k-3):(4*k)], 'Puntos' = c(0, 0, 0, 0), 'Prob' = c(0, 0, 0, 0))
  } else {
    Grupo <- tibble('Equipo' = equipos[k:(k+3)], 'Puntos' = c(0, 0, 0, 0), 'Prob' = c(0, 0, 0, 0))
  }
  
  # Recorrido por cada integrante del grupo
  for (i in 1:4) {
    
    # Simula los partidos del equipo 1 
    if (i == 1) {
      
      # prob_i llama a la función partido y devuelve vector con prob de ganar, empatar o perder 
      prob1 <- partido(Grupo$Equipo[i], Grupo$Equipo[2], n)
      prob2 <- partido(Grupo$Equipo[i], Grupo$Equipo[3], n)
      prob3 <- partido(Grupo$Equipo[i], Grupo$Equipo[4], n)
      
      # Usa la función prob_points para el vector de prob ligados a los puntos obtenidos en los tres partidos 
      prob <- prob_points(prob1, prob2, prob3)
      # Se puede obtener desde 0 hasta 9 puntos ---> *prob(8) = 0
      points <- 0:9
      
      # Tira la moneda n veces tambien 
      fr_points <- sample(points, size = n , prob = prob, replace = T)
      
      # Extrae el valor mas repetido, los guarda en Puntos 
      Grupo$Puntos[i] <- mode(fr_points)
      
      # Cálcula la frecuencia de sumar 4 puntos o más
      Grupo$Prob[i] <- sum(fr_points >= 4) / n
      
      # Output de visualización de las probabilidades -- frecuencias 
      print("Probabilidades para Equipo1")
      print(table(fr_points) / n)
    }
    
    # Lo mismo para el equipo 2 
    if (i == 2) {
      prob1 <- partido(Grupo$Equipo[i], Grupo$Equipo[1], n)
      prob2 <- partido(Grupo$Equipo[i], Grupo$Equipo[3], n)
      prob3 <- partido(Grupo$Equipo[i], Grupo$Equipo[4], n)
        
      prob <- prob_points(prob1, prob2, prob3)
      points <- 0:9
        
      fr_points <- sample(points, size = n , prob = prob, replace = T)
      Grupo$Puntos[i] <- mode(fr_points)
      Grupo$Prob[i] <- sum(fr_points >= 4) / n
      print("Probabilidades para Equipo2")
      print(table(fr_points) / n)
    }
    
    # Lo mismo pero para el equipo 3   
    if (i == 3) {
      prob1 <- partido(Grupo$Equipo[i], Grupo$Equipo[1], n)
      prob2 <- partido(Grupo$Equipo[i], Grupo$Equipo[2], n)
      prob3 <- partido(Grupo$Equipo[i], Grupo$Equipo[4], n)
        
      prob <- prob_points(prob1, prob2, prob3)
      points <- 0:9
        
      fr_points <- sample(points, size = n , prob = prob, replace = T)
      Grupo$Puntos[i] <- mode(fr_points)
      Grupo$Prob[i] <- sum(fr_points >= 4) / n
      print("Probabilidades para Equipo3")
      print(table(fr_points) / n)
    }
    
    #Lo mismo para el último equipo 
    if (i == 4) {
      prob1 <- partido(Grupo$Equipo[i], Grupo$Equipo[1], n)
      prob2 <- partido(Grupo$Equipo[i], Grupo$Equipo[2], n)
      prob3 <- partido(Grupo$Equipo[i], Grupo$Equipo[3], n)
        
      prob <- prob_points(prob1, prob2, prob3)
      points <- 0:9
        
      fr_points <- sample(points, size = n , prob = prob, replace = T)
      Grupo$Puntos[i] <- mode(fr_points)
      Grupo$Prob[i] <- sum(fr_points >= 4) / n
      print("Probabilidades para Equipo4")
      print(table(fr_points) / n)
    }
  }
  
  # Ordena el grupo para quién obtuvo más puntos 
  # Grupo <- Grupo[order(Grupo$Puntos, decreasing = T),]
  
  # Output : DataFrame del Grupo con Puntos y Prob
  return(Grupo)
  
}

# Ej usando la función ronda1() 
n = 1000000
ronda1(8,n)

###--------------------------------------------------------------------------###
#-----------------------SIMULACIÓN DE LA PRIMERA RONDA-------------------------#
###--------------------------------------------------------------------------###

n = 1000000
for (i in 1:8) {
  ronda1(i,n)
}
