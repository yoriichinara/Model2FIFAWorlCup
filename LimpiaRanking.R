setwd("/home/yoriichinara/Documents/Probability/FinalProyect/Entrega2")
pkgs <- c("ggplot2", "purrr", "tidyr", "MASS", "magrittr", "AER", "dplyr", "readODS")
invisible(lapply(pkgs, require, character.only = TRUE))

df2022_1 <- read_ods('RankingELO.ods', 
                     sheet = 1, 
                     col_names = T)

df2022_2 <- read_ods('RankingELO.ods', 
                     sheet = 2, 
                     col_names = T)

df2022_3 <- read_ods('RankingELO.ods', 
                     sheet = 3, 
                     col_names = T)

df2022_4 <- read_ods('RankingELO.ods', 
                     sheet = 4, 
                     col_names = T)

df2022_5 <- read_ods('RankingELO.ods', 
                     sheet = 5, 
                     col_names = T)

df2022_6 <- read_ods('RankingELO.ods', 
                     sheet = 6, 
                     col_names = T)

equipos <- c("Qatar", "Ecuador", "Senegal", "Netherlands", "England", "Iran", "United States", "Wales", "Argentina", "Saudi Arabia", "Mexico", "Poland", "France", "Australia", "Denmark", "Tunisia", "Spain", "Costa Rica", "Germany", "Japan", "Belgium", "Canada", "Morocco", "Croatia", "Brazil", "Serbia", "Switzerland", "Cameroon", "Portugal", "Ghana", "Uruguay", "South Korea")

df2022_1 <- df2022_1 %>%
  filter(team %in% equipos) 

df2022_2 <- df2022_2 %>%
  filter(team %in% equipos) 

df2022_3 <- df2022_3 %>%
  filter(team %in% equipos) 

df2022_4 <- df2022_4 %>%
  filter(team %in% equipos) 

df2022_5 <- df2022_5 %>%
  filter(team %in% equipos)

df2022_6 <- df2022_6 %>%
  filter(team %in% equipos)

df2022_1 <- df2022_1[order(df2022_1$team), ]
df2022_2 <- df2022_2[order(df2022_2$team), ]
df2022_3 <- df2022_3[order(df2022_3$team), ]
df2022_4 <- df2022_4[order(df2022_4$team), ]
df2022_5 <- df2022_5[order(df2022_5$team), ]
df2022_6 <- df2022_6[order(df2022_6$team), ]


df <- cbind(df2022_1[1], df2022_1[3], df2022_2[3], df2022_3[3], df2022_4[3], df2022_5[3], df2022_6[3]) 

colnames(df) <- c('team', '2017', '2018', '2019', '2020', '2021', '2022')

df <- df %>% select(team, '1', '2', '3', '4', '5', '6', '7', '8', '9', '10')

for (i in 2:11) {
  df[i] <- df[i] / max(df[i])
}

df <- cbind(df, 'Media' = rowMeans(df[, 2:11])) 
