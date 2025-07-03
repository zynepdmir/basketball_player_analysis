##################### DATA CLEANING ##########################
library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(glmnet)
library(corrplot)
library(lmtest)
library(car)

#### DATA CLEANING FOR PLAYERS DATA ####
players_data <- read.csv("C:/Users/zynep/OneDrive/Desktop/stat493/players_3_seasons.csv",
                         fileEncoding = "latin1")

# drop the useless variables in players_data
useless_vars_players <- c("externalId","linkDetail","linkDetailMatch","linkDetailTeam",
                          "linkDetailPerson","linkDetailLeague","periodNumber","periodType",
                          "DNPReason","isPlayer","isTeamOfficial", "images.photo.S1.size",
                          "images.photo.S1.height","images.photo.S1.width","images.photo.S1.bytes",
                          "images.photo.S1.url","images.photo.T1.size", "images.photo.T1.width", 
                          "images.photo.T1.bytes" ,"images.photo.T1.url", "linkDetailCompetitionPlayer",
                          "images.photo.T1.height","nationalityCodeIOC","nationalityCode","nationality",
                          "personGsId","teamGsId","competitionGsId","matchGsId","sMVPVotes","leagueName",
                          "teamNameInternational","personNameInternational","leagueId","matchExternalId",
                          "competitionExternalId", "teamExternalId","shirtNumber","updated",
                          "competitionId","competitionName", "personExternalId", "familyName",
                          "internationalFirstName", "internationalFamilyName")

players_data <- select(players_data, -all_of(useless_vars_players))

# convert character type to factor type in players_data
cat_vars_players <- c("Years", "matchId", "personId", "teamId", "playingPosition", "participated",
                      "teamName", "personName")


players_data[cat_vars_players] <- lapply(players_data[cat_vars_players], as.factor)


#table(players_data$Years)

# choose 2020-2021 for now because its the newest season
players_data <- players_data[players_data$Year == "2020-2021",]



# in players data prepare isStarter variable for analysis, make NA's zero
players_data$isStarter[is.na(players_data$isStarter)] <- 0


library(stringr)
# Define a named vector of replacements for Turkish characters
replace_turkish_chars <- function(x) {
  x %>%
    str_replace_all("Ç", "C") %>%
    str_replace_all("ç", "c") %>%
    str_replace_all("Ğ", "G") %>%
    str_replace_all("ğ", "g") %>%
    str_replace_all("İ", "I") %>%
    str_replace_all("ı", "i") %>%
    str_replace_all("Ö", "O") %>%
    str_replace_all("ö", "o") %>%
    str_replace_all("Ş", "S") %>%
    str_replace_all("ş", "s") %>%
    str_replace_all("Ü", "U") %>%
    str_replace_all("ü", "u") %>%
    str_trim()
}

# Apply cleaning to the players_data
players_data <- players_data %>%
  mutate(across(where(is.character), replace_turkish_chars))


# drop the useless variables in players_data again
useless_vars_players2 <- c("Years", "playingPosition" ,"participated", "sStealsPercentage", "sPER", "sAssistsDefensive",
                           "sFoulsCoachDisqualifying", "sFoulsCoachTechnical", "sDefensiveRating","sReboundsPercentage",
                           "firstName" ,"TVName", "scoreboardName","nickName","website" , "internationalReference" )

players_data <- select(players_data, -all_of(useless_vars_players2))
#dim(players_data)




#### DATA CLEANING FOR ACTIONS DATA ####
actions_data <- read.csv("C:/Users/zynep/OneDrive/Desktop/stat493/actions_3_seasons.csv",
                         fileEncoding = "latin1")
# drop the useless variables in actions_data
useless_vars_actions <- c('x','y','clock','shotClock','officialId','x2','y2','linkDetail',
                          'linkDetailLeague','linkDetailMatch','linkDetailTeam','TVName',
                          'scoreboardName','nickName','linkDetailPerson','leagueName', 
                          "teamExternalId","matchExternalId","personExternalId","shirtNumber",
                          'value','leagueId','timeActual', "internationalFirstName", 
                          "internationalFamilyName")

actions_data <- select(actions_data, -all_of(useless_vars_actions))

# combine first name and family name in actions_data
actions_data <- actions_data %>%
  mutate(personName = str_c(firstName, familyName, sep = " "))

# drop firstName and familyName
actions_data <- select(actions_data, -all_of(c("firstName", "familyName")))
# convert character type to factor type in actions_data
cat_vars_actions <- c("Years", "matchId", "periodType", "teamId", "personId", "actionType", "subType",
                      "qualifiers", "area", "side", "playersTeam1", "playersTeam2", "teamName",
                      "personName")
actions_data[cat_vars_actions] <- lapply(actions_data[cat_vars_actions], as.factor)
actions_data <- actions_data[actions_data$Years == "2020-2021",]
# do the same for success variable in actions data
actions_data$success[is.na(actions_data$success)] <- 0
# çünkü aslında success ya 1 ya da NA gelmiş, demek ki NA'ler aslında 0

# Define a named vector of replacements for Turkish characters
replace_turkish_chars <- function(x) {
  x %>%
    str_replace_all("Ç", "C") %>%
    str_replace_all("ç", "c") %>%
    str_replace_all("Ğ", "G") %>%
    str_replace_all("ğ", "g") %>%
    str_replace_all("İ", "I") %>%
    str_replace_all("ı", "i") %>%
    str_replace_all("Ö", "O") %>%
    str_replace_all("ö", "o") %>%
    str_replace_all("Ş", "S") %>%
    str_replace_all("ş", "s") %>%
    str_replace_all("Ü", "U") %>%
    str_replace_all("ü", "u") %>%
    str_trim()
}

# Apply cleaning to the actions_data
actions_data <- actions_data %>%
  mutate(across(where(is.character), replace_turkish_chars))

# select only regular actions 
actions_data <- actions_data[actions_data$periodType == "REGULAR",]


########### ACTIONS DATA ###########
# oyuncusu NA olan aksiyonların olmadığı bir dataframe tanımla
actions_data_clean <- actions_data[is.na(actions_data$personName) == 0,]
# 259 maç var, 282 oyuncu var.

# ELMEDIN KIKANOVIC hangi maçta kaç tane başarılı turnover_doubledribble yapmış:
library(dplyr)
library(tidyr)
library(stringr)

players <- unique(actions_data_clean$personName)
match_ids <- unique(actions_data_clean$matchId)

# action_sub oluştur
actions_data_clean <- actions_data_clean %>%
  mutate(
    subType = ifelse(is.na(subType), "NA", subType),
    action_sub = paste0(actionType, "_", subType)
  )

# Tüm olası action_sub’lar
all_action_subs <- actions_data_clean %>%
  filter(success == 1) %>%
  pull(action_sub) %>%
  unique()

# Tüm olası qualifiers'ları çıkar
all_qualifiers <- actions_data_clean %>%
  filter(success == 1, !is.na(qualifiers)) %>%
  pull(qualifiers) %>%
  str_split(";") %>%
  unlist() %>%
  unique()

player_list <- list()

for (player in players) {
  # 1. Başarılı action_sub sayımı
  action_df <- actions_data_clean %>%
    filter(personName == player, success == 1) %>%
    group_by(matchId, action_sub) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = action_sub, values_from = count)
  
  for (col in all_action_subs) {
    if (!(col %in% colnames(action_df))) {
      action_df[[col]] <- 0
    }
  }
  
  # 2. Başarılı qualifiers sayımı
  qual_df <- actions_data_clean %>%
    filter(personName == player, success == 1, !is.na(qualifiers)) %>%
    separate_rows(qualifiers, sep = ";") %>%
    group_by(matchId, qualifiers) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = qualifiers, values_from = count)
  
  for (col in all_qualifiers) {
    if (!(col %in% colnames(qual_df))) {
      qual_df[[col]] <- 0
    }
  }
  
  # 3. Tüm maçlar için boş iskelet
  all_matches <- data.frame(matchId = match_ids)
  
  # 4. Join ve NA'ları 0 ile doldur
  full_df <- all_matches %>%
    left_join(action_df, by = "matchId") %>%
    left_join(qual_df, by = "matchId") %>%
    mutate(across(-matchId, ~replace_na(., 0)))
  
  player_list[[player]] <- full_df
}


#### PLAYERS DATA ####

vars_to_zero_na <- names(players_data)


players_data <- players_data %>%
  mutate(across(all_of(vars_to_zero_na), ~replace_na(., 0)))


### PER (player efficiency rate) ####
players_data <- subset(players_data, 
                       select = c(matchId, isStarter, sReboundsDefensive,
                                  sPoints, sReboundsOffensive, sTurnovers,
                                  sSteals, sFieldGoalsAttempted, sFieldGoalsMade,
                                  sBlocks, sMinutes, sAssists, sFreeThrowsAttempted,
                                  sFreeThrowsMade, sFoulsPersonal, sFoulsOffensive,
                                  personName, sUsagePercentage, sThreePointersMade,
                                  sTwoPointersMade
                                  ))

players_data <- players_data %>%
  mutate(
    missedFG = sFieldGoalsAttempted - sFieldGoalsMade,
    missedFT = sFreeThrowsAttempted - sFreeThrowsMade,
    totalRebounds = sReboundsOffensive + sReboundsDefensive,
    
    PER = (sPoints + sAssists + totalRebounds + sSteals + sBlocks 
                  - missedFG - missedFT - sTurnovers - sFoulsPersonal - sFoulsOffensive) / sMinutes
  )


# weighted mean of matches for players
players_weighted_per <- players_data %>%
  group_by(personName) %>%
  summarise(
    weighted_PER = sum(PER * sMinutes) / sum(sMinutes)
  ) 


#str(players_weighted_per)

players_weighted_per <- players_weighted_per %>%
  mutate(
    PER_class = ifelse(weighted_PER >= median(weighted_PER, na.rm = TRUE), "High", "Low")
  )


# Oyuncu bazında ortalama (veya toplam) istatistikler alınmalı
# df of indep vars
player_level_data <- players_data %>% # df of indep vars and the outcome
  group_by(personName) %>%
  summarise(
    sPoints = mean(sPoints, na.rm = TRUE),
    sAssists = mean(sAssists, na.rm = TRUE),
    sTurnovers = mean(sTurnovers, na.rm = TRUE),
    sMinutes = mean(sMinutes, na.rm = TRUE),
    sReboundsOffensive = mean(sReboundsOffensive, na.rm = TRUE),
    sReboundsDefensive = mean(sReboundsDefensive, na.rm = TRUE),
    sSteals = mean(sSteals, na.rm = TRUE),
    sBlocks = mean(sBlocks, na.rm = TRUE),
    sUsagePercentage = mean(sUsagePercentage, na.rm = TRUE),
    sFreeThrowsMade = mean(sFreeThrowsMade, na.rm = TRUE),
    sThreePointersMade = mean(sThreePointersMade, na.rm = TRUE),
    sTwoPointersMade = mean(sTwoPointersMade, na.rm = TRUE),
    sFoulsPersonal = mean(sFoulsPersonal, na.rm = TRUE),
    sFoulsOffensive = mean(sFoulsOffensive, na.rm = TRUE),
    sFieldGoalsAttempted = mean(sFieldGoalsAttempted, na.rm = TRUE),
    sFieldGoalsMade = mean(sFieldGoalsMade, na.rm =TRUE),
    sFreeThrowsAttempted = mean(sFreeThrowsAttempted, na.rm = TRUE),
    sFreeThrowsMade = mean(sFreeThrowsMade, na.rm =TRUE)
  ) %>%
  left_join(players_weighted_per, by = "personName")

player_level_data$totalRebounds <- player_level_data$sReboundsDefensive + player_level_data$sReboundsOffensive
player_level_data$missedFG <- player_level_data$sFieldGoalsAttempted - player_level_data$sFieldGoalsMade
player_level_data$missedFT <- player_level_data$sFreeThrowsAttempted - player_level_data$sFreeThrowsMade

  
# correlation plot of weighted_PER
cor_vars <- player_level_data[, c("sPoints", "sAssists", "totalRebounds",
                                  "sSteals", "sBlocks", "missedFG", 
                                  "missedFT", "sTurnovers", "sFoulsPersonal",
                                   "sMinutes", "weighted_PER")]
corr_matrix <- cor(cor_vars)

corrplot(corr_matrix, method = "color", 
         addCoef.col = "black",
         tl.col = "black",
         main = "",
         mar=c(0,0,1,0),
         type="lower",
         tl.pos ="l")

# density of missed actions
hist(player_level_data$missedFG, 
     breaks = 20, 
     probability = TRUE, 
     main = "Missed Free Throw Distribution",
     xlab = "Missed Free Throw", 
     col = "darkgreen", 
     border = "black")

lines(density(player_level_data$missedFG), 
      col = "black", 
      lwd = 2)

#### Decision tree ####

# Gerekli kütüphaneler
library(rpart)
library(rpart.plot)
library(caret)

# 1. Veri setini ayır
set.seed(493)  # Tekrarlanabilirlik için
train_index <- createDataPartition(player_level_data$PER_class, p = 0.7, list = FALSE)

train_data <- player_level_data[train_index, ]
test_data <- player_level_data[-train_index, ]

# 2. Decision tree modelini kur (train setiyle)
dt_model <- rpart(PER_class ~ ., 
                  data = train_data %>% select(-personName, -weighted_PER),
                  method = "class")

# 3. Test setiyle tahmin yap
preds <- predict(dt_model, newdata = test_data, type = "class")

# 4. Accuracy hesapla
conf_mat <- confusionMatrix(preds, as.factor(test_data$PER_class))
accuracy <- conf_mat$overall['Accuracy']
print(accuracy)

# 5. Decision tree grafiği
rpart.plot(dt_model,
           type = 3,              # yes/no ayrımlı kutular
           extra = 101,           # sınıf, olasılık ve yığın bilgisi
           box.palette = "RdYlGn",
           fallen.leaves = TRUE,
           main = "Decision Tree for PER_class")

# feature importance
dt_model$variable.importance

# feature importance plot
library(ggplot2)

# Feature importance'ı dataframe'e çevir
importance_df <- data.frame(
  Feature = names(dt_model$variable.importance),
  Importance = dt_model$variable.importance
)

# Feature'ları önem sırasına göre sırala
importance_df <- importance_df %>%
  arrange(desc(Importance))

# Plot
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance (Decision Tree)",
       x = "Feature",
       y = "Importance") +
  theme_minimal()

# example 1 (low) Ahmet Can Duran
example_player1 <- test_data[1, ]  

predicted_class1 <- predict(dt_model, newdata = example_player1, type = "class")

actual_class1 <- example_player1$PER_class

cat("Player Name:", example_player1$personName, "\n")
cat("Actual PER Class:", actual_class1, "\n")
cat("Predicted PER Class:", predicted_class1, "\n")

print(example_player1 %>% select(sPoints, sAssists, sMinutes, sTurnovers, sFreeThrowsMade, sTwoPointersMade))

# example 2 (high) alperen Şengün
example_player2 <- test_data[5, ]  

predicted_class2 <- predict(dt_model, newdata = example_player2, type = "class")

actual_class2 <- example_player2$PER_class

cat("Player Name:", example_player2$personName, "\n")
cat("Actual PER Class:", actual_class2, "\n")
cat("Predicted PER Class:", predicted_class2, "\n")

print(example_player2 %>% select(sPoints, sAssists, sMinutes, sTurnovers, sFreeThrowsMade, sTwoPointersMade))
