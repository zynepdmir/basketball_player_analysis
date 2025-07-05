##################### DATA CLEANING ##########################
players_data <- read.csv("C:/Users/zynep/OneDrive/Desktop/stat493/players_3_seasons.csv",
                         fileEncoding = "latin1")
actions_data <- read.csv("C:/Users/zynep/OneDrive/Desktop/stat493/actions_3_seasons.csv",
                         fileEncoding = "latin1")

# cleaning players data
library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(glmnet)
library(corrplot)
library(lmtest)
library(car)

#summary(players_data)
#dim(players_data)

# drop the useless variables in actions_data
useless_vars_actions <- c('x','y','clock','shotClock','officialId','x2','y2','linkDetail',
                          'linkDetailLeague','linkDetailMatch','linkDetailTeam','TVName',
                          'scoreboardName','nickName','linkDetailPerson','leagueName', 
                          "teamExternalId","matchExternalId","personExternalId","shirtNumber",
                          'value','leagueId','timeActual', "internationalFirstName", 
                          "internationalFamilyName")

actions_data <- select(actions_data, -all_of(useless_vars_actions))

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

# combine first name and family name in actions_data
actions_data <- actions_data %>%
  mutate(personName = str_c(firstName, familyName, sep = " "))

# drop firstName and familyName
actions_data <- select(actions_data, -all_of(c("firstName", "familyName")))

# convert character type to factor type in players_data
cat_vars_players <- c("Years", "matchId", "personId", "teamId", "playingPosition", "participated",
                       "teamName", "personName")

# convert character type to factor type in actions_data
cat_vars_actions <- c("Years", "matchId", "periodType", "teamId", "personId", "actionType", "subType",
                      "qualifiers", "area", "side", "playersTeam1", "playersTeam2", "teamName",
                      "personName")

players_data[cat_vars_players] <- lapply(players_data[cat_vars_players], as.factor)
actions_data[cat_vars_actions] <- lapply(actions_data[cat_vars_actions], as.factor)

table(players_data$Years)

# choose 2020-2021 for now because its the newest season
players_data <- players_data[players_data$Year == "2020-2021",]
actions_data <- actions_data[actions_data$Years == "2020-2021",]

# in players data prepare isStarter variable for analysis, make NA's zero
players_data$isStarter[is.na(players_data$isStarter)] <- 0

# do the same for success variable in actions data
actions_data$success[is.na(actions_data$success)] <- 0

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

# Apply cleaning to the actions_data
actions_data <- actions_data %>%
  mutate(across(where(is.character), replace_turkish_chars))

# select only regular actions 
actions_data <- actions_data[actions_data$periodType == "REGULAR",]

# drop the useless variables in players_data again
useless_vars_players2 <- c("Years", "playingPosition" ,"participated", "sStealsPercentage", "sPER", "sAssistsDefensive",
                           "sFoulsCoachDisqualifying", "sFoulsCoachTechnical", "sDefensiveRating","sReboundsPercentage",
                           "firstName" ,"TVName", "scoreboardName","nickName","website" , "internationalReference" )

players_data <- select(players_data, -all_of(useless_vars_players2))
dim(players_data)

########### END OF THE DATA CLEANING ########################

dim(players_data)
dim(actions_data)

length(unique(players_data$Years))
# there's 16 teams in the last season

length(unique(players_data$personId))
# 282 player 

players_data %>%
  group_by(teamId) %>%
  summarise(team_size = n_distinct(personId))
# 14-20 players in each team

# önce datasetlere ayrı ayrı bak, değişken ele,
# missingness imputation yap, sonra datasetleri birleştir


length(unique(actions_data$matchId))
#259
length(unique(players_data$matchId))
#259

players_data %>%
  group_by(teamId) %>%
  summarise(match_size = n_distinct(matchId))

########### ACTIONS DATA ###########

length(unique(actions_data$matchId)) # 259 maç
dim(actions_data[actions_data$actionNumber == 1, ]) # 257 tane aksiyon seti
dim(actions_data[actions_data$actionType == "game", ])
sum(actions_data[actions_data$actionNumber == 1,]$score1) # score yok
sum(actions_data[actions_data$actionNumber == 1,]$score2) # score yok
actions_data[actions_data$actionNumber == 1,]
# aksiyon numarası 1 ise aksiyon tipi = game
# subType = start
# teamId-personId-qualifiers-prevAction-area-side-score1-score2-teamName-personName = NA
# period = 1
# success = 1 (ortada bi başarı yok zaten)
# periodType = REGULAR

dim(actions_data[actions_data$actionType == "period", ]) # tipi period olan 2092 aksiyon var
actions_data[actions_data$actionNumber == 2,]
sum(actions_data[actions_data$actionNumber == 2,]$score1) # score yok
sum(actions_data[actions_data$actionNumber == 2,]$score2) # score yok
nrow(actions_data[actions_data$actionNumber == 2,]) # 259 tane aksiyon numarası 2 olan aksiyon var
# aksiyon numarası 2 ise aksiyon tipi = period
# subType =start 
# teamId-personId-qualifiers-prevAction-area-side-score1-score2-teamName-personName = NA
# period = 1
# success = 1 (başarı söz konusu değil)
# periodType = REGULAR

actions_data[actions_data$actionNumber == 3,]
sum(actions_data[actions_data$actionNumber == 3,]$score1) # score yok
sum(actions_data[actions_data$actionNumber == 3,]$score2) # score yok
dim(actions_data[actions_data$actionType == "clock", ]) # 46794 tane clock var
# aksiyon numarası 3 ise aksiyon tipi = clock
# subType = start 
# teamId-personId-qualifiers-prevAction-area-side-score1-score2-teamName-personName = NA
# period = 1
# success = 1
# periodType = REGULAR


sum(actions_data[actions_data$actionNumber == 4,]$score1) # score yok
sum(actions_data[actions_data$actionNumber == 4,]$score2) # score yok
actions_data[actions_data$actionNumber == 4,]
dim(actions_data[actions_data$actionType == "jumpball", ]) # 907 tane jumpball var
# aksiyon numarası 4 ise aksiyon tipi = jumpball
# subType = startperiod
# teamId-personId-qualifiers-prevAction-area-side-score1-score2-teamName-personName = NA
# period = 1
# success = 1
# periodType = REGULAR

sum(actions_data[actions_data$actionNumber == 5,]$score1) # score yok
sum(actions_data[actions_data$actionNumber == 5,]$score2) # score yok
actions_data[actions_data$actionNumber == 5,]
dim(actions_data[actions_data$actionType == "jumpball", ]) # 907 tane jumpball var
# aksiyon numarası 5 ise aksiyon tipi = jumpball ya da clock
# aksiyon tipi jumpball ise subtype won ve tem-person belli, prevAction = 4
# aksiyon tipi clock ise subtype stop ve team-person NA, prevAction = NA,
# subType = won ya da stop
# qualifiers-area-score1-score2 = NA
# period = 1
# success = 1
# periodType = REGULAR

sum(actions_data[actions_data$actionNumber == 6,]$score1) # score yok
sum(actions_data[actions_data$actionNumber == 6,]$score2) # score yok
actions_data[actions_data$actionNumber == 6,]
dim(actions_data[actions_data$actionType == "jumpball", ]) # 907 tane jumpball var
# aksiyon numarası 6 ise aksiyon tipi = jumpball ya da clock
# aksiyon tipi jumpball ise subtype = lost ya da won ve tem-person belli, prevAction = 4
# aksiyon tipi clock ise subtype stop ve team-person NA, prevAction = NA,
# subType = won ya da stop
# qualifiers-area-score1-score2 = NA
# period = 1
# success = 1
# periodType = REGULAR


# actionTypes :
# game = başlangıç ya da bitiş
# hakem topu havaya atar ve iki takımdan birer kişi yakalamaya çalışır
# jumpball yapan oyuncuların hangileri kazandı hangileri kaybetti?
# hangi oyuncular jumpball yapınca kazanma olasılığı yüksek?
# veya jumpball'da iyi olan ilk 10 oyuncu hangileri?
unique(actions_data$personName) # 282 oyuncu var

length(actions_data[actions_data$actionType == "jumpball" & actions_data$subType == "won", ]$personName)
# 259 tanesi jumpball'da başarılı olmuş, 23 tanesi başarısız olmuş

length(actions_data[actions_data$actionType == "jumpball" & actions_data$subType == "lost", ]$personName)

unique(actions_data$periodType)

# aksiyonlara ayır actions_datayı ve her bir oyuncu ne kadar score sağlamış göster
length(unique(actions_data$playersTeam1))

# I struggled with how to interpret the NA's

#playersTeam1-2 kullanarak hangi 5li kombinasyonlarda takım başarı sağlamış bakabilirim
# actionType = clock veya timeout olan aksiyonların takımları oyuncuları NA çünkü

# successi en çok olan ilk 10 oyuncu - son 10 oyuncu listesi
# bu oyuncular hangi aksiyon tiplerini yapmış, actionType ve succes ilişkisi var mı
# hangi oyuncuların attemptleri başarılı olmuş
# defens tarafında hangi oyuncular başarılı hangileri başarısız vs

# 2 pt attempt yapıp da success==1 olan oyuncular hangileri - frekansa göre sıralama
# how many attempts did the players make?
actions_data %>%
  filter(actionType == "2pt") %>%
  group_by(personId) %>%
  summarise(attempt = n()) %>%
  summarise(
    min = min(attempt),
    max = max(attempt),
    mean = mean(attempt),
    median = median(attempt),
    sd = sd(attempt)
  )

# which players are more successful with 2 point attempt,
# among the players who attempted at least as the median of the total attempts?
two_pt_prop <- actions_data %>%
  filter(actionType == "2pt") %>%
  group_by(personName) %>%
  summarise(
    attempt = n(),
    success = sum(success == 1),
    prop = success / attempt
  )

two_pt_median <- median(two_pt_prop$attempt)

two_pt_prop %>%
  filter(attempt > two_pt_median) %>%
  arrange(desc(prop))

# which players are more successful with 3 point attempt,
# among the players who attempted at least as the median of the total attempts?
three_pt_prop <- actions_data %>%
  filter(actionType == "3pt") %>%
  group_by(personName) %>%
  summarise(
    attempt = n(),
    success = sum(success == 1),
    prop = success / attempt
  )

three_pt_median <- median(two_pt_prop$attempt)

three_pt_prop %>%
  filter(attempt > two_pt_median) %>%
  arrange(desc(prop))

# which players' 2 points attempts were the least successful?
two_pt_prop %>%
  filter(attempt > two_pt_median) %>%
  arrange(prop)

# which player' 3 points attempts were the least successful?
three_pt_prop %>%
  filter(attempt > three_pt_median) %>%
  arrange(prop)


unique(actions_data$actionType)
actions_data[actions_data$actionType == "foul" & actions_data$success == 0,] 
actions_data[actions_data$actionType == "timeout" & actions_data$success == 0,] 
# success variable is useful for only 2pt, 3pt and freethrow action types => prob variable
# freethrow = attempts to score 1 points on the line when there's no defense (after opposite team's foul)
# 2pt = attempts close to the basket
# 3pt = attempts far from the basket


#### DEFINE VARIABLES ####
# two_pt_success INDEPENDENT VARIABLE
df_2pt <- actions_data %>%
  filter(actionType == "2pt" & !is.na(personName)) %>%
  group_by(personName, matchId) %>%
  summarise(
    total_2pt_game = n(),
    successful_2pt_game = sum(success == 1),
    .groups = "drop"
  ) %>%
  group_by(personName) %>%
  summarise(
    total_2pt = mean(total_2pt_game),
    successful_2pt = mean(successful_2pt_game),
    success_rate_2pt = successful_2pt / total_2pt,
    .groups = "drop"
  )

# three_pt_success INDEPENDENT VARIABLE
 df_3pt <- actions_data %>%
  filter(actionType == "3pt" & !is.na(personName)) %>%
  group_by(personName, matchId) %>%
  summarise(
    total_3pt_game = n(),
    successful_3pt_game = sum(success == 1),
    .groups = "drop"
  ) %>%
   group_by(personName) %>%
   summarise(
     total_3pt = mean(total_3pt_game),
     successful_3pt = mean(successful_3pt_game),
     success_rate_3pt = successful_3pt/total_3pt
   )
 
# freethrow_success INDEPENDENT VARIABLE
df_freethrow <- actions_data %>%
  filter(actionType == "freethrow" & !is.na(personName)) %>%
  group_by(personName, matchId) %>%
  summarise(
    total_freethrow_game = n(),
    successful_freethrow_game = sum(success == 1),
    .groups = "drop"
  ) %>%
  group_by(personName) %>%
  summarise(
    total_freethrow = mean(total_freethrow_game),
    successful_freethrow = mean(successful_freethrow_game),
    success_rate_freethrow = successful_freethrow/total_freethrow
  )

# foul, foulon, rebound, turnover, steal, assist, block => rate variables
# foul = the player who makes foul
# foulon = the player whom opposite team make foul to
# rebound = the player who gets the ball when someone else's shot is unsuccessful
# If that someone is from his team, then it is offensive rebound
# If that someone is from the opposite team, then it is defensive rebound
# turnover = losing the ball 
# steal = the player who got the ball from the opposite team
# assist = the player who helps his teammate to score
# block = the player who block the opposite team's shot

# foul count INDEPENDENT VARIABLE
df_foul <- actions_data %>%
  filter(actionType == "foul" & !is.na(personName)) %>%
  group_by(personName, matchId) %>%
  summarise(
    foul_count_game = n(),
    .groups = "drop"
    ) %>%
  group_by(personName) %>%
  summarise(
    foul_count = mean(foul_count_game)
  )
  

#actions_data[actions_data$actionType == "foul" & is.na(actions_data$personId),]
# subType = "benchTechnical" => not the player but the team made the faul
# that's why we drop the NA's 

# foulon count INDEPENDENT VARIABLE
df_foulon <- actions_data %>%
  filter(actionType == "foulon" ) %>%
  group_by(personName, matchId) %>%
  summarise(
    foulon_count_game = n(),
    .groups = "drop"
    ) %>%
  group_by(personName) %>%
  summarise(
    foulon_count = mean(foulon_count_game)
  )

# offensive rebound count INDEPENDENT VARIABLE 
# players being unknown mean that the rebound is not made by only one player in the team,
# so drop the NA's here since we won't use them in player-base analysis
df_rebound_offensive <- actions_data %>%
  filter(actionType == "rebound" & subType == "offensive" & !is.na(personName)) %>%
  group_by(personName, matchId) %>%
  summarise(offensive_rebound_count_game = n(), .groups = "drop") %>%
  group_by(personName) %>%
  summarise(
    offensive_rebound_count = mean(offensive_rebound_count_game)
  )

# defensive rebound count INDEPENDENT VARIABLE 
# drop the NA's here too
df_rebound_defensive <- actions_data %>%
  filter(actionType == "rebound" & subType == "defensive" & !is.na(personName)) %>%
  group_by(personName, matchId) %>%
  summarise(defensive_rebound_count_game = n(), .groups = "drop") %>%
  group_by(personName) %>%
  summarise(
    defensive_rebound_count = mean(defensive_rebound_count_game)
  )

# turnover count INDEPENDENT VARIABLE
df_turnover <- actions_data %>%
  filter(actionType == "turnover" & !is.na(personName)) %>%
  group_by(personName, matchId) %>%
  summarise(turnover_count_game = n(), .groups = "drop") %>%
  group_by(personName) %>%
  summarise(
    turnover_count = mean(turnover_count_game)
  )
# personName = NA means that the turnover is not caused by a player. So drop NA's again

# steal count INDEPENDENT VARIABLE
df_steal <- actions_data %>%
  filter(actionType == "steal" & !is.na(personName)) %>%
  group_by(personName, matchId) %>%
  summarise(steal_count_game = n(), .groups = "drop") %>%
  group_by(personName) %>%
  summarise(steal_count = mean(steal_count_game))

# assist count INDEPENDENT VARIABLE
df_assist <- actions_data %>%
  filter(actionType == "assist" ) %>%
  group_by(personName, matchId) %>%
  summarise(assist_count_game = n(), .groups = "drop") %>%
  group_by(personName) %>%
  summarise(assist_count = mean(assist_count_game))
  

# block count INDEPENDENT VARIABLE
df_block <- actions_data %>%
  filter(actionType == "block") %>%
  group_by(personName, matchId) %>%
  summarise(block_count_game = n(), .groups = "drop") %>%
  group_by(personName) %>%
  summarise(block_count = mean(block_count_game))

# score_up per match DEPENDENT VARIABLE
df_score_up <- actions_data %>%
  arrange(matchId, row_number()) %>%
  mutate(
    prev_score1 = lag(score1),
    prev_score2 = lag(score2),
    prev_personName = lag(personName),
    score1_diff = score1 - prev_score1,
    score2_diff = score2 - prev_score2,
    score_up = (score1_diff > 0 | score2_diff > 0)
  ) %>%
  filter(score_up) %>%
  mutate(
    personName = case_when(
      !is.na(personName) ~ personName,
      actionType == "clock" & !is.na(prev_personName) ~ prev_personName,
      TRUE ~ NA_character_
    ),
    score_count = pmax(score1_diff, score2_diff, 0, na.rm = TRUE)
  ) %>%
  filter(!is.na(personName)) %>%
  group_by(personName, matchId) %>%
  summarise(score_total = sum(score_count), .groups = "drop") %>%
  group_by(personName) %>%
  summarise(mean_score_up = mean(score_total), .groups = "drop") %>%
  right_join(
    actions_data %>% distinct( personName),
    by = "personName"
  ) %>%
  mutate(mean_score_up = replace_na(mean_score_up, 0)) %>%
  filter(!is.na(personName))

#### COMBINE VARIABLES ####
# combine dependent variable and independent variables in one dataframe
# dependent variable: score_up_per_game
# independent variables: action counts for different action types
# combine df_2pt, df_3pt, df_freethrow, df_foul, df_foulon, df_rebound_offensive,
# df_rebound_defensive, df_turnover, df_steal, df_assist, df_block, df_score_up
all_players <- actions_data %>%
  filter(!is.na(personName) ) %>%
  distinct(personId, personName)


# If the player's some action count or rate is NA, then that player didn't make that action at all.

# combine all
df_players <- all_players %>%
  left_join(df_2pt, by = "personName") %>%
  left_join(df_3pt, by = "personName") %>%
  left_join(df_freethrow, by = "personName") %>%
  left_join(df_foul, by = "personName") %>%
  left_join(df_foulon, by = "personName") %>%
  left_join(df_rebound_offensive, by = "personName") %>%
  left_join(df_rebound_defensive, by = "personName") %>%
  left_join(df_turnover, by = "personName") %>%
  left_join(df_steal, by = "personName") %>%
  left_join(df_assist, by = "personName") %>%
  left_join(df_block, by = "personName") %>%
  left_join(df_score_up, by = "personName") 

#summary(df_players)

# filling the NA's with zeros. So that If Ahmet didn't assist, then his assist count is zero.
# or if Mehmet didn't attempt any 2pt shot, then his 2pt shot success rate is zero.
players_df_clean <- df_players %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

#### PERFORMANCE SCORE (METRIC) ####
# finding the weights of performance metric 
# so that it has the highest correlation with mean_score_up
set.seed(42)
actions <- c("total_2pt", "total_3pt", "total_freethrow", "foul_count", 
             "foulon_count", "offensive_rebound_count", "defensive_rebound_count", 
             "assist_count", "steal_count", "block_count", "turnover_count")
n_trials <- 10000

results <- data.frame(correlation = numeric(n_trials), 
                      matrix(nrow = n_trials, 
                             ncol = length(actions)))

colnames(results)[-1] <- actions

for (i in 1:n_trials) {
  weights <- c(
    runif(1, 1.5, 3),   # total_2pt
    runif(1, 2, 4),     # total_3pt
    runif(1, 1, 2),     # freethrow
    runif(1, -3, -1),   # foul_count
    runif(1, 0, 3),     # foulon_count
    runif(1, 0, 2),     # off rebound
    runif(1, 0, 2),     # def rebound
    runif(1, 1, 3),     # assist
    runif(1, 1, 2),     # steal
    runif(1, 1, 2),     # block
    runif(1, -2, -0.5)  # turnover
  )
  
  score <- rowSums(mapply(`*`, players_df_clean[actions], weights))
  corr <- cor(score, players_df_clean$mean_score_up)
  
  results[i, ] <- c(corr, weights)
}

best <- results[which.max(results$correlation), ]
print(best)

players_df_clean$performance_score <- 
  best$total_2pt * players_df_clean$total_2pt +
  best$total_3pt * players_df_clean$total_3pt +
  best$total_freethrow * players_df_clean$total_freethrow +
  best$foul_count * players_df_clean$foul_count +
  best$foulon_count * players_df_clean$foulon_count +
  best$offensive_rebound_count * players_df_clean$offensive_rebound_count +
  best$defensive_rebound_count * players_df_clean$defensive_rebound_count +
  best$assist_count * players_df_clean$assist_count +
  best$steal_count * players_df_clean$steal_count +
  best$block_count * players_df_clean$block_count +
  best$turnover_count * players_df_clean$turnover_count

cor(players_df_clean$mean_score_up, players_df_clean$performance_score)

  
#### THE LM MODEL ####
# building a model with mean actions to explain mean_score_up
model1 <- lm(mean_score_up ~ total_2pt + success_rate_2pt + total_3pt + 
                     success_rate_3pt + total_freethrow + success_rate_freethrow + 
                     foul_count + foulon_count + offensive_rebound_count + 
                     defensive_rebound_count + turnover_count + steal_count + 
                     assist_count + block_count, data = players_df_clean)


model_step <- step(model1, direction = "both")
summary(model_step)

# checking the assumptions
# normality of the residuals
qqnorm(residuals(model_step))
qqline(residuals(model_step))
shapiro.test(residuals(model_step)) # not normal
# 200+ observations => central limit theorem

# 2. Homoskedasticity
plot(model_step$fitted.values, residuals(model_step), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")
bptest(model_step) # variance is constant

# 3. independence 
dwtest(model_step) # errors are independent

# 4. multicolinearity
vif(model_step) # remove foulon_count


model2 <- lm(mean_score_up ~ total_2pt + success_rate_2pt + total_3pt + 
               success_rate_3pt + total_freethrow  + foul_count  + assist_count, 
             data = players_df_clean)
vif(model2)
summary(model2)
model2_step <- step(model2)
summary(model2_step)

# checking the assumptions again
#### ASSUMPTION GRAPHS ####

# 1. normality
qqnorm(residuals(model2_step))
qqline(residuals(model2_step), col="darkred")
shapiro.test(residuals(model2_step)) # non-normal residuals

# 2. constant variance
plot(model_step$fitted.values, residuals(model2_step), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "darkred")
bptest(model2_step) # constant variance

# 3. independence
dwtest(model2_step) # no autocorrelation

# 4. multicolinearity
vif(model2_step) # no multicolinearity

# the problem is the normality assumption is not satisfied
# then use central limit theorem since there is 282 observations,
# the sample size is big enough to approximate normality

#### EDA GRAPHS ####
# Correlation matrix
numeric_vars1 <- players_df_clean[, c("success_rate_2pt", "success_rate_3pt", "success_rate_freethrow",
                                    "mean_score_up", "performance_score")]
corr_matrix1 <- cor(numeric_vars1)

corrplot(corr_matrix1, method = "color", 
         addCoef.col = "black",
         tl.col = "black",
         main = "",
         mar=c(0,0,1,0),
         type="lower",
         tl.pos ="l")

numeric_vars2 <- players_df_clean[, c("foul_count", "foulon_count", "offensive_rebound_count",
                                      "defensive_rebound_count",  "mean_score_up", "performance_score")]
corr_matrix2 <- cor(numeric_vars2)

corrplot(corr_matrix2, method = "color", 
         addCoef.col = "black",
         tl.col = "black",
         main = "",
         mar=c(0,0,1,0),
         type="lower",
         tl.pos= "l")

numeric_vars3 <- players_df_clean[, c("turnover_count", "steal_count",
                                      "assist_count", "block_count",  "mean_score_up", "performance_score")]
corr_matrix3 <- cor(numeric_vars3)

corrplot(corr_matrix3, method = "color", 
         addCoef.col = "black",
         tl.col = "black",
         main = "",
         mar=c(0,0,1,0),
         type = "lower",
         tl.pos="l")


# Distributions of success rates
par(mfrow = c(1, 1))
hist(players_df_clean$success_rate_2pt, 
     breaks = 20, 
     probability = TRUE, 
     main = "2 Point Success Rate",
     xlab = "2pt success rate", 
     col = "orange", 
     border = "black")

lines(density(players_df_clean$success_rate_2pt), 
      col = "black", 
      lwd = 2)

hist(players_df_clean$success_rate_3pt, 
     breaks = 20, 
     probability = TRUE, 
     main = "3 Point Success Rate",
     xlab = "3pt success rate", 
     col = "darkgreen", 
     border = "black")

lines(density(players_df_clean$success_rate_3pt), 
      col = "black", 
      lwd = 2)

hist(players_df_clean$success_rate_freethrow, 
     breaks = 20, 
     probability = TRUE, 
     main = "Freethrow Success Rate",
     xlab = "freethrow success rate", 
     col = "darkred", 
     border = "black")

lines(density(players_df_clean$success_rate_freethrow), 
      col = "black", 
      lwd = 2)


# Distributions of action counts  
par(mfrow = c(2, 3))
hist(players_df_clean$foul_count, 
     breaks = 20, 
     probability = TRUE, 
     main = "Distribution of Foul Count",
     xlab = "Foul Count", 
     col = "#529985", 
     border = "black")

lines(density(players_df_clean$foul_count), 
      col = "black", 
      lwd = 2)

hist(players_df_clean$foulon_count, 
     breaks = 20, 
     probability = TRUE, 
     main = "Distribution of Foul on Count",
     xlab = "Foul on Count", 
     col = "#C51517", 
     border = "black")

lines(density(players_df_clean$foulon_count), 
      col = "black", 
      lwd = 2)

hist(players_df_clean$turnover_count, 
     breaks = 20, 
     probability = TRUE, 
     main = "Distribution of Turnover Count",
     xlab = "turnover count", 
     col = "#55A154", 
     border = "black")

lines(density(players_df_clean$turnover_count), 
      col = "black", 
      lwd = 2)

hist(players_df_clean$steal_count, 
     breaks = 20, 
     probability = TRUE, 
     main = "Distribution of Steal Count",
     xlab = "steal count", 
     col = "#7C4D79", 
     border = "black")

lines(density(players_df_clean$steal_count), 
      col = "black", 
      lwd = 2)

hist(players_df_clean$assist_count, 
     breaks = 20, 
     probability = TRUE, 
     main = "Distribution of Assist Count",
     xlab = "assist count", 
     col = "orange", 
     border = "black")

lines(density(players_df_clean$assist_count), 
      col = "black", 
      lwd = 2)

hist(players_df_clean$block_count, 
     breaks = 20, 
     probability = TRUE, 
     main = "Distribution of Block Count",
     xlab = "block count", 
     col = "brown", 
     border = "black")

lines(density(players_df_clean$block_count), 
      col = "black", 
      lwd = 2)

# distributions of rebounds 
par(mfrow = c(1,2))
hist(players_df_clean$offensive_rebound_count, 
     breaks = 20, 
     probability = TRUE, 
     main = "",
     xlab = "offensive rebound", 
     col = "#BF4723", 
     border = "black")

lines(density(players_df_clean$offensive_rebound_count), 
      col = "black", 
      lwd = 2)

hist(players_df_clean$defensive_rebound_count, 
     breaks = 20, 
     probability = TRUE, 
     main = "",
     xlab = "defensive rebound", 
     col = "#32769B", 
     border = "black")

lines(density(players_df_clean$defensive_rebound_count), 
      col = "black", 
      lwd = 2)

mtext("Offensive vs Defensive Rebound Density", outer = TRUE, line = -2, cex = 1.5)

# summary of actions
summary(players_df_clean[, c("total_2pt", "total_3pt", "assist_count")])

# scatterplot matrix
pairs(players_df_clean[, c("total_2pt", "total_3pt", "total_freethrow","mean_score_up")])

# target vs predictor scatterplot
# total 2pt score
par(mfrow=c(1,2))
plot(players_df_clean$total_2pt, players_df_clean$mean_score_up,
     xlab = "Total 2pt", ylab = "Mean Score Up",
     main = "Total 2pt vs Mean Score Up")
abline(lm(mean_score_up ~ total_2pt, data = players_df_clean), col = "darkred", lwd=2)

# total 3pt score
plot(players_df_clean$total_3pt, players_df_clean$mean_score_up,
     xlab = "Total 3pt", ylab = "Mean Score Up",
     main = "Total 3pt vs Mean Score Up")
abline(lm(mean_score_up ~ total_3pt, data = players_df_clean), col = "darkred", lwd=2)

# total freethrow
plot(players_df_clean$total_freethrow, players_df_clean$mean_score_up,
     xlab = "Total freethrow", ylab = "Mean Score Up",
     main = "Total Freethrow vs Mean Score")
abline(lm(mean_score_up ~ total_freethrow, data = players_df_clean), col = "darkred", lwd=2)

# foul count
plot(players_df_clean$foul_count, players_df_clean$mean_score_up,
     xlab = "foul", ylab = "Mean Score",
     main = "Foul vs Mean Score Up")
abline(lm(mean_score_up ~ foul_count, data = players_df_clean), col = "darkred", lwd=2)

# turnover count
plot(players_df_clean$turnover_count, players_df_clean$mean_score_up,
     xlab = "turnover", ylab = "Mean Score Up",
     main = "Turnover vs Mean Score Up")
abline(lm(mean_score_up ~ turnover_count, data = players_df_clean), col = "darkred", lwd=2)


players_df_clean$mean_score_up <- round(players_df_clean$mean_score_up,2)
# top 10 players wrt mean score up
players_df_clean[order(-players_df_clean$mean_score_up), c("personName", "mean_score_up")][1:10, ]
  
# bottom 10 players wrt mean score up
players_df_clean[order(players_df_clean$mean_score_up), c("personName", "mean_score_up",
                                                            "total_2pt", "total_3pt", "total_freethrow")][1:10, ]

# performance metric
players_df_clean <- players_df_clean %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

players_df_clean[order(-players_df_clean$performance_score),
                 c("personName", "performance_score", "success_rate_2pt","success_rate_3pt",
                   "success_rate_freethrow", "foul_count")][1:10, ]

library(writexl)
write_xlsx(players_df_clean, "players_table.xlsx")
write_xlsx(best, "best_weights.xlsx")


cor(players_df_clean$mean_score_up, players_df_clean$performance_score)
# performance metric is positively correlated with the data-driven score_up metric


plot(players_df_clean$performance_score, players_df_clean$mean_score_up,
     xlab = "custom",
     ylab = "data-driven",
     main = "Custom vs Data-Driven Performance Score",
     col = "lightblue", pch = 16)
abline(lm(mean_score_up ~ performance_score, data = players_df_clean),
       col = "darkred", lwd = 2)


# we derived custom performance score metric based on actions
# This metric is measures players' performance based on their
# fundamental actions such as shots, assist, steal etc.

# we derived another metric, mean score up, which is the
# mean point a player scores in the matchs he played.

# we found that these two metrics is positively correlated,
# which means that actions can explain a player's contribution to
# his team

# the problem: how can we measure the players' game performance correct?
# Is their performance measured better with only their shots or all of the actions 
# they make in a game?

# while we searched a way to measure the players' performance,
# we found that not only the shots but also the other actions such as turnover,
# rebound and block can show if a player doing good or not. But it is not clear 
# how can we measure these actions, and which actions we should focus on to reflect a player's 
# performance.


#### PLAYERS DATA ####
dim(players_data)

# Are they player
df_starter <- players_data %>%
  group_by(personName) %>%
  summarise(
    total_matches = n(),
    started_matches = sum(isStarter, na.rm = TRUE),
    starter_rate = mean(isStarter, na.rm = TRUE)
  ) %>%
  ungroup()

# how many minutes they played
df_minutes <- players_data %>%
  group_by(personName) %>%
  summarise(total_minutes = sum(sMinutes, na.rm = TRUE)) %>%
  arrange(desc(total_minutes))

# how many point their team lost when they're playing
df_minus <- players_data %>%
  group_by(personName) %>%
  summarise(total_sMinus = sum(sMinus, na.rm = TRUE)) %>%
  arrange(desc(total_sMinus))

# how many points their team scored when they're playing
df_plus <- players_data %>%
  group_by(personName) %>%
  summarise(total_sPlus = sum(sPlus, na.rm = TRUE)) %>%
  arrange(desc(total_sPlus))

players_df_clean2 <- players_df_clean %>%
  left_join(df_minutes, by = "personName") %>%
  left_join(df_minus, by =  "personName") %>%
  left_join(df_plus, by = "personName")

##### THE COMBINED LM MODEL #####
# building a model with mean actions to explain mean_score_up
model3 <- lm(mean_score_up ~ total_2pt + success_rate_2pt + total_3pt + 
               success_rate_3pt + total_freethrow + success_rate_freethrow + 
               foul_count + foulon_count + offensive_rebound_count + 
               defensive_rebound_count + turnover_count + steal_count + 
               assist_count + block_count + total_minutes + total_sPlus +
             total_sMinus,data = players_df_clean2)


model_step3 <- step(model3, direction = "both")
summary(model_step3)












