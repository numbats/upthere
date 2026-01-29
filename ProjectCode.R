library(fitzRoy)
library(dplyr)
library(ggplot2)
library(lubridate)

m2022 <- fetch_player_stats(season = 2022, comp = "VFL", source = "AFL")
m2025 <- fetch_player_stats(season = 2025, comp = "AFLM", source = "AFL")

f2022 <- fetch_player_stats(season = 2022, comp = "VFLW", source = "AFL")
f2025 <- fetch_player_stats(season = 2025, comp = "AFLW", source = "AFL")

#mens VFL 2022 
vflm2022 <- m2022 |> 
  group_by(player.player.player.playerId) |>
  summarise_if(is.numeric, mean)
vflmnames <- m2022 |>
  select(player.player.player.playerId,
         player.player.player.givenName,
         player.player.player.surname) |>
  distinct()
vflm2022 <- left_join(vflm2022, vflmnames) |>
  rename(ID = player.player.player.playerId,
         given_name = player.player.player.givenName,
         surname = player.player.player.surname) |>
  select(ID, given_name, surname,
         goals:clearances.totalClearances)
vflm2022

#mens AFL 2025
aflm2025 <- m2025 |> 
  group_by(player.player.player.playerId) |>
  summarise_if(is.numeric, mean)
aflmnames <- m2025 |>
  select(player.player.player.playerId,
         player.player.player.givenName,
         player.player.player.surname) |>
  distinct()
aflm2025 <- left_join(aflm2025, aflmnames) |>
  rename(ID = player.player.player.playerId,
         given_name = player.player.player.givenName,
         surname = player.player.player.surname) |>
  select(ID, given_name, surname,
         goals:clearances.totalClearances)
aflm2025


#womens VFL 2022 
vflw2022 <- f2022 |> 
  group_by(player.player.player.playerId) |>
  summarise_if(is.numeric, mean)
vflwnames <- f2022 |>
  select(player.player.player.playerId,
         player.player.player.givenName,
         player.player.player.surname) |>
  distinct()
vflw2022 <- left_join(vflw2022, vflwnames) |>
  rename(ID = player.player.player.playerId,
         given_name = player.player.player.givenName,
         surname = player.player.player.surname) |>
  select(ID, given_name, surname,
         goals:clearances.totalClearances)
vflw2022

#womens AFL 2025
aflw2025 <- f2025 |> 
  group_by(player.player.player.playerId) |>
  summarise_if(is.numeric, mean)
aflwnames <- f2025 |>
  select(player.player.player.playerId,
         player.player.player.givenName,
         player.player.player.surname) |>
  distinct()
aflw2025 <- left_join(aflw2025, aflwnames) |>
  rename(ID = player.player.player.playerId,
         given_name = player.player.player.givenName,
         surname = player.player.player.surname) |>
  select(ID, given_name, surname,
         goals:clearances.totalClearances)
aflw2025

#playerinfo
aflwinfo <- fetch_player_details(season = 2025, comp = "AFLW", current = FALSE) |> 
  select(providerId, firstName, surname, position, dateOfBirth, heightInCm, debutYear) |> 
  rename(ID = providerId)

vflwinfo <- fetch_player_details(season = 2022, comp = "VFLW", current = FALSE) |> 
  select(providerId, firstName, surname,dateOfBirth, heightInCm) |>
  rename(ID = providerId)

aflminfo <- fetch_player_details(season = 2025, comp = "AFLM", current = FALSE) |>
  select(providerId, firstName, surname, position, dateOfBirth, heightInCm, draftYear, debutYear) |>
  rename(ID = providerId)

vflminfo <- fetch_player_details(season = 2022, comp = "VFL", current = FALSE) |>
  select(providerId, firstName, surname, dateOfBirth, heightInCm, draftYear, debutYear) |>
  rename(ID = providerId)

female <- bind_rows(aflwinfo, vflwinfo) |>
  distinct(ID, .keep_all = TRUE) |> 
  rename(given_name = firstName)

male <- bind_rows(aflminfo, vflminfo) |> 
  distinct(ID, .keep_all = TRUE) |>
  rename(given_name = firstName)


#full datasets 
m<- left_join(vflm2022, aflm2025, 
              by = c("ID", "given_name", "surname")) |> 
  left_join(male, by = c("ID", "given_name", "surname")) |>
  mutate(playedAFL25 = ifelse(!is.na(dreamTeamPoints.y), 1, 0)) |> 
  mutate(dateOfBirth = as.Date(dateOfBirth)) |> 
  mutate(ageVFL = as.numeric(difftime(as.Date("2022-03-25"), dateOfBirth, units = "days")) / 365)
  
  
f <- left_join(vflw2022, aflw2025, 
               by = c("ID", "given_name", "surname")) |> 
  left_join(female, by = c("ID", "given_name", "surname")) |> 
  mutate(playedAFL25 = ifelse(!is.na(dreamTeamPoints.y), 1, 0)) |> 
  mutate(dateOfBirth = as.Date(dateOfBirth)) |> 
  mutate(ageVFL = as.numeric(difftime(as.Date("2022-02-12"), dateOfBirth, units = "days")) / 365)


#comparing goals VFL 2022 vs AFL 2025 
ggplot(m, aes(x = goals.x, y = goals.y)) +
  geom_point() + 
  labs(x = "Goals per VFLM game", 
       y = "Goals per AFLM game")
cor.test(m$goals.x, m$goals.y, use = "complete.obs")

ggplot(f, aes(x = goals.x, y = goals.y)) +
  geom_point() + 
  labs(x = "Goals per VFLW game", 
       y = "Goals per AFLW game")
cor.test(f$goals.x, f$goals.y, use = "complete.obs")


#comparing dream team points VFL 2022 vs AFL 2025
ggplot(m, aes(x = dreamTeamPoints.x, y = dreamTeamPoints.y)) +
  geom_point() + 
  labs(x = "Dream Team Points per VFL game", 
       y = "Dream Team Points per AFL game")
cor.test(m$dreamTeamPoints.x, m$dreamTeamPoints.y, use = "complete.obs")

ggplot(f, aes(x = dreamTeamPoints.x, y = dreamTeamPoints.y)) +
  geom_point() + 
  labs(x = "Dream Team Points per VFLW game", 
       y = "Dream Team Points per AFLW game")
cor.test(f$dreamTeamPoints.x, f$dreamTeamPoints.y, use = "complete.obs")

#comparing disposals VFL 2022 vs AFL 2025
ggplot(m, aes(x = disposals.x, y = disposals.y)) +
  geom_point() + 
  labs(x = "disposals per VFL game", 
       y = "disposals per AFL game")
cor.test(m$disposals.x, m$disposals.y, use = "complete.obs")

ggplot(f, aes(x = disposals.x, y = disposals.y)) +
  geom_point() + 
  labs(x = "disposals per VFLW game", 
       y = "disposals per AFLW game")
cor.test(f$disposals.x, f$disposals.y, use = "complete.obs")



#comp stats, averages per game
#VFLW 
f2022 |> summarise(avg_goals = mean(goals),
             avg_disposals = mean(disposals), 
             avg_dreamteam = mean(dreamTeamPoints), 
             avg_marks = mean(marks), 
             avg_tackles = mean(tackles))

#AFLW
f2025 |> summarise(avg_goals = mean(goals),
               avg_disposals = mean(disposals), 
               avg_dreamteam = mean(dreamTeamPoints), 
               avg_marks = mean(marks), 
               avg_tackles = mean(tackles))

#VFLM
m2022 |> summarise(avg_goals = mean(goals),
               avg_disposals = mean(disposals), 
               avg_dreamteam = mean(dreamTeamPoints), 
               avg_marks = mean(marks), 
               avg_tackles = mean(tackles))

#AFLM 
m2025 |> summarise(avg_goals = mean(goals),
               avg_disposals = mean(disposals), 
               avg_dreamteam = mean(dreamTeamPoints), 
               avg_marks = mean(marks), 
               avg_tackles = mean(tackles))


#comparing VFL comp stats for players selected vs not selected for AFL  
ggplot(m, aes(x = factor(playedAFL25, labels = c("Not Selected", "Selected")), y = disposals.x)) + 
  geom_boxplot() +
  labs(x = "AFLM2025", y = "Disposals per game in VFLM 2022")
  
ggplot(f, aes(x = factor(playedAFL25, labels = c("Not Selected", "Selected")), y = disposals.x)) + 
  geom_boxplot() +
  labs(x = "AFLW2025", y = "Disposals per game in VFLW 2022")

ggplot(m, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = dreamTeamPoints.x)) + 
  geom_boxplot() + 
  labs(x = "AFLM2025", y = "Dream team points per game in VFLM 2022")

ggplot(f, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = dreamTeamPoints.x)) + 
  geom_boxplot() + 
  labs(x = "AFLW2025", y = "Dream team points per game in VFLW 2022")

ggplot(m, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = marks.x)) + 
  geom_boxplot() + 
  labs(x = "AFLM2025", y = "Marks per game in VFLM 2022")

ggplot(f, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = marks.x)) + 
  geom_boxplot() + 
  labs(x = "AFLW2025", y = "Marks per game in VFLW 2022")

ggplot(m, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = tackles.x)) + 
  geom_boxplot() + 
  labs(x = "AFLM2025", y = "Tackles per game in VFLM 2022")

ggplot(f, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = tackles.x)) + 
  geom_boxplot() + 
  labs(x = "AFLW2025", y = "Tackles per game in VFLW 2022")


#comparing demographics for players selected vs not selected for AFL
ggplot(m, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = ageVFL)) + 
  geom_boxplot() + 
  labs(x = "AFLM2025", y = "Age start of VFL season")

ggplot(f, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = ageVFL)) + 
  geom_boxplot() + 
  labs(x = "AFLW2025", y = "Age start of VFLW season")

ggplot(filter(m, heightInCm != 0), aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = heightInCm)) + 
  geom_boxplot() + 
  labs(x = "AFLM2025", y = "Height")

ggplot(filter(f, heightInCm !=0), aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = heightInCm)) + 
  geom_boxplot() + 
  labs(x = "AFLW2025", y = "Height")


#logistic regression - whether VFL stats/age and height associated with probability of being selected AFL 
model1 <- glm(playedAFL25 ~ goals.x + disposals.x + marks.x + tackles.x + clearances.totalClearances.x + heightInCm + ageVFL, 
              family = "binomial", 
              data = f)
summary(model1)

model2 <- glm(playedAFL25 ~ goals.x + disposals.x + marks.x + tackles.x + clearances.totalClearances.x + heightInCm + ageVFL, 
              family = "binomial", 
              data = m)
summary(model2)
