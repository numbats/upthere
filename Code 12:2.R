library(fitzRoy)
library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot)
library(GGally)

f2025 <- fetch_player_stats(season = 2025, comp = "AFLW", source = "AFL") |> 
  mutate(player.player.player.playerId = as.character(player.player.player.playerId))

m2025 <- fetch_player_stats(season = 2025, comp = "AFLM", source = "AFL") |> 
  mutate(player.player.player.playerId = as.character(player.player.player.playerId))

f2024 <- fetch_player_stats(season = 2024, comp = "VFLW", source = "AFL") |> 
  mutate(player.player.player.playerId = as.character(player.player.player.playerId))

m2024 <- fetch_player_stats(season = 2024, comp = "VFL", source = "AFL") |> 
  mutate(player.player.player.playerId = as.character(player.player.player.playerId))

#mens VFL 2024 
vflm2024 <- m2024 |> 
  group_by(player.player.player.playerId) |>
  summarise_if(is.numeric, mean)
vflmnames <- m2024 |>
  group_by(player.player.player.playerId) |>
  summarise(
    given_name = first(player.player.player.givenName),
    surname = first(player.player.player.surname),
    .groups = "drop"
  )
vflm2024 <- left_join(vflm2024, vflmnames, by = "player.player.player.playerId") |>
  rename(ID = player.player.player.playerId) |>
  select(ID, given_name, surname,
         goals:marks, tackles, inside50s, hitouts, freesFor:rebound50s, clearances.totalClearances)
vflm2024

#mens AFL 2025 
aflm2025 <- m2025 |> 
  group_by(player.player.player.playerId) |>
  summarise_if(is.numeric, mean)
aflmnames <- m2025 |>
  group_by(player.player.player.playerId) |>
  summarise(
    given_name = first(player.player.player.givenName),
    surname = first(player.player.player.surname),
    .groups = "drop"
  )
aflm2025 <- left_join(aflm2025, aflmnames, by = "player.player.player.playerId") |>
  rename(ID = player.player.player.playerId) |>
  select(ID, given_name, surname,
         goals:clearances.totalClearances)
aflm2025

#womens VFL 2024
vflw2024 <- f2024 |> 
  group_by(player.player.player.playerId) |>
  summarise_if(is.numeric, mean)
vflwnames <- f2024 |>
  group_by(player.player.player.playerId) |>
  summarise(
    given_name = first(player.player.player.givenName),
    surname = first(player.player.player.surname),
    .groups = "drop"
  )
vflw2024 <- left_join(vflw2024, vflwnames, by = "player.player.player.playerId") |>
  rename(ID = player.player.player.playerId) |>
  select(ID, given_name, surname,
         goals:marks, tackles, inside50s, hitouts, freesFor:rebound50s, clearances.totalClearances)
vflw2024

#womens AFL 2025 
aflw2025 <- f2025 |> 
  group_by(player.player.player.playerId) |>
  summarise_if(is.numeric, mean)
aflwnames <- f2025 |>
  group_by(player.player.player.playerId) |>
  summarise(
    given_name = first(player.player.player.givenName),
    surname = first(player.player.player.surname),
    .groups = "drop"
  )
aflw2025 <- left_join(aflw2025, aflwnames, by = "player.player.player.playerId") |>
  rename(ID = player.player.player.playerId) |>
  select(ID, given_name, surname,
         goals:clearances.totalClearances)
aflw2025

#player info
aflwinfo <- fetch_player_details(season = 2025, comp = "AFLW", current = FALSE) |> 
  select(providerId, firstName, surname, position, dateOfBirth, heightInCm, debutYear) |> 
  rename(ID = providerId)

vflwinfo <- fetch_player_details(season = 2024, comp = "VFLW", current = FALSE) |> 
  select(providerId, firstName, surname,dateOfBirth, heightInCm) |>
  rename(ID = providerId)

aflminfo <- fetch_player_details(season = 2025, comp = "AFLM", current = FALSE) |>
  select(providerId, firstName, surname, position, dateOfBirth, heightInCm, draftYear, debutYear) |>
  rename(ID = providerId)

vflminfo <- fetch_player_details(season = 2024, comp = "VFL", current = FALSE) |>
  select(providerId, firstName, surname, dateOfBirth, heightInCm, draftYear, debutYear) |>
  rename(ID = providerId)

female <- bind_rows(aflwinfo, vflwinfo) |>
  distinct(ID, .keep_all = TRUE) |> 
  rename(given_name = firstName)

male <- bind_rows(aflminfo, vflminfo) |> 
  distinct(ID, .keep_all = TRUE) |>
  rename(given_name = firstName)

#full dataset 
m<- left_join(vflm2024, aflm2025, 
              by = "ID") |> 
  left_join(male, by = "ID") |>
  mutate(playedAFL25 = ifelse(!is.na(dreamTeamPoints.y), 1, 0)) |> 
  mutate(dateOfBirth = as.Date(dateOfBirth)) |> 
  mutate(ageVFL = as.numeric(difftime(as.Date("2024-03-22"), dateOfBirth, units = "days")) / 365)

f <- left_join(vflw2024, aflw2025, 
               by = "ID") |> 
  left_join(female, by = "ID") |> 
  mutate(playedAFL25 = ifelse(!is.na(dreamTeamPoints.y), 1, 0)) |> 
  mutate(dateOfBirth = as.Date(dateOfBirth)) |> 
  mutate(ageVFL = as.numeric(difftime(as.Date("2024-03-22"), dateOfBirth, units = "days")) / 365)

#adding positional data 
primary_position_female <- f2024 |>
  group_by(player.player.player.playerId, player.player.position) |>
  summarise(n_games = n(), .groups = "drop") |>
  group_by(player.player.player.playerId) |>
  arrange(player.player.position == "INT", desc(n_games), .by_group = TRUE) |>
  slice(1) |> 
  select(ID = player.player.player.playerId, primary_positionVFL = player.player.position)
f <- left_join(f, primary_position_female, by = "ID")

primary_position_male <- m2024 |>
  group_by(player.player.player.playerId, player.player.position) |>
  summarise(n_games = n(), .groups = "drop") |>
  group_by(player.player.player.playerId) |>
  arrange(player.player.position == "INT", desc(n_games), .by_group = TRUE) |>
  slice(1) |> 
  select(ID = player.player.player.playerId, primary_positionVFL = player.player.position)
m <- left_join(m, primary_position_male, by = "ID")

#PCA 
#first check correlations variables 
cor_matrix_f <- cor(f |> select(goals.x:clearances.totalClearances.x), use = "complete.obs")
corrplot(cor_matrix_f, method = "circle")
vars_f <- f |> select(goals.x:clearances.totalClearances.x)
ggpairs(vars_f)

cor_matrix_m <- cor(m |> select(goals.x:clearances.totalClearances.x), use = "complete.obs")
corrplot(cor_matrix_m, method = "circle")
vars_m <- m |> select(goals.x:clearances.totalClearances.x)
ggpairs(vars_m)

PCA_vflw <- f |> select(goals.x:clearances.totalClearances.x) |> 
  prcomp(center = TRUE, scale. = TRUE)
plot(c(1:14),PCA_vflw$sdev^2,xlab="PC No.",ylab="Variance",type="l")
var_vflw<- PCA_vflw$sdev^2/sum(PCA_vflw$sdev^2)
cumsum(var_vflw)
PCA_vflw$rotation[, 1:4]

PCA_vflm <- m |> select(goals.x:clearances.totalClearances.x) |>
  prcomp(center = TRUE, scale. = TRUE)
plot(c(1:14),PCA_vflm$sdev^2,xlab="PC No.",ylab="Variance",type="l")
var_vflm<- PCA_vflm$sdev^2/sum(PCA_vflm$sdev^2)
cumsum(var_vflm)
PCA_vflm$rotation[,1:4]

f <- f |> 
  mutate(PC1 = PCA_vflw$x[,1], 
         PC2 = PCA_vflw$x[,2], 
         PC3 = PCA_vflw$x[,3], 
         PC4 = PCA_vflw$x[,4])

m <- m |> 
  mutate(PC1 = PCA_vflm$x[,1], 
         PC2 = PCA_vflm$x[,2], 
         PC3 = PCA_vflm$x[,3], 
         PC4 = PCA_vflm$x[,4])

ggplot(f, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = PC1)) +
  geom_boxplot() + 
  labs(x = "AFLW2025", y = "PC1 - VFLW")
ggplot(m, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = PC1)) +
  geom_boxplot() + 
  labs(x = "AFL2025", y = "PC1 - VFL")

ggplot(f, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = PC2)) +
  geom_boxplot() + 
  labs(x = "AFLW2025", y = "PC2 - VFLW")
ggplot(m, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = PC2)) +
  geom_boxplot() + 
  labs(x = "AFL2025", y = "PC2 - VFL")

ggplot(f, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = PC3)) +
  geom_boxplot() + 
  labs(x = "AFLW2025", y = "PC3 - VFLW")
ggplot(m, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = PC3)) +
  geom_boxplot() + 
  labs(x = "AFL2025", y = "PC3 - VFL")

ggplot(f, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = PC4)) +
  geom_boxplot() + 
  labs(x = "AFLW2025", y = "PC4 - VFLW")
ggplot(m, aes(x = factor(playedAFL25, labels = c("Not selected", "Selected")), y = PC4)) +
  geom_boxplot() + 
  labs(x = "AFL2025", y = "PC4 - VFL")


#clustering 
set.seed(123)
k.max <- 15
dataf <- f |> select(PC1:PC4)
wssf <- sapply(1:k.max, 
               function(k){kmeans(dataf, k, nstart = 50, iter.max = 15)$tot.withinss})
plot(1:k.max, wssf,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)
kmeansf <- kmeans(dataf, centers = 5, nstart= 50, iter.max = 15)
f<- f |> 
  mutate(cluster = factor(kmeansf$cluster))
f |> 
  group_by(cluster) |> 
  summarise(across(PC1:PC4, mean))

pos_vs_cluster_f <- table(f$cluster, f$position)
print(pos_vs_cluster_f)

position_f <- table(f$cluster, f$primary_positionVFL)
print(position_f)

f |>
  group_by(cluster) |>
  summarise(
    n = n(),
    no.selected = sum(playedAFL25 == 1),
    selection_rate = mean(playedAFL25 == 1),
    .groups = "drop"
  ) |>
  ggplot(aes(x = factor(cluster), y = selection_rate)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Cluster",
    y = "Selection rate",
    title = "AFLW selection rate by cluster"
  )


set.seed(123)
k.max <- 15
datam <- m |> select(PC1:PC4)
wssm <- sapply(1:k.max, 
               function(k){kmeans(datam, k, nstart = 50, iter.max = 15)$tot.withinss})
plot(1:k.max, wssm,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)
kmeansm <- kmeans(datam, centers = 5, nstart= 50, iter.max = 15)
m<- m |> 
  mutate(cluster = factor(kmeansm$cluster))
m |> 
  group_by(cluster) |> 
  summarise(across(PC1:PC4, mean))

pos_vs_cluster_m <- table(m$cluster, m$position)
print(pos_vs_cluster_m)

position_m <- table(m$cluster, m$primary_positionVFL)
print(position_m)

m |>
  group_by(cluster) |>
  summarise(
    n = n(),
    no.selected = sum(playedAFL25 == 1),
    selection_rate = mean(playedAFL25 == 1),
    .groups = "drop"
  ) |>
  ggplot(aes(x = factor(cluster), y = selection_rate)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Cluster",
    y = "Selection rate",
    title = "AFL selection rate by cluster"
  )


#propensity scores 
m_final <- m |> 
  mutate(age_c = ageVFL - mean(ageVFL, na.rm = TRUE)) |> 
  mutate(height_c = heightInCm - mean(heightInCm, na.rm = TRUE))

f_final <- f|> 
  mutate(age_c = ageVFL - mean(ageVFL, na.rm = TRUE)) |> 
  mutate(height_c = heightInCm - mean(heightInCm, na.rm = TRUE))

ps_data_m <- m_final |>
  filter(
    !is.na(age_c),
    !is.na(height_c),
    complete.cases(PC1, PC2, PC3, PC4)
  )

ps_data_f <- f_final |>
  filter(
    !is.na(age_c),
    !is.na(height_c),
    complete.cases(PC1, PC2, PC3, PC4)
  )

ps_male <- glm(playedAFL25 ~ PC1 + PC2 + PC3 + PC4 + age_c + height_c, 
               data = ps_data_m, 
               family = "binomial")
summary(ps_male)

ps_female <- glm(playedAFL25 ~ PC1 + PC2 + PC3 + PC4 + age_c + height_c, 
                 data = ps_data_f, 
                 family = "binomial")
summary(ps_female)

ps_data_m$ps <- predict(ps_male, type = "response")
thr <- 0.5
ps_data_m$pred <- ifelse(ps_data_m$ps >= thr, 1, 0)
table(predicted = ps_data_m$pred, 
      actual = ps_data_m$playedAFL25)

ps_data_f$ps <- predict(ps_female, type = "response")
thr <- 0.5
ps_data_f$pred <- ifelse(ps_data_f$ps >= thr, 1, 0)
table(predicted = ps_data_f$pred, 
      actual = ps_data_f$playedAFL25)

#propensity score matching 
library(MatchIt)
library(cobalt)

set.seed(123)
matching_f <- matchit(playedAFL25 ~ PC1 + PC2 + PC3 + PC4 + age_c + height_c, 
                      data = ps_data_f, 
                      method = "nearest", 
                      replace = TRUE,
                      exact = ~cluster,
                      caliper = 0.15)
matched_df_f <- match.data(matching_f)

summary(matching_f)
bal.tab(matching_f)
love.plot(matching_f, 
          drop.distance = TRUE, 
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE, 
          thresholds = c(m = .1))


matching_m <- matchit(playedAFL25 ~ PC1 + PC2 + PC3 + PC4 + age_c + height_c, 
                      data = ps_data_m, 
                      method = "nearest", 
                      exact = ~cluster, 
                      replace = TRUE, 
                      caliper = 0.15)
matched_df_m <- match.data(matching_m)

summary(matching_m)
bal.tab(matching_m)
love.plot(matching_m, 
          drop.distance = TRUE, 
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE, 
          thresholds = c(m = .1))

#linear mixed model 
library(lme4)
library(lmerTest)

LMM_m1 <- lmer(dreamTeamPoints.y ~ PC1 + PC2 + PC3 + PC4 + age_c + height_c + (1 | cluster), 
            data = m_final)
summary(LMM_m1) 
           
LMM_m2 <- lmer(dreamTeamPoints.y ~ PC1 + age_c + height_c + (1 | cluster), 
               data = m_final)
summary(LMM_m2) 

anova(LMM_m1, LMM_m2)
#model 2 better

LMM_m3 <- lmer(dreamTeamPoints.y ~ PC1 + age_c + height_c + (PC1 | cluster), 
                         data = m_final)
summary(LMM_m3) 
anova(LMM_m2, LMM_m3)
#model 2 better

LM_m1 <- lm(dreamTeamPoints.y ~ PC1 + age_c + height_c, 
              data = m_final)
summary(LM_m1) 
anova(LMM_m2, LM_m1)
#more simple model with no random effects is better 


LMM_f1 <- lmer(dreamTeamPoints.y ~ PC1 + PC2 + PC3 + PC4 + age_c + height_c + (1 | cluster), 
               data = f_final)
summary(LMM_f1) 

LMM_f2 <- lmer(dreamTeamPoints.y ~ PC1 + age_c + (1 | cluster), 
               data = f_final)
summary(LMM_f2) 
anova(LMM_f1, LMM_f2)
#model 2 better

LM_f1 <- lm(dreamTeamPoints.y ~ PC1 + age_c, 
            data = f_final)
summary(LM_f1) 
anova(LMM_f2, LM_f1)
#more simple model with no random effects is better 


#creating game involvement variable (PC1) for AFL data - using same loadings as VFL 
required_cols_m <- rownames(PCA_vflm$rotation)
AFL_stats <- aflm2025 |>
  rename_with(~ paste0(., ".x"), goals:clearances.totalClearances) |> 
  select(ID, all_of(required_cols_m))
AFL_pc_scores <- predict(PCA_vflm, newdata = AFL_stats) 
AFL_stats <- AFL_stats |> mutate(AFL_PC1 = AFL_pc_scores[, 1])
m_final <- m_final |> 
  left_join(AFL_stats |> select(ID, AFL_PC1), by = "ID")

required_cols_f <- rownames(PCA_vflw$rotation)
AFLW_stats <- aflw2025 |> 
  rename_with(~ paste0(., ".x"), goals:clearances.totalClearances) |> 
  select(ID, all_of(required_cols_f))
AFLW_pc_scores <- predict(PCA_vflw, newdata = AFLW_stats) 
AFLW_stats <- AFLW_stats |> mutate(AFLW_PC1 = AFLW_pc_scores[, 1])
f_final <- f_final |> 
  left_join(AFLW_stats |> select(ID, AFLW_PC1), by = "ID")

