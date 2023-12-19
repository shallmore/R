library(devtools)
devtools::install_github("andreweatherman/cbbdata")
library(cbbdata)
library(dplyr)
library(gt)

cbd_login(username = 'avtriviality', password = 'Triviality#2006')

stats <- cbd_torvik_player_season()

stats_filt <- stats %>%
  filter(year >= 2010) %>%
  group_by(year, conf) %>%
  filter(g >= 0.65 * max(g)) %>%
  filter(mpg >= 0.5 * max(mpg)) %>%
  ungroup() %>%
  select(year, player, pos, team, conf, ppg, apg, rpg, spg, bpg, tov, ts, ortg, drtg, porpag, dporpag, adj_oe, adj_de, obpm, dbpm)

team <- data.frame()
years <- unique(stats_filt$year)

for(year in years) {
  team_each <- cbd_torvik_team_factors(year)
  team <- rbind(team, team_each) 
}

team <- team %>%
  mutate(win_pct = wins/games) %>%
  select(year, team, win_pct, team_strength = barthag)

stats_final <- inner_join(stats_filt, team, by = c("year", "team"))

stats_final <- stats_final %>%
  mutate(naismith = 0)

stats_final$naismith[which(stats_final$player == "Evan Turner" & stats_final$year == 2010)] <- 1
stats_final$naismith[which(stats_final$player == "Jimmer Fredette" & stats_final$year == 2011)] <- 1
stats_final$naismith[which(stats_final$player == "Anthony Davis" & stats_final$year == 2012)] <- 1
stats_final$naismith[which(stats_final$player == "Trey Burke" & stats_final$year == 2013)] <- 1
stats_final$naismith[which(stats_final$player == "Doug McDermott" & stats_final$year == 2014)] <- 1
stats_final$naismith[which(stats_final$player == "Frank Kaminsky" & stats_final$year == 2015)] <- 1
stats_final$naismith[which(stats_final$player == "Buddy Hield" & stats_final$year == 2016)] <- 1
stats_final$naismith[which(stats_final$player == "Frank Mason III" & stats_final$year == 2017)] <- 1
stats_final$naismith[which(stats_final$player == "Jalen Brunson" & stats_final$year == 2018)] <- 1
stats_final$naismith[which(stats_final$player == "Zion Williamson" & stats_final$year == 2019)] <- 1
stats_final$naismith[which(stats_final$player == "Obi Toppin" & stats_final$year == 2020)] <- 1
stats_final$naismith[which(stats_final$player == "Luka Garza" & stats_final$year == 2021)] <- 1
stats_final$naismith[which(stats_final$player == "Oscar Tshiebwe" & stats_final$year == 2022)] <- 1
stats_final$naismith[which(stats_final$player == "Zach Edey" & stats_final$year == 2023)] <- 1

stats_final_train <- stats_final %>%
  filter(year != 2024)

stats_final_test <- stats_final %>%
  filter(year == 2024)

stats_final_train[is.na(stats_final_train)] <- 0
stats_final_test[is.na(stats_final_test)] <- 0

naismith_reg <- glm(naismith ~ ppg + apg + rpg + spg + bpg + tov + adj_oe + adj_de + porpag + dporpag + win_pct + team_strength, data = stats_final_train, family = "binomial")

stats_final_train <- stats_final_train %>%
  ungroup() %>%
  mutate(prediction = predict(naismith_reg, stats_final_train, type = "response")) %>%
  group_by(year) %>%
  mutate(award_prob = prediction/sum(prediction)) %>%
  mutate(award_won = ifelse(naismith == 1, "WON", "")) %>%
  ungroup() 

stats_final_test <- stats_final_test %>%
  ungroup() %>%
  mutate(prediction = predict(naismith_reg, stats_final_test, type = "response")) %>%
  group_by(year) %>%
  mutate(award_prob = prediction/sum(prediction)) %>%
  mutate(award_won = ifelse(naismith == 1, "WON", "")) %>%
  ungroup() 

summary(naismith_reg)

final_data_top15_train <- stats_final_train %>%
  group_by(year) %>%
  arrange(-award_prob) %>%
  filter(row_number() <= 15) %>%
  mutate(award_prob = round(award_prob, 3)) %>%
  select(year, player, team, pos, conf, award_prob, award_won) %>%
  ungroup()

final_data_top15_2023 <- stats_final_test %>%
  arrange(-award_prob) %>%
  filter(row_number() <= 15) %>%
  mutate(award_prob = round(award_prob, 3)) %>%
  select(player, team, pos, conf, award_prob)

subfolder_path <- "naismith/"
dir.create(subfolder_path, showWarnings = FALSE)

szns <- unique(final_data_top15_train$year)

for (szn in szns) {
  per_year <- final_data_top15_train %>%
    filter(year == szn)
  table <- per_year %>% gt() %>% 
    cols_align(
      align = "center",
      columns = c(year, player, team, pos, conf, award_prob, award_won)
    ) %>%
    data_color(
      columns = award_prob,
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::blue_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>%
    cols_label(
      year = md("**Year**"),
      player = md("**Player**"),
      team = md("**Team**"),
      pos = md("**Position**"),
      conf = md("**Conference**"),
      award_prob = md("**Naismith Probability**"),
      award_won = md("**Naismith Result**")
    ) 
  filename <- paste0(szn, "naismith.png")
  gtsave(table, file.path(subfolder_path, filename))
}

table_2024 <- final_data_top15_2023 %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(player, team, pos, conf, award_prob)
  ) %>%
  data_color(
    columns = award_prob,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team = md("**Team**"),
    pos = md("**Position**"),
    conf = md("**Conference**"),
    award_prob = md("**Naismith Probability**"),
  ) %>%
  tab_header(
    title = md("**2023-24 CBB Naismith Probability**"),
    subtitle = "Based on Naismith CBB Data from 2010 - 2023"
  )
gtsave(table_2024, "naismith/2024naismith.png")
