
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(cfbfastR)
library(hoopR)
library(dotenv)

# Set API Keys ------------------------------------------------------------

load_dot_env()
Sys.setenv(CFBD_API_KEY = Sys.getenv("API_KEY"))

# CFB ---------------------------------------------------------------------

years <- c(2020:2025)
weeks <- c(1:15)
map_cfb <- expand.grid(year = years, week = weeks)

final_scores_cfb <- purrr::map2_dfr(map_cfb$year, map_cfb$week, function(y, w) {
  espn_cfb_schedule(
    year = y, 
    week = w, 
    groups = 'fbs'
  )
}, .progress = TRUE) %>% 
  filter(status_name == 'STATUS_FINAL')

one_score_loss_cfb <- final_scores_cfb %>%
  filter(abs(home_score - away_score) <= 8) %>% 
  mutate(losing_team = ifelse(home_score < away_score, home_team_full, away_team_full)) %>%
  group_by(losing_team) %>%
  summarise(one_score_loss_cfb = n()) %>%
  arrange(desc(one_score_loss_cfb))


# MBB ---------------------------------------------------------------------

final_scores_mbb <- hoopR::espn_mbb_scoreboard(2026)

final_scores_mbb <- purrr::map_dfr(c(2021:2026), function(y) {
  hoopR::espn_mbb_scoreboard(y)
}, .progress = TRUE) %>% 
  filter(status_name == 'STATUS_FINAL')

one_score_loss_mbb <- final_scores_mbb %>% 
  filter(abs(home_score - away_score) <= 3) %>% 
  mutate(losing_team = ifelse(home_score < away_score, home_team_full_name, away_team_full_name)) %>%
  group_by(losing_team) %>%
  summarise(one_poss_loss_mbb = n()) %>%
  arrange(desc(one_poss_loss_mbb))


# Merge -------------------------------------------------------------------

results <- one_score_loss_cfb %>% 
  left_join(one_score_loss_mbb, by = "losing_team") %>% 
  mutate(
    one_score_loss_cfb = coalesce(one_score_loss_cfb, 0),
    one_poss_loss_mbb  = coalesce(one_poss_loss_mbb, 0),
    total = one_score_loss_cfb + one_poss_loss_mbb
  ) %>% 
  arrange(desc(total))

team_logos <- hoopR::espn_mbb_teams() %>% 
  select(display_name, logo)

results <- results %>% 
  left_join(team_logos, by = c('losing_team' = 'display_name'))

write.csv(results, 'results.csv')


# Analysis ---------------------------------------------------------------------

results <- results %>% filter(total >= 7) # Removes FCS teams

# Remove teams who weren't in FBS since 2020
teams_to_remove <- c('Nicholls Colonels', 'Missouri State Bears',
                     'Kennesaw State Owls', 'Sam Houston Bearkats',
                     'Delaware Blue Hens', 'James Madison Dukes', 
                     'Jacksonville State Gamecocks')

results <- results %>% filter(!losing_team %in% teams_to_remove)

# Fix App St.

# Analysis
hist(results$total, breaks = 25)
mean(results$total)
sd(results$total)
shapiro.test(results$total)

hist(results$one_score_loss_cfb, breaks = 25)
mean(results$one_score_loss_cfb)
sd(results$one_score_loss_cfb)
shapiro.test(results$one_score_loss_cfb)

library(e1071)

skewness(results$one_score_loss_cfb)
kurtosis(results$one_score_loss_cfb)