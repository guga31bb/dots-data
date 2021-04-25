library(tidyverse) 
library(gganimate)

# where the raw tracking data is stored
path <- "../nfl-big-data-bowl-2021"

df_games <- suppressMessages(readr::read_csv(glue::glue("{path}/input/games.csv"))) %>%
  janitor::clean_names()

info <- nflfastR::load_pbp(2018) %>% 
  dplyr::rename(nflfastr_id = game_id, game_id = old_game_id) %>%
  dplyr::select(posteam, home_team, away_team, down, ydstogo, week, nflfastr_id, game_id, qtr, play_id, epa, yards_gained, air_yards, epa, desc, complete_pass) %>%
  dplyr::mutate(game_id = as.numeric(game_id))

# https://github.com/rossdrucker/sportyR
nfl_field <- sportyR::geom_football('nfl')

load_week <- function(week) {
  
  df_track <- suppressMessages(readr::read_csv(glue::glue("{path}/input/week{week}.csv"))) %>%
    janitor::clean_names() %>%
    left_join(info, by = c("game_id", "play_id")) %>%
    mutate(
      o_rad = o * pi / 180,
      team_name = case_when(
        team == "home" ~ home_team,
        team == "away" ~ away_team,
        TRUE ~ team,
      ),
      # for the ball
      o_x = ifelse(is.na(o), NA_real_, sin(o_rad)),
      o_y = ifelse(is.na(o), NA_real_, cos(o_rad)),
    ) %>%
    # dplyr::mutate_at(dplyr::vars("team_name"), nflfastR:::team_name_fn) %>%
    left_join(nflfastR::teams_colors_logos %>% select(-team_name), by = c("team_name" = "team_abbr")) %>%
    mutate(team_color = ifelse(team_name == "football", "brown", team_color))
  
}


save_play <- function(row) {
  
  # testing only
  # row <- plays %>% dplyr::slice(1)
  
  g <- row$game_id
  p <- row$play_id
  
  play <- info %>% filter(game_id == g, play_id == p)
  title <- glue::glue("{play$nflfastr_id}")
  caption <- glue::glue("{play$down}&{play$ydstogo}: Q{play$qtr} {play$desc}")
  
  df_track <- weekly_tracking %>%
    filter(game_id == g, play_id == p)
  
  # before animation
  fig <- nfl_field +
    # orientation
    geom_point(data = df_track, aes(x, y), color = df_track$team_color, 
               shape = ifelse(df_track$team_name == "football", 19, 1), 
               size = 4
    ) +
    geom_segment(
      data = df_track,
      aes(x, y, xend = x + 2.5 * o_x, yend = y + 2.5 * o_y),
      color = df_track$team_color, size = 1.5
    ) +
    geom_text(
      data = df_track,
      mapping = aes(x = x, y = y, label = jersey_number),
      colour = df_track$team_color2, size = 2
    ) +
    labs(
      caption = caption,
      title = title
    ) +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      plot.margin = margin(.1, 0, .5, 0, "cm"),
      plot.caption = element_text(size = 8)
    )
  
  fig
  
  plot <- fig +
    transition_time(df_track$frame_id)
  
  animate(plot, 
          height = 3.5, width = 7, units = "in", res = 200,
          nframes = n_distinct(df_track$frame_id)
  )
  
  anim_save(glue::glue("data/dots_{g}_{p}.gif"))
  
}


weekly_tracking <- load_week(14)

plays <- weekly_tracking %>%
  select(game_id, play_id) %>%
  group_by(game_id, play_id) %>%
  dplyr::slice(1) %>%
  ungroup()

# testing
# plays <- plays %>%
#   dplyr::slice(1:10)

walk(1 : nrow(plays), ~{
  save_play(plays %>% dplyr::slice(.x))
})




