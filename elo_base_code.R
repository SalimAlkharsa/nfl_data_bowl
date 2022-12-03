# data.table library is necessary to compute DT functions later
library(tidyverse)
library(data.table)
# all files get read into dataframes and manipulated to get a final data.table of every pass snap with all the info we need on that snap
games_file <- read.csv("C:/Users/klein/Dropbox/PC/Downloads/games.csv")
players_file <- read.csv("C:/Users/klein/Dropbox/PC/Downloads/players.csv")
scouting_data <- read.csv("C:/Users/klein/Dropbox/PC/Downloads/pffScoutingData.csv/pffScoutingData.csv")
plays_file <- read.csv("C:/Users/klein/Dropbox/PC/Downloads/plays.csv/plays.csv")
player_name_and_positions <- players_file |>
  transmute(nflId, name = displayName, pos = officialPosition)
scouting_data_w_names_and_positions <- scouting_data |>
  left_join(player_name_and_positions, by = "nflId")
scouting_data_np_and_game_specifics <- scouting_data_w_names_and_positions |>
  left_join(games_file, by = "gameId")
every_pass_snap_f7 <- scouting_data_np_and_game_specifics |>
  filter(pos %in% c("DE", "DT", "ILB", "LB", "MLB", "NT", "OLB")) |>
  filter(nflId %in% elo_chart$nflId) |>
  transmute(gameId, week, playId, rusherId = nflId, rusherName = name, rusher_lineup = pff_positionLinedUp, rusherPos = pos)
every_pass_snap_ol <- scouting_data_np_and_game_specifics |>
  filter(pos %in% c("C", "G", "T")) |>
  filter(pff_nflIdBlockedPlayer %in% elo_chart$nflId) |>
  transmute(gameId, week, homeTeamAbbr, visitorTeamAbbr, playId, blockerId = nflId, blockerName = name,
            blocker_lineup = pff_positionLinedUp, blockerPos = pos,
            blocker_win = 1-pff_beatenByDefender, rusher_win = pff_beatenByDefender,
            rusherId = pff_nflIdBlockedPlayer, pff_blockType, pff_backFieldBlock)
every_pass_snap_ol_vs_f7 <- every_pass_snap_ol |>
  left_join(every_pass_snap_f7, by = c('gameId', 'week', 'playId', 'rusherId')) |>
  mutate(presnap_blocker_elo = 0,
         presnap_rusher_elo = 0,
         postplay_blocker_elo = 0,
         postplay_rusher_elo = 0) |>
  group_by(gameId, playId, rusherId) |>
  mutate(double_team = n()>1) |>
  ungroup() |>
  filter(double_team == FALSE) |>
  filter(pff_blockType %in% c("PP", "PA", "PR", "CL", "UP")) |>
  group_by(blockerId) |>
  mutate(blocker_snaps = n()) |>
  ungroup() |>
  group_by(rusherId) |>
  mutate(rusher_snaps = n()) |>
  ungroup()
setDT(every_pass_snap_ol_vs_f7)


# magic number is the pass block win rate in the NFL that season (can be changed and placed in the data table for every season)
average_pbwr = mean(every_pass_snap_ol_vs_f7$blocker_win)
magic_number = 400*log((1-average_pbwr)/average_pbwr)

# an elo chart is created (and will constantly be updated throughout the program) of all players updated elo (starts at 1000)
elo_chart <- players_file |>
  mutate(side_of_the_ball = ifelse(officialPosition %in% c("C", "G", "T"), "Blocker",
                                   ifelse(officialPosition %in% c("DE", "DT", "ILB", "LB", "MLB", "NT", "OLB"), "Rusher", 0))) |>
  filter(side_of_the_ball != 0) |>
  mutate(elo = 1000)
setDT(elo_chart)
elo_chart_blocker <- merge(elo_chart, every_pass_snap_ol_vs_f7 |> select(blockerId, blocker_snaps), by.x = "nflId", by.y = "blockerId")
elo_chart_blocker <- distinct(elo_chart_blocker)
elo_chart_rusher <- merge(elo_chart, every_pass_snap_ol_vs_f7 |> select(rusherId, rusher_snaps), by.x = "nflId", by.y = "rusherId")
elo_chart_rusher <- distinct(elo_chart_rusher)
elo_chart <- rbind.fill(elo_chart_blocker, elo_chart_rusher)

# full elo calculation, blocker is always player A, rusher is always player B
elo_calculation = function(playerARating, playerBRating, k=32) {
  # Expected score for player A and for player B.
  EA <- (1 / (1 + 10^((playerBRating - playerARating + magic_number)/400)))
  EB <- (1 / (1 + 10^((playerARating - playerBRating - magic_number)/400)))
  # RAn = RA + K * (SA - EA)
  newRatingPlyAWins  <- playerARating + k * (1 - EA)
  newRatingPlyADraws <- playerARating + k * (0.5 - EA)
  newRatingPlyADefeated  <- playerARating + k * (0 - EA)
  # RBn = RB + K * (SB - EB)
  newRatingPlyBWins  <- playerBRating + k * (1 - EB)
  newRatingPlyBDraws <- playerBRating + k * (0.5 - EB)
  newRatingPlyBDefeated  <- playerBRating + k * (0 - EB)
  chanceToWin <- round(data.frame(chanceToWin=c(EA, EB)) * 100, digits=0)
  playerAWins  <- round(data.frame(playerAWins=c(newRatingPlyAWins, newRatingPlyBDefeated)), digits=0)
  playerDraw  <- round(data.frame(draw=c(newRatingPlyADraws, newRatingPlyBDraws)), digits=0)
  playerBWins  <- round(data.frame(playerBWins=c(newRatingPlyADefeated, newRatingPlyBWins)), digits=0)
  
  df <- cbind(chanceToWin, playerAWins, playerDraw, playerBWins)
  rownames(df) <- c('playerA', 'playerB')
  return(df)
}

# here we run the program of elo calculation for every snap, updating the data table with the results as we go along, to help with tracking and evaluation
for(row in 1:nrow(every_pass_snap_ol_vs_f7)) {
  blocker = every_pass_snap_ol_vs_f7[row, blockerId]
  rusher = every_pass_snap_ol_vs_f7[row, rusherId]
  blocker_won = every_pass_snap_ol_vs_f7[row, blocker_win]
  rusher_won = every_pass_snap_ol_vs_f7[row, rusher_win]
  every_pass_snap_ol_vs_f7[row, presnap_blocker_elo:=elo_chart[nflId == blocker, elo]][row, presnap_rusher_elo:=elo_chart[nflId == rusher, elo]]
  results_chart <- elo_calculation(every_pass_snap_ol_vs_f7[row, presnap_blocker_elo], every_pass_snap_ol_vs_f7[row, presnap_rusher_elo])
  ifelse(blocker_won, elo_chart[nflId == blocker, elo:=results_chart[1,2]][nflId == rusher, elo:=results_chart[2,2]],
         ifelse(rusher_won, elo_chart[nflId == blocker, elo:=results_chart[1,4]][nflId == rusher, elo:=results_chart[2,4]],
                elo_chart[nflId == blocker, elo:=results_chart[1,3]][nflId == rusher, elo:=results_chart[2,3]]))
  ifelse(blocker_won, every_pass_snap_ol_vs_f7[blockerId == blocker, postplay_blocker_elo:=results_chart[1,2]][rusherId == rusher, postplay_rusher_elo:=results_chart[2,2]],
         ifelse(rusher_won, every_pass_snap_ol_vs_f7[blockerId == blocker, postplay_blocker_elo:=results_chart[1,4]][rusherId == rusher, postplay_rusher_elo:=results_chart[2,4]],
                every_pass_snap_ol_vs_f7[blockerId == blocker, postplay_blocker_elo:=results_chart[1,3]][rusherId == rusher, postplay_rusher_elo:=results_chart[2,3]]))
}
