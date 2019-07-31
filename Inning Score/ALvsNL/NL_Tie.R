setwd("C:/Users/Jaewon/Desktop/Fall 2015/Research - Baseball Analytics/Inning Score/ALvsNL/")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2010 MLB Season: 2010-04-04 to 2010-10-03
# 2011 MLB Season: 2011-03-31 to 2011-09-28
# 2012 MLB Season: 2012-03-28 to 2012-10-03
# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

urls <- makeUrls(start = "2012-03-28", end = "2012-10-03")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid_AL <- c()
valid_gid_NL <- c()

## Find valid games. Count games that marked "Final" && NL
count = 0
for (i in 1:nrow(game)) {
  if ((game$status[i] == "Final") & (game$home_league_id[i] == 104)) {
    valid_gid_NL <- c(valid_gid_NL, toString(game$url[i]))
  }
}

## Game by game to count tie at some point during each game
gids_table <- data.frame(valid_gid_NL)
size = nrow(gids_table)
result = matrix(0, nrow = 8, ncol = 8)
total = matrix(0, nrow = 8, ncol = 8)

for (i in 1:size) {
  temp <- XML2Obs(paste0(gids_table[i,1]))
  single <- collapse_obs(temp)
  single_game <- data.frame(single$`game//linescore`)
  single_game$url <- NULL
  single_game$home_inning_runs <- as.numeric(as.character(single_game$home_inning_runs))
  single_game$away_inning_runs <- as.numeric(as.character(single_game$away_inning_runs))
  single_game$inning <- as.numeric(as.character(single_game$inning))
  
  length <- nrow(single_game)
  
  for (j in 1:8) {
    index_diff = abs(single_game[j,2] - single_game[j,3])
    if (index_diff >= 8) {
      index_diff = 8
    }
    if (index_diff > 0) {
      total[index_diff, j] = total[index_diff, j] + 1
      temp_home = single_game[j,2]
      temp_away = single_game[j,3]
      for (k in (j+1):length) {
        if (toString(single_game[k,2]) == 'NA') {
          single_game[k,2] = 0
        }
        temp_home = temp_home + single_game[k,2]
        temp_away = temp_away + single_game[k,3]
        if (temp_home == temp_away) {
          result[index_diff, j] = result[index_diff, j] + 1
          break
        }
      }
    }
  }
}
# Number of Tie at some point
write.csv(result,"Tie_NL_result_2012.csv")
# Count number of leads for inning by inning score
write.csv(total,"Tie_NL_total_2012.csv")

