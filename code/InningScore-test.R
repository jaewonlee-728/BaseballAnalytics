setwd("C:/Users/Jaewon/Desktop/")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2010 MLB Season: 2010-04-04 to 2010-10-03
# 2011 MLB Season: 2011-03-31 to 2011-09-28
# 2012 MLB Season: 2012-03-28 to 2012-10-03
# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

urls <- makeUrls(start = "2010-04-04", end = "2010-10-03")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid <- c()

## Find valid games. Count games that marked "Final"
count = 0
for (i in 1:nrow(game)) {
  if (game$status[i] == "Final") {
    count <- count + 1
    valid_gid <- c(valid_gid, toString(game$url[i]))
  }
}

## Game by game to count lead eventually winning each game
gids_table <- data.frame(valid_gid)
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
  final_home = 0
  final_away = 0
  for (j in 1:length) {
    if (toString(single_game[j,2]) == 'NA') {
      single_game[j,2] = 0
    }
    final_home = final_home + single_game[j,2]
    final_away = final_away + single_game[j,3]
  }
  for (k in 1:8) {
    diff = abs(single_game[k,2] - single_game[k,3])
    if (diff >= 8) {
      diff = 8
    }
    if (diff > 0) {
      total[diff,k] = total[diff,k] + 1
      if (single_game[k,2] > single_game[k,3] && final_home > final_away) {
        result[diff,k] = result[diff,k] + 1        
      } else if (single_game[k,2] < single_game[k,3] && final_home < final_away) {
        result[diff,k] = result[diff,k] + 1
      }
    }
  }
}

# result: number of games lead for diff and eventually win
write.csv(result, "2010_lead_result.csv")
# total: number of games has lead
write.csv(total, "2010_lead_total.csv")

      
