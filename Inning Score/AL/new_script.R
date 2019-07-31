setwd("C:/Users/Jaewon/Desktop/AL")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

# game$home_league_id -> 103 for AL, 104 for NL

urls <- makeUrls(start = "2013-03-31", end = "2013-09-30")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid_AL_13_lead <- c()

## Find valid games. Count games that marked "Final"
count = 0
for (i in 1:nrow(game)) {
  if ((game$status[i] == "Final") & (game$home_league_id[i] == 103)) {
    valid_gid_AL_13_lead <- c(valid_gid_AL_13_lead, toString(game$url[i]))
  }
}

# Process again for valid games
valid_xml <- XML2Obs(paste0(valid_gid_AL_13_lead))
valid <- collapse_obs(valid_xml)
valid_game <- data.frame(valid$game)
linescore <- data.frame(valid$`game//linescore`)

# Store all final scores for both home_team and away_team
home_team_runs <- valid_game$home_team_runs
away_team_runs <- valid_game$away_team_runs
total_score <- data.frame(home_team_runs, away_team_runs)
total_score$home_team_runs <- as.numeric(as.character(total_score$home_team_runs))
total_score$away_team_runs <- as.numeric(as.character(total_score$away_team_runs))

# Store all inning by inning score 
one = 0
two = 0
three = 0
four = 0
five = 0
six = 0
seven = 0
eight = 0
nine = 0

one_run = c()
two_run = c()
three_run = c()
four_run = c()
five_run = c()
six_run = c()
seven_run = c()
eight_run = c()
nine_run = c()

for (i in 1:nrow(linescore)) {
  if (as.numeric(as.character(linescore[i,1])) == 1) {
    # 1st inning
    one <- one + 1
    one_run <- rbind(one_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 2) {
    # 2nd inning
    two = two + 1
    two_run <- rbind(two_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 3) {
    # 3rd inning
    three = three + 1
    three_run <- rbind(three_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 4) {
    # 4th inning
    four = four + 1
    four_run <- rbind(four_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 5) {
    # 5th inning
    five = five + 1
    five_run <- rbind(five_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 6) {
    # 6th inning
    six = six + 1
    six_run <- rbind(six_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 7) {
    # 7th inning
    seven = seven + 1
    seven_run <- rbind(seven_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 8) {
    # 8th inning
    eight = eight + 1
    eight_run <- rbind(eight_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) >= 9) {
    # 9th inning or more
    nine = nine + 1
    nine_run <- rbind(nine_run, linescore[i,])
  }
}

result = matrix(0, nrow = 8, ncol = 8)
total = matrix(0, nrow = 8, ncol = 8)

# 1st Inning
one_run$inning <- as.numeric(as.character(one_run$inning))
one_run$home_inning_runs <- as.numeric(as.character(one_run$home_inning_runs))
one_run$away_inning_runs <- as.numeric(as.character(one_run$away_inning_runs))
one_run$url <- NULL
j = 1
for (i in 1:nrow(one_run)) {
  diff = abs(one_run[i,2] - one_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (one_run[i,2] > one_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1
    }
  }
  # away team leads and eventually away team wins
  else if (one_run[i,2] < one_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 2nd Inning
two_run$inning <- as.numeric(as.character(two_run$inning))
two_run$home_inning_runs <- as.numeric(as.character(two_run$home_inning_runs))
two_run$away_inning_runs <- as.numeric(as.character(two_run$away_inning_runs))
two_run$url <- NULL
j = 2
for (i in 1:nrow(two_run)) {
  diff = abs(two_run[i,2] - two_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (two_run[i,2] > two_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1
    }
  }
  # away team leads and eventually away team wins
  else if (two_run[i,2] < two_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 3rd Inning
three_run$inning <- as.numeric(as.character(three_run$inning))
three_run$home_inning_runs <- as.numeric(as.character(three_run$home_inning_runs))
three_run$away_inning_runs <- as.numeric(as.character(three_run$away_inning_runs))
three_run$url <- NULL
j = 3
for (i in 1:nrow(three_run)) {
  diff = abs(three_run[i,2] - three_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (three_run[i,2] > three_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (three_run[i,2] < three_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 4th Inning
four_run$inning <- as.numeric(as.character(four_run$inning))
four_run$home_inning_runs <- as.numeric(as.character(four_run$home_inning_runs))
four_run$away_inning_runs <- as.numeric(as.character(four_run$away_inning_runs))
four_run$url <- NULL
j = 4
for (i in 1:nrow(four_run)) {
  diff = abs(four_run[i,2] - four_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (four_run[i,2] > four_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (four_run[i,2] < four_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 5th Inning
five_run$inning <- as.numeric(as.character(five_run$inning))
five_run$home_inning_runs <- as.numeric(as.character(five_run$home_inning_runs))
five_run$away_inning_runs <- as.numeric(as.character(five_run$away_inning_runs))
five_run$url <- NULL
j = 5
for (i in 1:nrow(five_run)) {
  diff = abs(five_run[i,2] - five_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (five_run[i,2] > five_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (five_run[i,2] < five_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 6th Inning
six_run$inning <- as.numeric(as.character(six_run$inning))
six_run$home_inning_runs <- as.numeric(as.character(six_run$home_inning_runs))
six_run$away_inning_runs <- as.numeric(as.character(six_run$away_inning_runs))
six_run$url <- NULL
j = 6
for (i in 1:nrow(six_run)) {
  diff = abs(six_run[i,2] - six_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (six_run[i,2] > six_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (six_run[i,2] < six_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 7th Inning
seven_run$inning <- as.numeric(as.character(seven_run$inning))
seven_run$home_inning_runs <- as.numeric(as.character(seven_run$home_inning_runs))
seven_run$away_inning_runs <- as.numeric(as.character(seven_run$away_inning_runs))
seven_run$url <- NULL
j = 7
for (i in 1:nrow(seven_run)) {
  diff = abs(seven_run[i,2] - seven_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (seven_run[i,2] > seven_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (seven_run[i,2] < seven_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 8th Inning
eight_run$inning <- as.numeric(as.character(eight_run$inning))
eight_run$home_inning_runs <- as.numeric(as.character(eight_run$home_inning_runs))
eight_run$away_inning_runs <- as.numeric(as.character(eight_run$away_inning_runs))
eight_run$url <- NULL
j = 8
for (i in 1:nrow(eight_run)) {
  diff = abs(eight_run[i,2] - eight_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (eight_run[i,2] > eight_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (eight_run[i,2] < eight_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# result: number of games lead for diff and eventually win
write.csv(result, "Lead_AL_result_2013.csv")
# total: number of games has lead
write.csv(total, "Lead_AL_total_2013.csv")

setwd("C:/Users/Jaewon/Desktop/AL")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

# game$home_league_id -> 103 for AL, 104 for NL

urls <- makeUrls(start = "2014-03-22", end = "2014-09-28")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid_AL_14_lead <- c()

## Find valid games. Count games that marked "Final"
count = 0
for (i in 1:nrow(game)) {
  if ((game$status[i] == "Final") & (game$home_league_id[i] == 103)) {
    valid_gid_AL_14_lead <- c(valid_gid_AL_14_lead, toString(game$url[i]))
  }
}

# Process again for valid games
valid_xml <- XML2Obs(paste0(valid_gid_AL_14_lead))
valid <- collapse_obs(valid_xml)
valid_game <- data.frame(valid$game)
linescore <- data.frame(valid$`game//linescore`)

# Store all final scores for both home_team and away_team
home_team_runs <- valid_game$home_team_runs
away_team_runs <- valid_game$away_team_runs
total_score <- data.frame(home_team_runs, away_team_runs)
total_score$home_team_runs <- as.numeric(as.character(total_score$home_team_runs))
total_score$away_team_runs <- as.numeric(as.character(total_score$away_team_runs))

# Store all inning by inning score 
one = 0
two = 0
three = 0
four = 0
five = 0
six = 0
seven = 0
eight = 0
nine = 0

one_run = c()
two_run = c()
three_run = c()
four_run = c()
five_run = c()
six_run = c()
seven_run = c()
eight_run = c()
nine_run = c()

for (i in 1:nrow(linescore)) {
  if (as.numeric(as.character(linescore[i,1])) == 1) {
    # 1st inning
    one <- one + 1
    one_run <- rbind(one_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 2) {
    # 2nd inning
    two = two + 1
    two_run <- rbind(two_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 3) {
    # 3rd inning
    three = three + 1
    three_run <- rbind(three_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 4) {
    # 4th inning
    four = four + 1
    four_run <- rbind(four_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 5) {
    # 5th inning
    five = five + 1
    five_run <- rbind(five_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 6) {
    # 6th inning
    six = six + 1
    six_run <- rbind(six_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 7) {
    # 7th inning
    seven = seven + 1
    seven_run <- rbind(seven_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 8) {
    # 8th inning
    eight = eight + 1
    eight_run <- rbind(eight_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) >= 9) {
    # 9th inning or more
    nine = nine + 1
    nine_run <- rbind(nine_run, linescore[i,])
  }
}

result = matrix(0, nrow = 8, ncol = 8)
total = matrix(0, nrow = 8, ncol = 8)

# 1st Inning
one_run$inning <- as.numeric(as.character(one_run$inning))
one_run$home_inning_runs <- as.numeric(as.character(one_run$home_inning_runs))
one_run$away_inning_runs <- as.numeric(as.character(one_run$away_inning_runs))
one_run$url <- NULL
j = 1
for (i in 1:nrow(one_run)) {
  diff = abs(one_run[i,2] - one_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (one_run[i,2] > one_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1
    }
  }
  # away team leads and eventually away team wins
  else if (one_run[i,2] < one_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 2nd Inning
two_run$inning <- as.numeric(as.character(two_run$inning))
two_run$home_inning_runs <- as.numeric(as.character(two_run$home_inning_runs))
two_run$away_inning_runs <- as.numeric(as.character(two_run$away_inning_runs))
two_run$url <- NULL
j = 2
for (i in 1:nrow(two_run)) {
  diff = abs(two_run[i,2] - two_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (two_run[i,2] > two_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1
    }
  }
  # away team leads and eventually away team wins
  else if (two_run[i,2] < two_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 3rd Inning
three_run$inning <- as.numeric(as.character(three_run$inning))
three_run$home_inning_runs <- as.numeric(as.character(three_run$home_inning_runs))
three_run$away_inning_runs <- as.numeric(as.character(three_run$away_inning_runs))
three_run$url <- NULL
j = 3
for (i in 1:nrow(three_run)) {
  diff = abs(three_run[i,2] - three_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (three_run[i,2] > three_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (three_run[i,2] < three_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 4th Inning
four_run$inning <- as.numeric(as.character(four_run$inning))
four_run$home_inning_runs <- as.numeric(as.character(four_run$home_inning_runs))
four_run$away_inning_runs <- as.numeric(as.character(four_run$away_inning_runs))
four_run$url <- NULL
j = 4
for (i in 1:nrow(four_run)) {
  diff = abs(four_run[i,2] - four_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (four_run[i,2] > four_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (four_run[i,2] < four_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 5th Inning
five_run$inning <- as.numeric(as.character(five_run$inning))
five_run$home_inning_runs <- as.numeric(as.character(five_run$home_inning_runs))
five_run$away_inning_runs <- as.numeric(as.character(five_run$away_inning_runs))
five_run$url <- NULL
j = 5
for (i in 1:nrow(five_run)) {
  diff = abs(five_run[i,2] - five_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (five_run[i,2] > five_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (five_run[i,2] < five_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 6th Inning
six_run$inning <- as.numeric(as.character(six_run$inning))
six_run$home_inning_runs <- as.numeric(as.character(six_run$home_inning_runs))
six_run$away_inning_runs <- as.numeric(as.character(six_run$away_inning_runs))
six_run$url <- NULL
j = 6
for (i in 1:nrow(six_run)) {
  diff = abs(six_run[i,2] - six_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (six_run[i,2] > six_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (six_run[i,2] < six_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 7th Inning
seven_run$inning <- as.numeric(as.character(seven_run$inning))
seven_run$home_inning_runs <- as.numeric(as.character(seven_run$home_inning_runs))
seven_run$away_inning_runs <- as.numeric(as.character(seven_run$away_inning_runs))
seven_run$url <- NULL
j = 7
for (i in 1:nrow(seven_run)) {
  diff = abs(seven_run[i,2] - seven_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (seven_run[i,2] > seven_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (seven_run[i,2] < seven_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 8th Inning
eight_run$inning <- as.numeric(as.character(eight_run$inning))
eight_run$home_inning_runs <- as.numeric(as.character(eight_run$home_inning_runs))
eight_run$away_inning_runs <- as.numeric(as.character(eight_run$away_inning_runs))
eight_run$url <- NULL
j = 8
for (i in 1:nrow(eight_run)) {
  diff = abs(eight_run[i,2] - eight_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (eight_run[i,2] > eight_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (eight_run[i,2] < eight_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# result: number of games lead for diff and eventually win
write.csv(result, "Lead_AL_result_2014.csv")
# total: number of games has lead
write.csv(total, "Lead_AL_total_2014.csv")

setwd("C:/Users/Jaewon/Desktop/AL")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

# game$home_league_id -> 103 for AL, 104 for NL

urls <- makeUrls(start = "2015-04-05", end = "2015-10-04")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid_AL_15_lead <- c()

## Find valid games. Count games that marked "Final"
count = 0
for (i in 1:nrow(game)) {
  if ((game$status[i] == "Final") & (game$home_league_id[i] == 103)) {
    valid_gid_AL_15_lead <- c(valid_gid_AL_15_lead, toString(game$url[i]))
  }
}

# Process again for valid games
valid_xml <- XML2Obs(paste0(valid_gid_AL_15_lead))
valid <- collapse_obs(valid_xml)
valid_game <- data.frame(valid$game)
linescore <- data.frame(valid$`game//linescore`)

# Store all final scores for both home_team and away_team
home_team_runs <- valid_game$home_team_runs
away_team_runs <- valid_game$away_team_runs
total_score <- data.frame(home_team_runs, away_team_runs)
total_score$home_team_runs <- as.numeric(as.character(total_score$home_team_runs))
total_score$away_team_runs <- as.numeric(as.character(total_score$away_team_runs))

# Store all inning by inning score 
one = 0
two = 0
three = 0
four = 0
five = 0
six = 0
seven = 0
eight = 0
nine = 0

one_run = c()
two_run = c()
three_run = c()
four_run = c()
five_run = c()
six_run = c()
seven_run = c()
eight_run = c()
nine_run = c()

for (i in 1:nrow(linescore)) {
  if (as.numeric(as.character(linescore[i,1])) == 1) {
    # 1st inning
    one <- one + 1
    one_run <- rbind(one_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 2) {
    # 2nd inning
    two = two + 1
    two_run <- rbind(two_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 3) {
    # 3rd inning
    three = three + 1
    three_run <- rbind(three_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 4) {
    # 4th inning
    four = four + 1
    four_run <- rbind(four_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 5) {
    # 5th inning
    five = five + 1
    five_run <- rbind(five_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 6) {
    # 6th inning
    six = six + 1
    six_run <- rbind(six_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 7) {
    # 7th inning
    seven = seven + 1
    seven_run <- rbind(seven_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 8) {
    # 8th inning
    eight = eight + 1
    eight_run <- rbind(eight_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) >= 9) {
    # 9th inning or more
    nine = nine + 1
    nine_run <- rbind(nine_run, linescore[i,])
  }
}

result = matrix(0, nrow = 8, ncol = 8)
total = matrix(0, nrow = 8, ncol = 8)

# 1st Inning
one_run$inning <- as.numeric(as.character(one_run$inning))
one_run$home_inning_runs <- as.numeric(as.character(one_run$home_inning_runs))
one_run$away_inning_runs <- as.numeric(as.character(one_run$away_inning_runs))
one_run$url <- NULL
j = 1
for (i in 1:nrow(one_run)) {
  diff = abs(one_run[i,2] - one_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (one_run[i,2] > one_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1
    }
  }
  # away team leads and eventually away team wins
  else if (one_run[i,2] < one_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 2nd Inning
two_run$inning <- as.numeric(as.character(two_run$inning))
two_run$home_inning_runs <- as.numeric(as.character(two_run$home_inning_runs))
two_run$away_inning_runs <- as.numeric(as.character(two_run$away_inning_runs))
two_run$url <- NULL
j = 2
for (i in 1:nrow(two_run)) {
  diff = abs(two_run[i,2] - two_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (two_run[i,2] > two_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1
    }
  }
  # away team leads and eventually away team wins
  else if (two_run[i,2] < two_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 3rd Inning
three_run$inning <- as.numeric(as.character(three_run$inning))
three_run$home_inning_runs <- as.numeric(as.character(three_run$home_inning_runs))
three_run$away_inning_runs <- as.numeric(as.character(three_run$away_inning_runs))
three_run$url <- NULL
j = 3
for (i in 1:nrow(three_run)) {
  diff = abs(three_run[i,2] - three_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (three_run[i,2] > three_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (three_run[i,2] < three_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 4th Inning
four_run$inning <- as.numeric(as.character(four_run$inning))
four_run$home_inning_runs <- as.numeric(as.character(four_run$home_inning_runs))
four_run$away_inning_runs <- as.numeric(as.character(four_run$away_inning_runs))
four_run$url <- NULL
j = 4
for (i in 1:nrow(four_run)) {
  diff = abs(four_run[i,2] - four_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (four_run[i,2] > four_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (four_run[i,2] < four_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 5th Inning
five_run$inning <- as.numeric(as.character(five_run$inning))
five_run$home_inning_runs <- as.numeric(as.character(five_run$home_inning_runs))
five_run$away_inning_runs <- as.numeric(as.character(five_run$away_inning_runs))
five_run$url <- NULL
j = 5
for (i in 1:nrow(five_run)) {
  diff = abs(five_run[i,2] - five_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (five_run[i,2] > five_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (five_run[i,2] < five_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 6th Inning
six_run$inning <- as.numeric(as.character(six_run$inning))
six_run$home_inning_runs <- as.numeric(as.character(six_run$home_inning_runs))
six_run$away_inning_runs <- as.numeric(as.character(six_run$away_inning_runs))
six_run$url <- NULL
j = 6
for (i in 1:nrow(six_run)) {
  diff = abs(six_run[i,2] - six_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (six_run[i,2] > six_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (six_run[i,2] < six_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 7th Inning
seven_run$inning <- as.numeric(as.character(seven_run$inning))
seven_run$home_inning_runs <- as.numeric(as.character(seven_run$home_inning_runs))
seven_run$away_inning_runs <- as.numeric(as.character(seven_run$away_inning_runs))
seven_run$url <- NULL
j = 7
for (i in 1:nrow(seven_run)) {
  diff = abs(seven_run[i,2] - seven_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (seven_run[i,2] > seven_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (seven_run[i,2] < seven_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 8th Inning
eight_run$inning <- as.numeric(as.character(eight_run$inning))
eight_run$home_inning_runs <- as.numeric(as.character(eight_run$home_inning_runs))
eight_run$away_inning_runs <- as.numeric(as.character(eight_run$away_inning_runs))
eight_run$url <- NULL
j = 8
for (i in 1:nrow(eight_run)) {
  diff = abs(eight_run[i,2] - eight_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (eight_run[i,2] > eight_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (eight_run[i,2] < eight_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# result: number of games lead for diff and eventually win
write.csv(result, "Lead_AL_result_2015.csv")
# total: number of games has lead
write.csv(total, "Lead_AL_total_2015.csv")

setwd("C:/Users/Jaewon/Desktop/AL")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

urls <- makeUrls(start = "2013-03-31", end = "2013-09-30")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid_AL <- c()

## Find valid games. Count games that marked "Final" && AL
count = 0
for (i in 1:nrow(game)) {
  if ((game$status[i] == "Final") & (game$home_league_id[i] == 103)) {
    valid_gid_AL <- c(valid_gid_AL, toString(game$url[i]))
  }
}

## Game by game to count tie at some point during each game
gids_table <- data.frame(valid_gid_AL)
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
write.csv(result,"Tie_AL_result_2013.csv")
# Count number of leads for inning by inning score
write.csv(total,"Tie_AL_total_2013.csv")

setwd("C:/Users/Jaewon/Desktop/AL")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

urls <- makeUrls(start = "2014-03-22", end = "2014-09-28")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid_AL <- c()

## Find valid games. Count games that marked "Final" && AL
count = 0
for (i in 1:nrow(game)) {
  if ((game$status[i] == "Final") & (game$home_league_id[i] == 103)) {
    valid_gid_AL <- c(valid_gid_AL, toString(game$url[i]))
  }
}

## Game by game to count tie at some point during each game
gids_table <- data.frame(valid_gid_AL)
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
write.csv(result,"Tie_AL_result_2014.csv")
# Count number of leads for inning by inning score
write.csv(total,"Tie_AL_total_2014.csv")

setwd("C:/Users/Jaewon/Desktop/AL")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

urls <- makeUrls(start = "2015-04-05", end = "2015-10-04")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid_AL <- c()

## Find valid games. Count games that marked "Final" && AL
count = 0
for (i in 1:nrow(game)) {
  if ((game$status[i] == "Final") & (game$home_league_id[i] == 103)) {
    valid_gid_AL <- c(valid_gid_AL, toString(game$url[i]))
  }
}

## Game by game to count tie at some point during each game
gids_table <- data.frame(valid_gid_AL)
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
write.csv(result,"Tie_AL_result_2015.csv")
# Count number of leads for inning by inning score
write.csv(total,"Tie_AL_total_2015.csv")


setwd("C:/Users/Jaewon/Desktop/NL")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

# game$home_league_id -> 103 for AL, 104 for NL

urls <- makeUrls(start = "2013-03-31", end = "2013-09-30")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid_NL_13_lead <- c()

## Find valid games. Count games that marked "Final"
count = 0
for (i in 1:nrow(game)) {
  if ((game$status[i] == "Final") & (game$home_league_id[i] == 104)) {
    valid_gid_NL_13_lead <- c(valid_gid_NL_13_lead, toString(game$url[i]))
  }
}

# Process again for valid games
valid_xml <- XML2Obs(paste0(valid_gid_NL_13_lead))
valid <- collapse_obs(valid_xml)
valid_game <- data.frame(valid$game)
linescore <- data.frame(valid$`game//linescore`)

# Store all final scores for both home_team and away_team
home_team_runs <- valid_game$home_team_runs
away_team_runs <- valid_game$away_team_runs
total_score <- data.frame(home_team_runs, away_team_runs)
total_score$home_team_runs <- as.numeric(as.character(total_score$home_team_runs))
total_score$away_team_runs <- as.numeric(as.character(total_score$away_team_runs))

# Store all inning by inning score 
one = 0
two = 0
three = 0
four = 0
five = 0
six = 0
seven = 0
eight = 0
nine = 0

one_run = c()
two_run = c()
three_run = c()
four_run = c()
five_run = c()
six_run = c()
seven_run = c()
eight_run = c()
nine_run = c()

for (i in 1:nrow(linescore)) {
  if (as.numeric(as.character(linescore[i,1])) == 1) {
    # 1st inning
    one <- one + 1
    one_run <- rbind(one_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 2) {
    # 2nd inning
    two = two + 1
    two_run <- rbind(two_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 3) {
    # 3rd inning
    three = three + 1
    three_run <- rbind(three_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 4) {
    # 4th inning
    four = four + 1
    four_run <- rbind(four_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 5) {
    # 5th inning
    five = five + 1
    five_run <- rbind(five_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 6) {
    # 6th inning
    six = six + 1
    six_run <- rbind(six_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 7) {
    # 7th inning
    seven = seven + 1
    seven_run <- rbind(seven_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 8) {
    # 8th inning
    eight = eight + 1
    eight_run <- rbind(eight_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) >= 9) {
    # 9th inning or more
    nine = nine + 1
    nine_run <- rbind(nine_run, linescore[i,])
  }
}

result = matrix(0, nrow = 8, ncol = 8)
total = matrix(0, nrow = 8, ncol = 8)

# 1st Inning
one_run$inning <- as.numeric(as.character(one_run$inning))
one_run$home_inning_runs <- as.numeric(as.character(one_run$home_inning_runs))
one_run$away_inning_runs <- as.numeric(as.character(one_run$away_inning_runs))
one_run$url <- NULL
j = 1
for (i in 1:nrow(one_run)) {
  diff = abs(one_run[i,2] - one_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (one_run[i,2] > one_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1
    }
  }
  # away team leads and eventually away team wins
  else if (one_run[i,2] < one_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 2nd Inning
two_run$inning <- as.numeric(as.character(two_run$inning))
two_run$home_inning_runs <- as.numeric(as.character(two_run$home_inning_runs))
two_run$away_inning_runs <- as.numeric(as.character(two_run$away_inning_runs))
two_run$url <- NULL
j = 2
for (i in 1:nrow(two_run)) {
  diff = abs(two_run[i,2] - two_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (two_run[i,2] > two_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1
    }
  }
  # away team leads and eventually away team wins
  else if (two_run[i,2] < two_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 3rd Inning
three_run$inning <- as.numeric(as.character(three_run$inning))
three_run$home_inning_runs <- as.numeric(as.character(three_run$home_inning_runs))
three_run$away_inning_runs <- as.numeric(as.character(three_run$away_inning_runs))
three_run$url <- NULL
j = 3
for (i in 1:nrow(three_run)) {
  diff = abs(three_run[i,2] - three_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (three_run[i,2] > three_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (three_run[i,2] < three_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 4th Inning
four_run$inning <- as.numeric(as.character(four_run$inning))
four_run$home_inning_runs <- as.numeric(as.character(four_run$home_inning_runs))
four_run$away_inning_runs <- as.numeric(as.character(four_run$away_inning_runs))
four_run$url <- NULL
j = 4
for (i in 1:nrow(four_run)) {
  diff = abs(four_run[i,2] - four_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (four_run[i,2] > four_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (four_run[i,2] < four_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 5th Inning
five_run$inning <- as.numeric(as.character(five_run$inning))
five_run$home_inning_runs <- as.numeric(as.character(five_run$home_inning_runs))
five_run$away_inning_runs <- as.numeric(as.character(five_run$away_inning_runs))
five_run$url <- NULL
j = 5
for (i in 1:nrow(five_run)) {
  diff = abs(five_run[i,2] - five_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (five_run[i,2] > five_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (five_run[i,2] < five_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 6th Inning
six_run$inning <- as.numeric(as.character(six_run$inning))
six_run$home_inning_runs <- as.numeric(as.character(six_run$home_inning_runs))
six_run$away_inning_runs <- as.numeric(as.character(six_run$away_inning_runs))
six_run$url <- NULL
j = 6
for (i in 1:nrow(six_run)) {
  diff = abs(six_run[i,2] - six_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (six_run[i,2] > six_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (six_run[i,2] < six_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 7th Inning
seven_run$inning <- as.numeric(as.character(seven_run$inning))
seven_run$home_inning_runs <- as.numeric(as.character(seven_run$home_inning_runs))
seven_run$away_inning_runs <- as.numeric(as.character(seven_run$away_inning_runs))
seven_run$url <- NULL
j = 7
for (i in 1:nrow(seven_run)) {
  diff = abs(seven_run[i,2] - seven_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (seven_run[i,2] > seven_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (seven_run[i,2] < seven_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 8th Inning
eight_run$inning <- as.numeric(as.character(eight_run$inning))
eight_run$home_inning_runs <- as.numeric(as.character(eight_run$home_inning_runs))
eight_run$away_inning_runs <- as.numeric(as.character(eight_run$away_inning_runs))
eight_run$url <- NULL
j = 8
for (i in 1:nrow(eight_run)) {
  diff = abs(eight_run[i,2] - eight_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (eight_run[i,2] > eight_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (eight_run[i,2] < eight_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# result: number of games lead for diff and eventually win
write.csv(result, "Lead_NL_result_2013.csv")
# total: number of games has lead
write.csv(total, "Lead_NL_total_2013.csv")

setwd("C:/Users/Jaewon/Desktop/NL")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

# game$home_league_id -> 103 for AL, 104 for NL

urls <- makeUrls(start = "2014-03-22", end = "2014-09-28")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid_NL_14_lead <- c()

## Find valid games. Count games that marked "Final"
count = 0
for (i in 1:nrow(game)) {
  if ((game$status[i] == "Final") & (game$home_league_id[i] == 104)) {
    valid_gid_NL_14_lead <- c(valid_gid_NL_14_lead, toString(game$url[i]))
  }
}

# Process again for valid games
valid_xml <- XML2Obs(paste0(valid_gid_NL_14_lead))
valid <- collapse_obs(valid_xml)
valid_game <- data.frame(valid$game)
linescore <- data.frame(valid$`game//linescore`)

# Store all final scores for both home_team and away_team
home_team_runs <- valid_game$home_team_runs
away_team_runs <- valid_game$away_team_runs
total_score <- data.frame(home_team_runs, away_team_runs)
total_score$home_team_runs <- as.numeric(as.character(total_score$home_team_runs))
total_score$away_team_runs <- as.numeric(as.character(total_score$away_team_runs))

# Store all inning by inning score 
one = 0
two = 0
three = 0
four = 0
five = 0
six = 0
seven = 0
eight = 0
nine = 0

one_run = c()
two_run = c()
three_run = c()
four_run = c()
five_run = c()
six_run = c()
seven_run = c()
eight_run = c()
nine_run = c()

for (i in 1:nrow(linescore)) {
  if (as.numeric(as.character(linescore[i,1])) == 1) {
    # 1st inning
    one <- one + 1
    one_run <- rbind(one_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 2) {
    # 2nd inning
    two = two + 1
    two_run <- rbind(two_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 3) {
    # 3rd inning
    three = three + 1
    three_run <- rbind(three_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 4) {
    # 4th inning
    four = four + 1
    four_run <- rbind(four_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 5) {
    # 5th inning
    five = five + 1
    five_run <- rbind(five_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 6) {
    # 6th inning
    six = six + 1
    six_run <- rbind(six_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 7) {
    # 7th inning
    seven = seven + 1
    seven_run <- rbind(seven_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 8) {
    # 8th inning
    eight = eight + 1
    eight_run <- rbind(eight_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) >= 9) {
    # 9th inning or more
    nine = nine + 1
    nine_run <- rbind(nine_run, linescore[i,])
  }
}

result = matrix(0, nrow = 8, ncol = 8)
total = matrix(0, nrow = 8, ncol = 8)

# 1st Inning
one_run$inning <- as.numeric(as.character(one_run$inning))
one_run$home_inning_runs <- as.numeric(as.character(one_run$home_inning_runs))
one_run$away_inning_runs <- as.numeric(as.character(one_run$away_inning_runs))
one_run$url <- NULL
j = 1
for (i in 1:nrow(one_run)) {
  diff = abs(one_run[i,2] - one_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (one_run[i,2] > one_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1
    }
  }
  # away team leads and eventually away team wins
  else if (one_run[i,2] < one_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 2nd Inning
two_run$inning <- as.numeric(as.character(two_run$inning))
two_run$home_inning_runs <- as.numeric(as.character(two_run$home_inning_runs))
two_run$away_inning_runs <- as.numeric(as.character(two_run$away_inning_runs))
two_run$url <- NULL
j = 2
for (i in 1:nrow(two_run)) {
  diff = abs(two_run[i,2] - two_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (two_run[i,2] > two_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1
    }
  }
  # away team leads and eventually away team wins
  else if (two_run[i,2] < two_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 3rd Inning
three_run$inning <- as.numeric(as.character(three_run$inning))
three_run$home_inning_runs <- as.numeric(as.character(three_run$home_inning_runs))
three_run$away_inning_runs <- as.numeric(as.character(three_run$away_inning_runs))
three_run$url <- NULL
j = 3
for (i in 1:nrow(three_run)) {
  diff = abs(three_run[i,2] - three_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (three_run[i,2] > three_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (three_run[i,2] < three_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 4th Inning
four_run$inning <- as.numeric(as.character(four_run$inning))
four_run$home_inning_runs <- as.numeric(as.character(four_run$home_inning_runs))
four_run$away_inning_runs <- as.numeric(as.character(four_run$away_inning_runs))
four_run$url <- NULL
j = 4
for (i in 1:nrow(four_run)) {
  diff = abs(four_run[i,2] - four_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (four_run[i,2] > four_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (four_run[i,2] < four_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 5th Inning
five_run$inning <- as.numeric(as.character(five_run$inning))
five_run$home_inning_runs <- as.numeric(as.character(five_run$home_inning_runs))
five_run$away_inning_runs <- as.numeric(as.character(five_run$away_inning_runs))
five_run$url <- NULL
j = 5
for (i in 1:nrow(five_run)) {
  diff = abs(five_run[i,2] - five_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (five_run[i,2] > five_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (five_run[i,2] < five_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 6th Inning
six_run$inning <- as.numeric(as.character(six_run$inning))
six_run$home_inning_runs <- as.numeric(as.character(six_run$home_inning_runs))
six_run$away_inning_runs <- as.numeric(as.character(six_run$away_inning_runs))
six_run$url <- NULL
j = 6
for (i in 1:nrow(six_run)) {
  diff = abs(six_run[i,2] - six_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (six_run[i,2] > six_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (six_run[i,2] < six_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 7th Inning
seven_run$inning <- as.numeric(as.character(seven_run$inning))
seven_run$home_inning_runs <- as.numeric(as.character(seven_run$home_inning_runs))
seven_run$away_inning_runs <- as.numeric(as.character(seven_run$away_inning_runs))
seven_run$url <- NULL
j = 7
for (i in 1:nrow(seven_run)) {
  diff = abs(seven_run[i,2] - seven_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (seven_run[i,2] > seven_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (seven_run[i,2] < seven_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 8th Inning
eight_run$inning <- as.numeric(as.character(eight_run$inning))
eight_run$home_inning_runs <- as.numeric(as.character(eight_run$home_inning_runs))
eight_run$away_inning_runs <- as.numeric(as.character(eight_run$away_inning_runs))
eight_run$url <- NULL
j = 8
for (i in 1:nrow(eight_run)) {
  diff = abs(eight_run[i,2] - eight_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (eight_run[i,2] > eight_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (eight_run[i,2] < eight_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# result: number of games lead for diff and eventually win
write.csv(result, "Lead_NL_result_2014.csv")
# total: number of games has lead
write.csv(total, "Lead_NL_total_2014.csv")

setwd("C:/Users/Jaewon/Desktop/NL")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

# game$home_league_id -> 103 for AL, 104 for NL

urls <- makeUrls(start = "2015-04-05", end = "2015-10-04")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid_NL_15_lead <- c()

## Find valid games. Count games that marked "Final"
count = 0
for (i in 1:nrow(game)) {
  if ((game$status[i] == "Final") & (game$home_league_id[i] == 104)) {
    valid_gid_NL_15_lead <- c(valid_gid_NL_15_lead, toString(game$url[i]))
  }
}

# Process again for valid games
valid_xml <- XML2Obs(paste0(valid_gid_NL_15_lead))
valid <- collapse_obs(valid_xml)
valid_game <- data.frame(valid$game)
linescore <- data.frame(valid$`game//linescore`)

# Store all final scores for both home_team and away_team
home_team_runs <- valid_game$home_team_runs
away_team_runs <- valid_game$away_team_runs
total_score <- data.frame(home_team_runs, away_team_runs)
total_score$home_team_runs <- as.numeric(as.character(total_score$home_team_runs))
total_score$away_team_runs <- as.numeric(as.character(total_score$away_team_runs))

# Store all inning by inning score 
one = 0
two = 0
three = 0
four = 0
five = 0
six = 0
seven = 0
eight = 0
nine = 0

one_run = c()
two_run = c()
three_run = c()
four_run = c()
five_run = c()
six_run = c()
seven_run = c()
eight_run = c()
nine_run = c()

for (i in 1:nrow(linescore)) {
  if (as.numeric(as.character(linescore[i,1])) == 1) {
    # 1st inning
    one <- one + 1
    one_run <- rbind(one_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 2) {
    # 2nd inning
    two = two + 1
    two_run <- rbind(two_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 3) {
    # 3rd inning
    three = three + 1
    three_run <- rbind(three_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 4) {
    # 4th inning
    four = four + 1
    four_run <- rbind(four_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 5) {
    # 5th inning
    five = five + 1
    five_run <- rbind(five_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 6) {
    # 6th inning
    six = six + 1
    six_run <- rbind(six_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 7) {
    # 7th inning
    seven = seven + 1
    seven_run <- rbind(seven_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) == 8) {
    # 8th inning
    eight = eight + 1
    eight_run <- rbind(eight_run, linescore[i,])
  } else if (as.numeric(as.character(linescore[i,1])) >= 9) {
    # 9th inning or more
    nine = nine + 1
    nine_run <- rbind(nine_run, linescore[i,])
  }
}

result = matrix(0, nrow = 8, ncol = 8)
total = matrix(0, nrow = 8, ncol = 8)

# 1st Inning
one_run$inning <- as.numeric(as.character(one_run$inning))
one_run$home_inning_runs <- as.numeric(as.character(one_run$home_inning_runs))
one_run$away_inning_runs <- as.numeric(as.character(one_run$away_inning_runs))
one_run$url <- NULL
j = 1
for (i in 1:nrow(one_run)) {
  diff = abs(one_run[i,2] - one_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (one_run[i,2] > one_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1
    }
  }
  # away team leads and eventually away team wins
  else if (one_run[i,2] < one_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 2nd Inning
two_run$inning <- as.numeric(as.character(two_run$inning))
two_run$home_inning_runs <- as.numeric(as.character(two_run$home_inning_runs))
two_run$away_inning_runs <- as.numeric(as.character(two_run$away_inning_runs))
two_run$url <- NULL
j = 2
for (i in 1:nrow(two_run)) {
  diff = abs(two_run[i,2] - two_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (two_run[i,2] > two_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1
    }
  }
  # away team leads and eventually away team wins
  else if (two_run[i,2] < two_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 3rd Inning
three_run$inning <- as.numeric(as.character(three_run$inning))
three_run$home_inning_runs <- as.numeric(as.character(three_run$home_inning_runs))
three_run$away_inning_runs <- as.numeric(as.character(three_run$away_inning_runs))
three_run$url <- NULL
j = 3
for (i in 1:nrow(three_run)) {
  diff = abs(three_run[i,2] - three_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (three_run[i,2] > three_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (three_run[i,2] < three_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 4th Inning
four_run$inning <- as.numeric(as.character(four_run$inning))
four_run$home_inning_runs <- as.numeric(as.character(four_run$home_inning_runs))
four_run$away_inning_runs <- as.numeric(as.character(four_run$away_inning_runs))
four_run$url <- NULL
j = 4
for (i in 1:nrow(four_run)) {
  diff = abs(four_run[i,2] - four_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (four_run[i,2] > four_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (four_run[i,2] < four_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 5th Inning
five_run$inning <- as.numeric(as.character(five_run$inning))
five_run$home_inning_runs <- as.numeric(as.character(five_run$home_inning_runs))
five_run$away_inning_runs <- as.numeric(as.character(five_run$away_inning_runs))
five_run$url <- NULL
j = 5
for (i in 1:nrow(five_run)) {
  diff = abs(five_run[i,2] - five_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (five_run[i,2] > five_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (five_run[i,2] < five_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 6th Inning
six_run$inning <- as.numeric(as.character(six_run$inning))
six_run$home_inning_runs <- as.numeric(as.character(six_run$home_inning_runs))
six_run$away_inning_runs <- as.numeric(as.character(six_run$away_inning_runs))
six_run$url <- NULL
j = 6
for (i in 1:nrow(six_run)) {
  diff = abs(six_run[i,2] - six_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (six_run[i,2] > six_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (six_run[i,2] < six_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 7th Inning
seven_run$inning <- as.numeric(as.character(seven_run$inning))
seven_run$home_inning_runs <- as.numeric(as.character(seven_run$home_inning_runs))
seven_run$away_inning_runs <- as.numeric(as.character(seven_run$away_inning_runs))
seven_run$url <- NULL
j = 7
for (i in 1:nrow(seven_run)) {
  diff = abs(seven_run[i,2] - seven_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (seven_run[i,2] > seven_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (seven_run[i,2] < seven_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# 8th Inning
eight_run$inning <- as.numeric(as.character(eight_run$inning))
eight_run$home_inning_runs <- as.numeric(as.character(eight_run$home_inning_runs))
eight_run$away_inning_runs <- as.numeric(as.character(eight_run$away_inning_runs))
eight_run$url <- NULL
j = 8
for (i in 1:nrow(eight_run)) {
  diff = abs(eight_run[i,2] - eight_run[i,3])
  if (diff >= 8) {
    diff = 8
  }
  # home team leads and eventually home team wins
  if (eight_run[i,2] > eight_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] > total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
  # away team leads and eventually away team wins
  else if (eight_run[i,2] < eight_run[i,3]) {
    total[diff, j] = total[diff, j] + 1
    if (total_score[i,1] < total_score[i,2]) {
      result[diff,j] = result[diff,j] + 1      
    }
  }
}

# result: number of games lead for diff and eventually win
write.csv(result, "Lead_NL_result_2015.csv")
# total: number of games has lead
write.csv(total, "Lead_NL_total_2015.csv")

setwd("C:/Users/Jaewon/Desktop/NL")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

urls <- makeUrls(start = "2013-03-31", end = "2013-09-30")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid_NL <- c()

## Find valid games. Count games that marked "Final" && AL
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
write.csv(result,"Tie_NL_result_2013.csv")
# Count number of leads for inning by inning score
write.csv(total,"Tie_NL_total_2013.csv")

setwd("C:/Users/Jaewon/Desktop/NL")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

urls <- makeUrls(start = "2014-03-22", end = "2014-09-28")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid_NL <- c()

## Find valid games. Count games that marked "Final" && AL
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
write.csv(result,"Tie_NL_result_2014.csv")
# Count number of leads for inning by inning score
write.csv(total,"Tie_NL_total_2014.csv")

setwd("C:/Users/Jaewon/Desktop/NL")
library(pitchRx)
library(XML2R)
library(dplyr)

# 2013 MLB Season: 2013-03-31 to 2013-09-30
# 2014 MLB Season: 2014-03-22 to 2014-09-28
# 2015 MLB Season: 2015-04-05 to 2015-10-04

urls <- makeUrls(start = "2015-04-05", end = "2015-10-04")
gids <- sub("http://gd2.mlb.com/components/game/mlb/", "", urls)
xml <- XML2Obs(paste0("http://gd2.mlb.com/components/game/mlb/", gids, "/linescore.xml"))
temp <- collapse_obs(xml)
game <- data.frame(temp$game)
valid_gid_NL <- c()

## Find valid games. Count games that marked "Final" && AL
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
write.csv(result,"Tie_NL_result_2015.csv")
# Count number of leads for inning by inning score
write.csv(total,"Tie_NL_total_2015.csv")
