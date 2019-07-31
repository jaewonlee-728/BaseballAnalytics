setwd("C:/Users/Jaewon/Desktop/AL")

temp_13 <- read.csv("Lead_AL_total_2013.csv")
temp_14 <- read.csv("Lead_AL_total_2014.csv")
temp_15 <- read.csv("Lead_AL_total_2015.csv")
total = matrix(0, nrow = 8, ncol = 8)


for (i in 1:nrow(temp_13)) {
  for (j in 2:ncol(temp_13)) {
    total[i,j-1] = temp_13[i,j] + temp_14[i,j] + temp_15[i,j]
  }
}

te_13 <- read.csv("Lead_AL_result_2013.csv")
te_14 <- read.csv("Lead_AL_result_2014.csv")
te_15 <- read.csv("Lead_AL_result_2015.csv")
result = matrix(0, nrow = 8, ncol = 8)

for (i in 1:nrow(temp_13)) {
  for (j in 2:ncol(temp_13)) {
    result[i,j-1] = te_13[i,j] + te_14[i,j] + te_15[i,j]
  }
}

percent = matrix(0, nrow = 8, ncol = 8)
for (i in 1:nrow(percent)) {
  for (j in 1:ncol(percent)) {
    percent[i,j] = result[i,j] / total[i,j]
  }
}
write.csv(total, "combined_Lead_AL_total.csv")
write.csv(result, "combined_Lead_AL_result.csv")
write.csv(percent, "combined_Lead_AL.csv")
