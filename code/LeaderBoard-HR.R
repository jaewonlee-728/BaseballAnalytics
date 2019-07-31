setwd("C:/Users/Jaewon/Desktop/Fall 2015/Research - Baseball Analytics/")
library(XML2R)
library(dplyr)
library(splitstackshape)

delim = ","  # or is it "\t" ?
dec = "."    # or is it "," ?
file <- read.csv("example.csv", header=TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)

table <- data.frame(file)


colnames(table) <- c("Yr","Game","Date","Batter","Team","Opp","Pitcher","Score","Inn","RoB","Out", "Pit(Cnt)","RBI","WPA","RE24","LI","Play Description")

write.csv(table, "HR_result.csv")

