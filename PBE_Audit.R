library(tidyverse)
library(dplyr)

setwd("~/Documents/GitHub/PBE")

headers <- read.csv("pbe_rosters.csv", skip = 11, header = F, nrows = 1, as.is = T)
PBE_rosters <- read.csv("pbe_rosters.csv", skip = 14, header = F)
colnames(PBE_rosters) =  headers
PBE_rosters <- PBE_rosters[c(-152)]
PBE_rosters <- PBE_rosters[!is.na(PBE_rosters$` team_id`),]


clean.PBE_rosters <- PBE_rosters[-c(2,7,8:18,22:26,32,38:45,48,50,55:59,70:122,135:151)]

colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == 'ID'] <- 'Player_ID'
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == ' Infield Range'] <- 'Range'
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == ' Infield Error'] <- 'Error'
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == ' Infield Arm'] <- 'Arm'
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == ' CatcherAbil'] <- 'Catcher Ability'
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == ' Fastball (scale: 0-5)'] <- 'Fastball'
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == ' sac bunt'] <- 'Bunting'

