library(tidyverse)
library(dplyr)
library(gdata)
library(readxl)
library(Hmisc)

setwd("~/Documents/GitHub/PBE")
arch_bat <- read_excel("Archetypes.xlsx",sheet = 'Batter')
arch_bat.TPE <- sapply(arch_bat[,3:15], function(x)
  ifelse(x <= 40,x,
         ifelse(x <= 50,40 + ((x - 40) * 2),
                ifelse(x <= 60,20 + 40 + ((x - 50) * 3),
                       ifelse(x <= 70,30 + 20 + 40 + ((x - 60) * 4),
                              ifelse(x <= 80,40 + 30 + 20 + 40 + ((x - 70) * 6),
                                     ifelse(x <= 90,60 + 30 + 20 + 40 + 40 + ((x - 80) * 7),
                                            70 + 60 + 30 + 20 + 40 + 40 + ((x - 90) * 8))))))))
arch_bat <- arch_bat[c(1,2)]
arch_bat.TPE_Totals <- cbind(arch_bat,arch_bat.TPE)
arch_bat.TPE_Totals$Hit.Total_TPE <- rowSums(arch_bat.TPE_Totals[,3:15])
arch_bat.TPE_Totals <- arch_bat.TPE_Totals[c(1,2,16)]

arch_field <- read_excel("Archetypes.xlsx",sheet = 'Fielding')
arch_field.TPE <- sapply(arch_field[,3:7], function(x)
  ifelse(x <= 40,x,
         ifelse(x <= 50,40 + ((x - 40) * 2),
                ifelse(x <= 60,20 + 40 + ((x - 50) * 3),
                       ifelse(x <= 70,30 + 20 + 40 + ((x - 60) * 4),
                              ifelse(x <= 80,40 + 30 + 20 + 40 + ((x - 70) * 6),
                                     ifelse(x <= 90,60 + 30 + 20 + 40 + 40 + ((x - 80) * 7),
                                            70 + 60 + 30 + 20 + 40 + 40 + ((x - 90) * 8))))))))
arch_field <- arch_field[c(1,2)]
arch_field.TPE_Totals <- cbind(arch_field,arch_field.TPE)
arch_field.TPE_Totals$field.Total_TPE <- rowSums(arch_field.TPE_Totals[,3:7])
arch_field.TPE_Totals <- arch_field.TPE_Totals[c(1,2,8)]

arch_CL <- read_excel("Archetypes.xlsx",sheet = 'CL')
arch_SP <- read_excel("Archetypes.xlsx",sheet = 'SP')
DVS_comp <- read_excel("pbecompendium5.17.19.xlsx",range = "DVS!E1:G40")
DVS_comp <- DVS_comp[complete.cases(DVS_comp), ]
SAS_comp <- read_excel("pbecompendium5.17.19.xlsx",range = "SAS!E1:G40")
SAS_comp <- SAS_comp[complete.cases(SAS_comp), ]
VAN_comp <- read_excel("pbecompendium5.17.19.xlsx",range = "VAN!E1:G40")
VAN_comp <- VAN_comp[complete.cases(VAN_comp), ]
UTA_comp <- read_excel("pbecompendium5.17.19.xlsx",range = "UTA!E1:G40")
UTA_comp <- UTA_comp[complete.cases(UTA_comp), ]
OBX_comp <- read_excel("pbecompendium5.17.19.xlsx",range = "OBX!E1:G40")
OBX_comp <- OBX_comp[complete.cases(OBX_comp), ]
NYV_comp <- read_excel("pbecompendium5.17.19.xlsx",range = "NYV!E1:G40")
NYV_comp <- NYV_comp[complete.cases(NYV_comp), ]
PRO_comp <- read_excel("pbecompendium5.17.19.xlsx",range = "PRO!E1:G40")
PRO_comp <- PRO_comp[complete.cases(PRO_comp), ]
FL_comp <- read_excel("pbecompendium5.17.19.xlsx",range = "FL!E1:G40")
FL_comp <- FL_comp[complete.cases(FL_comp), ]
pl_comp <- rbind(DVS_comp,FL_comp,NYV_comp,OBX_comp,PRO_comp,SAS_comp,UTA_comp,VAN_comp)
colnames(pl_comp)[colnames(pl_comp) == 'Player Name'] <- 'Full_name'
pl_comp$Archetype <- gsub("Quick", "Speed",pl_comp$Archetype)
pl_comp$Archetype <- gsub("Speedy", "Speed",pl_comp$Archetype)
pl_comp$Archetype <- gsub("Strong Arm", "Arm",pl_comp$Archetype)
pl_comp$Archetype <- gsub("Finnese", "Finesse",pl_comp$Archetype)
pl_comp$Archetype <- gsub("Balance", "Balanced",pl_comp$Archetype)
pl_comp$Archetype <- gsub("Balancedd", "Balanced",pl_comp$Archetype)


hit.pl_comp <- subset(pl_comp,pl_comp$Position %nin% c("SP","CL"))
hit.pl_comp <- hit.pl_comp %>% separate(Archetype,into = c("b.Archetype","f.Archetype"), sep = "/")
hit.pl_comp$f.Archetype <- ifelse(hit.pl_comp$Position == 'C' & hit.pl_comp$f.Archetype == 'Arm','Arm-C',hit.pl_comp$f.Archetype)
hit.pl_comp <- merge(hit.pl_comp,arch_bat,all.x = TRUE)
hit.pl_comp <- merge(hit.pl_comp,arch_field,all.x = TRUE)
hit.pl_comp <- merge(hit.pl_comp,arch_bat.TPE_Totals, all.x = TRUE)
hit.pl_comp <- merge(hit.pl_comp,arch_field.TPE_Totals, all.x = TRUE)
hit.pl_comp$Archetype_Begin.Total_TPE <- hit.pl_comp$Hit.Total_TPE + hit.pl_comp$field.Total_TPE

sp.pit.pl_comp <- subset(pl_comp,pl_comp$Position == "SP")
colnames(sp.pit.pl_comp)[colnames(sp.pit.pl_comp) == 'Archetype'] <- 'sp.Archetype'
sp.pit.pl_comp <- merge(sp.pit.pl_comp,arch_SP,all.x = TRUE)

cl.pit.pl_comp <- subset(pl_comp,pl_comp$Position == "CL")
colnames(cl.pit.pl_comp)[colnames(cl.pit.pl_comp) == 'Archetype'] <- 'cl.Archetype'
cl.pit.pl_comp <- merge(cl.pit.pl_comp,arch_CL,all.x = TRUE)


headers <- read.csv("pbe_rosters.csv", skip = 11, header = F, nrows = 1, as.is = T)
headers <- trim(headers)
PBE_rosters <- read.csv("pbe_rosters.csv", skip = 14, header = F)
colnames(PBE_rosters) =  headers
PBE_rosters <- PBE_rosters[c(-152)]
PBE_rosters <- PBE_rosters[!is.na(PBE_rosters$`team_id`),]


clean.PBE_rosters <- PBE_rosters[-c(2,7,8:18,22:26,32,38:45,48,50,55:59,70:122,135:151)]
b.handeness <- data.frame(Bats = 1:3, Hand_Bats = c('R','L','S'))
clean.PBE_rosters <- merge(clean.PBE_rosters,b.handeness, all.x = TRUE)

t.handeness <- data.frame(Throws = 1:2, Hand_Throws = c('R','L'))
clean.PBE_rosters <- merge(clean.PBE_rosters,t.handeness, all.x = TRUE)
clean.PBE_rosters$Velocity <- ifelse(clean.PBE_rosters$Position == 1,clean.PBE_rosters$Velocity,0)
clean.PBE_rosters$Position <- NULL
vel <- data.frame(Velocity = 0:20,Velocity_label = c('0','0',
                                                     '80 - 83',
                                                     '83 - 85',
                                                     '84 - 86',
                                                     '85 - 87',
                                                     '86 - 88',
                                                     '87 - 89',
                                                     '88 - 90',
                                                     '89 - 91',
                                                     '90 - 92',
                                                     '91 - 93',
                                                     '92 - 94',
                                                     '93 - 95',
                                                     '94 - 96',
                                                     '95 - 97',
                                                     '96 - 98',
                                                     '97 - 99',
                                                     '98 - 100',
                                                     '99 - 100',
                                                     '100'),TPE_cost = c(0,0,30,30,30,30,30,30,30,50,50,50,50,50,50,75,75,75,75,75,75))
clean.PBE_rosters <- merge(clean.PBE_rosters,vel,all.x = TRUE)
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == '//id'] <- 'Player_ID'
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == 'Infield Range'] <- 'Range'
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == 'Infield Error'] <- 'Error'
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == 'Infield Arm'] <- 'Arm'
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == 'CatcherAbil'] <- 'Catcher Ability'
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == 'Fastball (scale: 0-5)'] <- 'Fastball'
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == 'sac bunt'] <- 'Bunting'
colnames(clean.PBE_rosters)[colnames(clean.PBE_rosters) == 'Ks vR'] <- 'Avoid K vR'
clean.PBE_rosters$Full_name <- paste(clean.PBE_rosters$FirstName,clean.PBE_rosters$LastName)
clean.PBE_rosters <- clean.PBE_rosters[c(50,4,6,5,48,2,47,3,49,1,28,29,9:27,30:46)]

hit.clean.PBE_rosters <- subset(clean.PBE_rosters,clean.PBE_rosters$Velocity == 0)
## reduce columns to hitter only, divide by two, run the function, summeraize to get total
## merge with archetype totals, subtract current total from sum of two archetype totals

hit.current.stats <- hit.clean.PBE_rosters[c(13:25,32:36)]
hit.info <- hit.clean.PBE_rosters[1:8]
hit.current.stats <- round(hit.current.stats[,1:ncol(hit.current.stats)]/2,0)
colnames(hit.current.stats) <- paste("Current_Value", colnames(hit.current.stats), sep = "_")
hitter.audit <- cbind(hit.info,hit.current.stats)

hit.TPE_cost_matrix <- as.data.frame(sapply(hit.current.stats[,1:18], function(x)
  ifelse(x <= 40,x,
         ifelse(x <= 50,40 + ((x - 40) * 2),
                ifelse(x <= 60,20 + 40 + ((x - 50) * 3),
                       ifelse(x <= 70,30 + 20 + 40 + ((x - 60) * 4),
                              ifelse(x <= 80,40 + 30 + 20 + 40 + ((x - 70) * 6),
                                     ifelse(x <= 90,60 + 30 + 20 + 40 + 40 + ((x - 80) * 7),
                                            70 + 60 + 30 + 20 + 40 + 40 + ((x - 90) * 8)))))))))
hit.TPE_cost_matrix$Total_TPE <- rowSums(hit.TPE_cost_matrix[,1:18])
hit.TPE_cost_matrix <- cbind(hit.info, hit.TPE_cost_matrix)
hit.TPE.Totals <- hit.TPE_cost_matrix[,27] 
hit.TPE <- cbind(hit.info, hit.TPE.Totals)



hit.audit <- merge(hit.pl_comp,hit.TPE,all.y = TRUE)
hit.audit <- merge(hit.audit,hitter.audit,all.x = TRUE)
hit.audit$TPE <- hit.audit$hit.TPE.Totals - hit.audit$Archetype_Begin.Total_TPE

sp.audit <- merge(sp.pit.pl_comp,PBE.rosters.audit,all.x = TRUE)
cl.audit <- merge(cl.pit.pl_comp,PBE.rosters.audit,all.x = TRUE)
