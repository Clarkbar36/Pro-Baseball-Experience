library(tidyverse)
library(dplyr)
library(gdata)
library(readxl)

setwd("~/Documents/GitHub/PBE/Audit")
arch_bat <- read_excel("Archetypes.xlsx",sheet = 'Batter')
arch_field <- read_excel("Archetypes.xlsx",sheet = 'Fielding')
arch_CL <- read_excel("Archetypes.xlsx",sheet = 'CL')
arch_SP <- read_excel("Archetypes.xlsx",sheet = 'SP')
test_data <- read_excel("Audit_Test_data.xlsx")

headers <- read.csv("pbe_rosters.csv", skip = 11, header = F, nrows = 1, as.is = T)
headers <- trim(headers)
PBE_rosters <- read.csv("pbe_rosters.csv", skip = 14, header = F)
colnames(PBE_rosters) =  headers
PBE_rosters <- PBE_rosters[c(-152)]
PBE_rosters <- PBE_rosters[!is.na(PBE_rosters$`team_id`),]


clean.PBE_rosters <- PBE_rosters[-c(2,7,8:18,22:26,32,38:45,48,50,55:59,70:122,135:151)]
positions <- data.frame('Position' = 1:10, 'Position_Name' = c('P','C','1B','2B','3B','SS','LF','CF','RF','DH'))
clean.PBE_rosters <- merge(clean.PBE_rosters,positions,by = "Position", all.x = TRUE)

b.handeness <- data.frame(Bats = 1:3, Hand_Bats = c('R','L','S'))
clean.PBE_rosters <- merge(clean.PBE_rosters,b.handeness, all.x = TRUE)

t.handeness <- data.frame(Throws = 1:2, Hand_Throws = c('R','L'))
clean.PBE_rosters <- merge(clean.PBE_rosters,t.handeness, all.x = TRUE)
clean.PBE_rosters$Velocity <- ifelse(clean.PBE_rosters$Position_Name == 'P',clean.PBE_rosters$Velocity,0)


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
                                     '100'))
#add to end of vel df:
# ,TPE_cost = c(0,0,30,30,30,30,30,30,30,50,50,50,50,50,50,75,75,75,75,75,75)

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
clean.PBE_rosters <- clean.PBE_rosters[c(52,5,7,6,48,4,49,3,50,2,1,51,29,30,10:28,31:47)]



PBE_real_stats <- round(clean.PBE_rosters[,15:ncol(clean.PBE_rosters)]/2,0)
colnames(PBE_real_stats) <- paste("Current_Value", colnames(PBE_real_stats), sep = "_")
PBE_info <- clean.PBE_rosters[,1:14]
PBE.rosters.audit <- cbind(PBE_info,PBE_real_stats)
#colnames(PBE.rosters.audit)[colnames(PBE.rosters.audit) == 'Full_name'] <- 'player'

PBE.rosters.audit$pitch_hit <- ifelse(PBE.rosters.audit$Position_Name != 'P','Hitter','Pitcher')
hit.PBE.rosters.audit <- subset(PBE.rosters.audit,PBE.rosters.audit$pitch_hit == 'Hitter')
pitch.PBE.rosters.audit <- subset(PBE.rosters.audit, PBE.rosters.audit$pitch_hit == "Pitcher")

hit.PBE.rosters.audit <- hit.PBE.rosters.audit[-c(11:14,28:33,39:51)]
hit.info <- hit.PBE.rosters.audit[c(1:10)]

hit.TPE <- sapply(hit.PBE.rosters.audit[,11:28], function(x)
  ifelse(x <= 40,x,
         ifelse(x <= 50,40 + ((x - 40) * 2),
                ifelse(x <= 60,20 + 40 + ((x - 50) * 3),
                       ifelse(x <= 70,30 + 20 + 40 + ((x - 60) * 4),
                              ifelse(x <= 80,40 + 30 + 20 + 40 + ((80 - 70) * 6),
                                     ifelse(x <= 90,60 + 30 + 20 + 40 + 40 + ((x - 80) * 7),
                                            70 + 60 + 30 + 20 + 40 + 40 + ((x - 90) * 8))))))))
colnames(hit.TPE) <- paste("TPE", colnames(hit.TPE), sep = "_")
hit.Total.TPE <- cbind(hit.info,hit.TPE)
hit.Total.TPE$TPE_Total <- rowSums(hit.Total.TPE[,11:28])

DV_Rosters <- merge(hit.Total.TPE,test_data)
DV_Rosters <- DV_Rosters %>% separate(Archetype,into = c("b.Archetype","f.Archetype"), sep = "/")
DV_Rosters <- merge(DV_Rosters,arch_bat,all.x = TRUE)
DV_Rosters <- merge(DV_Rosters,arch_field,all.x = TRUE)


DV.hit.TPE_Begin_Cost <- sapply(DV_Rosters[,c(34:46,48:52)], function(x)
  ifelse(x <= 40,x,
         ifelse(x <= 50,40 + ((x - 40) * 2),
                ifelse(x <= 60,20 + 40 + ((x - 50) * 3),
                       ifelse(x <= 70,30 + 20 + 40 + ((x - 60) * 4),
                              ifelse(x <= 80,40 + 30 + 20 + 40 + ((x - 70) * 6),
                                     ifelse(x <= 90,60 + 30 + 20 + 40 + 40 + ((x - 80) * 7),
                                                    70 + 60 + 30 + 20 + 40 + 40 + ((x - 90) * 8))))))))
colnames(DV.hit.TPE_Begin_Cost) <- paste("TPE_Cost", colnames(DV.hit.TPE_Begin_Cost), sep = "_")
DV.Total.TPE <- cbind(DV_Rosters,DV.hit.TPE_Begin_Cost)
DV.Total.TPE$Begin_TPE_Total <- rowSums(DV.Total.TPE[,53:70])
DV.Total.TPE$Total_TPE_Final <- DV.Total.TPE$TPE_Total - DV.Total.TPE$Begin_TPE_Total
# Sample Formula
# TPE_cost <- function(x){
#   if (x <= 40){
#     PBE_real_stats$`Gap vL` <- x;
#   }else if (x <= 50){
#     PBE_real_stats$`Gap vL` <- 40 + ((x - 40) * 2)
#   }else if (x <= 60){
#     PBE_real_stats$`Gap vL` <- 20 + 40 + ((x - 50) * 3)
#   }else if (x <= 70){
#     PBE_real_stats$`Gap vL` <- 30 + 20 + 40 + ((x - 60) * 4)
#   }else if (x <= 80){
#     PBE_real_stats$`Gap vL` <- 40 + 30 + 20 + 40 + ((x - 70) * 6)
#   }else if (x <= 90){
#     PBE_real_stats$`Gap vL` <- 60 + 30 + 20 + 40 + ((x - 80) * 7)
#   }else{
#     PBE_real_stats$`Gap vL` <- 70 + 60 + 30 + 20 + 40 + ((x - 90) * 8)
#   }}
