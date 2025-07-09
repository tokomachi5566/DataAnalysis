library(stats)
library(dplyr)
library(caret)

library(ggplot2)
library(grid)
library(cowplot)
library(ggpubr)

library(stringr)

#引入資料集
#車輛銷售
CS20_22 <- read.csv("C:/Users/user/Documents/CM505/CMC/AfterProcess/CarsSellsAP.csv")
#籌碼
CC20_22 <- read.csv("C:/Users/user/Documents/CM505/CMC/AfterProcess/CarsChipsAP.csv")
#JD、JDV、DE、DET、CT無"共同"，而為競賽
#實體店詢問數?
OCA18_22 <- read.csv("C:/Users/user/Documents/CM505/CMC/AfterProcess/Offline_Customer_Asking_2018-2022PerMonthAP.csv")

OCA20_22 <- OCA18_22 %>% filter(Year == '2020'|Year == '2021'|Year == '2022') %>% select('Year', 'Month', 'Model', 'NOI')
unique(OCA20_22$Model)
unique(CS20_22$Model)

OCA20_22 <- OCA20_22 %>% filter(Model %in% c("SR","RE","AS","ASP","DE","DET","JDP","CP","CT"))

#找出轉換率(銷量/來店詢問數)
OCA_CS_20_22 <- merge(OCA20_22, CS20_22, by = c('Year', 'Month','Model'), all = FALSE)
data_20_22 <- cbind(OCA_CS_20_22, reframe(OCA_CS_20_22, exc = (OCA_CS_20_22$Sells / OCA_CS_20_22$NOI)))
data_20_22 <- select(data_20_22, c('Year', 'Month','Model','exc'))

#結合籌碼

allcc <- cbind.data.frame(
  CC20_22 %>% filter(BonusType == '外促') %>% select('Bonus'),
  CC20_22 %>% filter(BonusType == '專案') %>% select('Bonus'),
  CC20_22 %>% filter(BonusType == '共同') %>% select('Bonus'),
  CC20_22 %>% filter(BonusType == '常態') %>% select('Bonus'),
  CS20_22 %>% select('Year', 'Month','Model'))
colnames(allcc) = c('外促', '專案', '常態', '共同', 'Year', 'Month','Model')
#結合籌碼
exc_cc <- merge(data_20_22, allcc, by = c('Year', 'Month','Model'), all = FALSE)

exc_cc_m <- select(exc_cc, c('外促', '專案', '常態', '共同','exc'))
exc_cc_lm = lm(exc ~ (.) ^ 2, data = exc_cc_m)
summary(exc_cc_lm); exc_cc_lm
