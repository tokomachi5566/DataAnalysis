library(stats)
library(dplyr)
library(caret)
library(randomForest)

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

#前處理
CS20_22 <- subset(CS20_22, subset = (Model != "CP"))
CS20_22 <- subset(CS20_22, subset = (Model != "CT"))
CC20_22 <- subset(CC20_22, subset = (Model != "CP"))
CC20_22 <- subset(CC20_22, subset = (Model != "CT"))

CC_without_0208 <- subset(CC20_22, !(Year == '2021' & (Month == '2' | Month == '8')))
CS_without_0208 <- subset(CS20_22, !(Year == '2021' & (Month == '2' | Month == '8')))

CC_new <- subset(CC_without_0208, !(Year == '2020' & Month <= 8 & Model == 'ASP'))
CS_new <- subset(CS_without_0208, !(Year == '2020' & Month <= 8 & Model == 'ASP'))
