library(stats)
library(dplyr)
library(caret)
library(randomForest)

#引入籌碼及來店客數
COME <- read.csv(
  "C:/Users/user/Documents/CM505/CMC/AfterProcess/Offline_Customer_Asking_2018-2022PerMonthAP.csv")
CC18_22 <- read.csv(
  "C:/Users/user/Documents/CM505/CMC/AfterProcess/CarsChipsAP18_22.csv")
#JD、JDV、DE、DET、CT無"共同"，而為競賽

#前處理，保留乘用車

COME <- filter(COME, Category == '乘用車');unique(COME$Model)
CC18_22 <- filter(CC18_22, Category == '乘用車');unique(CC18_22$Model)
COME_CC <- merge(COME, CC18_22, 
                 by = c("Year", "Month", "CarType", "Model", "Category"));unique(COME_CC$Model)
#配合籌碼，保留在2018/06後的資料
#Car Type跟Model不統一，調整後僅剩SR、RE、AS

selected_data <- COME_CC[seq(1, nrow(COME_CC), by = 4), ]
selected_data <- selected_data[,1:6]

CC <- cbind.data.frame(
  COME_CC %>% filter(BonusType == '外促') %>% select('Bonus'),
  COME_CC %>% filter(BonusType == '常態') %>% select('Bonus'),
  COME_CC %>% filter(BonusType == '專案') %>% select('Bonus'),
  COME_CC %>% filter(BonusType == '共同') %>% select('Bonus'))
colnames(CC) = c('外促', '常態', '專案', '共同')

NOI <- cbind(selected_data, CC)

cor_matrix <- cor(NOI[,6:10]);cor_matrix
cov_matrix <- cov(NOI[,6:10]);cov_matrix
# 印出相關係數矩陣
#            NOI        外促        常態        專案         共同
#NOI   1.0000000  0.12487714 0.118974592 -0.13992172 -0.211626595
#外促  0.1248771  1.00000000 0.329781257 -0.04604703 -0.061039774
#常態  0.1189746  0.32978126 1.000000000  0.02484782  0.004989426
#專案 -0.1399217 -0.04604703 0.024847824  1.00000000  0.244159749
#共同 -0.2116266 -0.06103977 0.004989426  0.24415975  1.000000000
#相關性整體偏低阿老鐵們

#線性模型阿，最喜歡的模型。
#標準化
z_NOI <- predict(preProcess(NOI, method = 'range'),NOI)

lm_of_car <- function(c){
  data <- filter(NOI, Model == c)
  z_NOI <- predict(preProcess(data, method = 'range'),data)
  NOI_lm = lm(NOI ~ (外促 + 專案 + 常態 + 共同) ^ 2, z_NOI)
  return(summary(NOI_lm))
}

unique(NOI$Model)
parameter <- c("SR","RE","AS")
all_lm <- Map(lm_of_car, parameter)
print(all_lm)

#Model | Residual standard error | Adjusted R-squared
#SR    | 0.4265                  | 0.2931 
#RE    | 0.2652                  | 0.09436  
#AS    | 0.5117                  | 0.3981  
#結果相當感人阿

#不分類看看
NOI_all_lm = lm(NOI ~ (外促 + 專案 + 常態 + 共同) ^ 2, z_NOI)
summary(NOI_all_lm)
#結果同樣感人
#multiple R-squared:  0.128,	Adjusted R-squared:  0.0703 

#隨機森林阿，最喜歡的模型阿。
#建立線性隨機森林模型，依照model
rf_of_car <- function(c){
  data <- filter(NOI, Model == c)
  z_NOI <- predict(preProcess(data, method = 'range'),data)
  n <- nrow(data)
  set.seed(2023)
  subdata <- sample(seq_len(n), size = round(0.7 * n))
  traindata <- z_NOI[subdata, ]
  train_rf<- randomForest(NOI ~ (外促 + 專案 + 常態 + 共同) ,data = traindata, mtry = 3)
  return(importance(train_rf))
}#每次選擇三個變數

all_rf <- Map(rf_of_car, parameter)
print(all_rf)

#Model | Mean of squared residuals | % Var explained
#SR    | 0.02484358                | 49.41
#RE    | 0.03512816                | 49.78
#AS    | 0.03010078                | 33.76  
#其他兩個表現不錯，接近五成，但AS效果相當感人阿兄弟們

#CarType      |SR         RE        AS
#IncNodePurity|
#    外促     | 0.1759828  0.3247797 0.3457774
#    專案     | 0.7102880  1.0161341 0.2941355
#    常態     | 0.2900605  0.6878142 0.4447837
#    共同     | 0.4876445  0.3241162 0.4227508
#整體而言又是如何
n <- nrow(z_NOI)
set.seed(2023)
subdata <- sample(seq_len(n), size = round(0.7 * n))
traindata <- z_NOI[subdata,6:10]
train_rf<- randomForest(NOI ~ (外促 + 專案 + 常態 + 共同) ,data = traindata, mtry = 3)
importance(train_rf);train_rf
# Mean of squared residuals: 0.01999852
#           % Var explained: 63.38
