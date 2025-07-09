library(stats)
library(dplyr)
library(caret)
library(randomForest)

#車輛銷售
CS18_22 <- read.csv(
  "C:/Users/user/Documents/中山/四下/CM505/CMC/AfterProcess/CarsSellsAP.csv",encoding="UTF-8")
#籌碼
colnames(CS18_22) <- c("Model", "CarType", "Category", "Year", "Month", "sales")

CC18_22 <- read.csv(
  "C:/Users/user/Documents/中山/四下/CM505/CMC/AfterProcess/CarsChipsAP18_22.csv",encoding="UTF-8")

CS18_22 <- CS18_22 %>%filter(!(Year == 2018 & Month %in% c(1,2,3,4,5,6)))

unique(CS18_22$CarType)

CS <- filter(CS18_22, CarType == 'ZINGER')
CC <- filter(CC18_22, CarType == 'ZINGER')

#2018/7~2022/12
CC_CS <- cbind.data.frame(
  CS %>% select('Year'),
  CS %>% select('Month'),
  CS %>% select('Model'),
  CS %>% select('CarType'),
  CS %>% select('Category'),
  CC %>% filter(BonusType == '外促') %>% select('Bonus'),
  CC %>% filter(BonusType == '專案') %>% select('Bonus'),
  CC %>% filter(BonusType == '共同') %>% select('Bonus'),
  CC %>% filter(BonusType == '常態') %>% select('Bonus'),
  CS %>% select("sales"))
colnames(CC_CS) = c('Year','Month','model','CarType','Category','外促', '專案', '共同', '常態','sales')

#標準化
CC_CS_z <- predict(preProcess(CC_CS[,6:10], method = 'range'),CC_CS[,6:10])

#線性模型

lm = lm(sales ~ (.), CC_CS_z)
summary(lm);lm
#不太行阿老鐵
#Multiple R-squared:  0.3125,	Adjusted R-squared:  0.1526 
#F-statistic: 1.955 on 10 and 43 DF,  p-value: 0.06337

#利用分割數據集評估模型
n <- nrow(CC_CS_z)
set.seed(2023)
subdata <- sample(seq_len(n), size = round(0.7 * n))
traindata <- CC_CS_z[subdata,]
testdata <- CC_CS_z[-subdata,]

train_rf <- randomForest(sales ~ 外促 + 專案 + 共同 + 常態 ,data = traindata, mtry = 3)
importance(train_rf);train_rf

#     IncNodePurity
#外促     0.5558773
#專案     0.2461036
#共同     0.5957386
#常態     0.1299090
# Mean of squared residuals: 0.04976652
# % Var explained: 2.11

predict_train <-predict(train_rf, traindata, importance = TRUE)
predict_test <- predict(train_rf, testdata, importance = TRUE)

mse_train <- mean((predict_train - traindata$sales) ^ 2);mse_train# 0.01628117
mse_test <- mean((predict_test - testdata$sales) ^ 2);mse_test# 0.04750996

#微過擬合


max(CC_CS$外促)
max(CC_CS$專案)
max(CC_CS$共同)
max(CC_CS$常態)
#最佳化預測
library(pso)
psoptim(par = c(0,0,0,0),  fn = function(x){
  if( x[1] + x[2] + x[3] + x[4]  > 13000 + 14650 + 22290 + 37860) return(0)
  cat("外促 = ", x[1], ", 專案 = ", x[2], ", 共同 = ", x[3], ", 常態 = ", x[4], "\n")
  return(predict(train_rf, newdata = data.frame(外促 = x[1], 專案 = x[2], 共同 = x[3],常態 = x[4] ))) 
}, lower = c(0,0,0,0), upper = c(13000, 14650, 22290, 37860), 
control = list(maxit = 500, s = 20, fnscale = -1))

#$par
#[1] 4132.961 10909.811     0.000     0.000
#$value
#[1] -0.6204686
