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

unique(CS18_22$Model)

CS <- filter(CS18_22, Model == 'DET')
CC <- filter(CC18_22, Model == 'DET')

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
#外促完全一致，故移除
CC_CS_z <- predict(preProcess(CC_CS[,6:10], method = 'range'),CC_CS[,6:10])
CC_CS_z <- CC_CS_z[,c(2,3,4,5)]
#線性模型

lm = lm(sales ~ (.) ^ 2, CC_CS_z)
summary(lm);lm
#不太行阿老鐵


#利用分割數據集評估模型
n <- nrow(CC_CS_z)
set.seed(2023)
subdata <- sample(seq_len(n), size = round(0.7 * n))
traindata <- CC_CS_z[subdata,]
testdata <- CC_CS_z[-subdata,]

train_rf <- randomForest(sales ~ . ,data = traindata)
importance(train_rf);train_rf

#     IncNodePurity
#外促     X
#專案     0.174557444
#共同     0.095164358
#常態     0.006923773
# Mean of squared residuals: 0.03124436
# % Var explained: -1.02

predict_train <-predict(train_rf, traindata, importance = TRUE)
predict_test <- predict(train_rf, testdata, importance = TRUE)

mse_train <- mean((predict_train - traindata$sales) ^ 2);mse_train# 0.0238171
mse_test <- mean((predict_test - testdata$sales) ^ 2);mse_test# 0.01134338






