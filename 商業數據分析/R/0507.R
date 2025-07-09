library(stats)
library(dplyr)
library(caret)
library(randomForest)

#引入籌碼及銷量
CS20_22 <- read.csv("C:/Users/user/Documents/CM505/CMC/AfterProcess/CarsSellsAP.csv")
CC20_22 <- read.csv("C:/Users/user/Documents/CM505/CMC/AfterProcess/CarsChipsAP.csv")
#JD、JDV、DE、DET、CT無"共同"，而為競賽

#前處理
#保留每年皆有的車款
CC20_22 <- subset(CC20_22, subset = !(Model %in% c("CP", "CT")))
CS20_22 <- subset(CS20_22, subset = !(Model %in% c("CP", "CT")))

#移除21年2月8月的突出資料
#CC_without_0208 <- subset(CC20_22, !(Year == '2021' & (Month == '2' | Month == '8')))
#CS_without_0208 <- subset(CS20_22, !(Year == '2021' & (Month == '2' | Month == '8')))

#保留年月車種車型資訊
CC_CS <- cbind.data.frame(
  CS20_22 %>% select('Year'),
  CS20_22 %>% select('Month'),
  CS20_22 %>% select('Model'),
  CS20_22 %>% select('CarType'),
  CC20_22 %>% filter(BonusType == '外促') %>% select('Bonus'),
  CC20_22 %>% filter(BonusType == '專案') %>% select('Bonus'),
  CC20_22 %>% filter(BonusType == '共同') %>% select('Bonus'),
  CC20_22 %>% filter(BonusType == '常態') %>% select('Bonus'),
  CS20_22 %>% select('Sells'))
colnames(CC_CS) = c('Year','Month','model','CarType','外促', '專案', '共同', '常態', 'sales')

CC_CS <- predict(preProcess(CC_CS, method = 'range'),CC_CS)
#建立模型，用one-hat encoding 將model列入考慮
#CC_CS <- model.matrix( ~ model + 外促 + 專案 + 共同 + 常態 + sales, data = z_CC_CS)
#CC_CS <- CC_CS[,-1]
#one-hat版本的data
#CC_CS <- as.data.frame(CC_CS)
#僅籌碼及銷量的data
only_CC_CS <- CC_CS[,-1:-8]

#線性模型
sales_lm <- lm(sales ~ (.), data = CC_CS)
summary(sales_lm);sales_lm

#隨機森林線性模型
#利用分割數據集評估模型
n <- nrow(CC_CS)
set.seed(2023)
subdata <- sample(seq_len(n), size = round(0.7 * n))
traindata <- CC_CS[subdata,]
testdata <- CC_CS[-subdata,]


train_rf <- randomForest(sales ~ . ,data = traindata, mtry = 5)
importance(train_rf);train_rf
#Mean of squared residuals: 0.004496862
#% Var explained: 84.06
#兄阿，這好像能用啊(喜)
#


traindata_2 <- only_CC_CS[subdata,]
testdata_2 <- only_CC_CS[-subdata,]
train_rf_2 <- randomForest(sales ~ . ,data = traindata_2, mtry = 3)
importance(train_rf);train_rf

#主成分分析降低維度
CC_CS_std <- scale(CC_CS)
pca <- prcomp(CC_CS_std, center = FALSE, scale = FALSE)
summary(pca);biplot(pca)

# 設定採樣方法
ctrl <- trainControl(method = "cv", number = 10)
# 設定 mtry 的範圍
tune.grid <- expand.grid(mtry = 1:13)
# 建立模型
set.seed(2023)
rf_fit <- train(sales ~ (.) , data = CC_CS, method = "rf", tuneGrid = tune.grid, trControl = ctrl)
# 查看最優的 mtry 值
print(rf_fit$bestTune)
#選取2個變數進行隨機森林

z_CC_CS <- predict(preProcess(CC_CS, method = 'range'),CC_CS)
z_CC_CS_lm = lm(sales ~ (.) ^ 2, CC_CS)
summary(z_CC_CS_lm);z_CC_CS_lm

#Residual standard error: 194.7 on 295 degrees of freedom
#Multiple R-squared:  0.5641,	Adjusted R-squared:  0.5493 
#F-statistic: 38.17 on 10 and 295 DF,  p-value: < 2.2e-16

CC_CS_lm_r = lm(sales ~ (專案 + 共同 + 常態 ) ^ 2, CC_CS)
summary(CC_CS_lm_r);CC_CS_lm_r
#Residual standard error: 0.1219 on 317 degrees of freedom
#Multiple R-squared:  0.5472,	Adjusted R-squared:  0.5381 
#F-statistic: 47.48 on 6 and 317 DF,  p-value: < 2.2e-16
#專案、常態、專案:共同、共同:常態 為影響銷量的主要因素

#不移除外促
set.seed(2023)
n <- nrow(z_CC_CS)
subdata <- sample(seq_len(n), size = round(0.7 * n))
traindata <- z_CC_CS[subdata,]
testdata <- z_CC_CS[-subdata,]

train_rf <- randomForest(sales ~ (.) ,data = traindata, mtry = 3)
importance(train_rf);train_rf

#Mean of squared residuals: 0.006192654
#% Var explained: 75.07

#IncNodePurity
#外促     0.1518475
#專案     2.3935248
#共同     0.7262221
#常態     1.5909669

predict_train <-predict(train_rf, traindata, importance = TRUE)
predict_test <- predict(train_rf, testdata, importance = TRUE)

mse_train <- mean((predict_train - traindata$sales) ^ 2);mse_train# 0.002621672
mse_test <- mean((predict_test - testdata$sales) ^ 2);mse_test# 0.008745915


max(CC_CS$外促)
#模擬退火
# 設定優化演算法
utility <- function(x){
  外促 <- x[1]
  專案 <- x[2]
  共同 <- x[3]
  常態 <- x[4]
  cost <- 外促 + 專案 + 共同 + 常態
  if (cost > 208000) {
    return(-Inf) # 超過預算，效用值為負無限大
  } else {
    # 使用隨機森林模型預測銷量
    pred <- predict(train_rf, data.frame(外促 = 外促,
                                         專案 = 專案,
                                         共同 = 共同,
                                         常態 = 常態))
    return(pred)
  }
}

# 設定初始值和範圍
init <- c(0, 0, 0, 0)
lower_bound <- c(0, 0, 0, 0)
upper_bound <- c(26000, 35000, 47600, 99400)

# 執行模擬退火
set.seed(2023)
sa_result <- optim(init, utility, method = "L-BFGS-B", control = list(maxit = 1000, temp = 100),
                   lower = lower_bound, upper = upper_bound)

# 最佳解和效用值
best_solution <- sa_result$par
best_utility <- sa_result$value

# 印出最佳解和效用值
cat("Best solution:", best_solution, "\n")
cat("Best utility value:", best_utility, "\n")



install.packages("pso")
library(pso)
psoptim(par = c(0,0,0,0),  fn = function(x){
  if( x[1] + x[2] + x[3] + x[4]  > 208000) return(0)
  cat("外促 = ", x[1], ", 專案 = ", x[2], ", 共同 = ", x[3], ", 常態 = ", x[4], "\n")
  return(predict(train_rf, newdata = data.frame(外促 = x[1], 專案 = x[2], 共同 = x[3],常態 = x[4] ))) 
}, lower = c(0,0,0,0), upper = c(26000, 35000, 47600, 99400), 
control = list(maxit = 500, s = 20, fnscale = -1))


