library(stats)
library(dplyr)
library(caret)
library(randomForest)
library(pROC)
library(e1071)

library(ggplot2)
library(grid)
library(cowplot)
library(ggpubr)

library(stringr)

#引入資料集
#車輛銷售
CS18_22 <- read.csv(
  "C:/Users/user/Documents/中山/四下/CM505/CMC/AfterProcess/CarsSellsAP.csv",encoding="UTF-8")
#籌碼
CC18_22 <- read.csv(
  "C:/Users/user/Documents/中山/四下/CM505/CMC/AfterProcess/CarsChipsAP18_22.csv",encoding="UTF-8")
#JD、JDV、DE、DET、CT無"共同"，而為競賽

#前處理
#CS20_22 <- subset(CS20_22, subset = (Model != "CP"))
#CS20_22 <- subset(CS20_22, subset = (Model != "CT"))
#CC20_22 <- subset(CC20_22, subset = (Model != "CP"))
#CC20_22 <- subset(CC20_22, subset = (Model != "CT"))

#標準化後備用


#z_CC20_22 <- cbind.data.frame(CC20_22, predict(preProcess(as.data.frame(CC20_22$Bonus), method = 'range'),as.data.frame(CC20_22$Bonus))) 
#z_CS20_22 <- cbind.data.frame(CS20_22, predict(preProcess(as.data.frame(CS20_22$Sells), method = 'range'),as.data.frame(CS20_22$Sells))) 



#CC_without_0208 <- subset(CC20_22, !(Year == '2021' & (Month == '2' | Month == '8')))
#CS_without_0208 <- subset(CS20_22, !(Year == '2021' & (Month == '2' | Month == '8')))

sales_alltype <- list(
  '2018' = c(unique(filter(CS18_22, year == '2018')$CarType)),
  '2019' = c(unique(filter(CS18_22, year == '2019')$CarType)),
  '2020' = c(unique(filter(CS18_22, year == '2020')$CarType)),
  '2021' = c(unique(filter(CS18_22, year == '2021')$CarType)),
  '2022' = c(unique(filter(CS18_22, year == '2022')$CarType))
);sales_alltype

#[1] "COLTPlUS", "LANCER", "OUTLANDER", "ZINGER", "DELICA(廂車)", "DELICA(貨車)", "A190", "A180"
chips_alltype <- list(
  '2018' = c(unique(filter(CC18_22, Year == '2018')$CarType)),
  '2019' = c(unique(filter(CC18_22, Year == '2019')$CarType)),
  '2020' = c(unique(filter(CC18_22, Year == '2020')$CarType)),
  '2021' = c(unique(filter(CC18_22, Year == '2021')$CarType)),
  '2022' = c(unique(filter(CC18_22, Year == '2022')$CarType))
);chips_alltype
#[1] "COLTPLUS", "LANCER", "OUTLANDER", "ZINGER", "DELICA(廂車)", "DELICA(貨車), "A190", "菱利""

#選定"NPZ" "SR"  "RE"  "AS"  "DE"  "DET" "JDP" "JDV"幾款車種。
#"COLTPLUS", "LANCER", "OUTLANDER", "ZINGER", "DELICA(廂車)", "DELICA(貨車), "A190"

CC18_22 <- subset(CC18_22, 
                  subset = (CarType %in% 
                              c("COLTPLUS", "LANCER", "OUTLANDER", "ZINGER", "DELICA(廂車)", "DELICA(貨車)", "A190")))
CS18_22 <- subset(CS18_22, 
                  subset = (CarType %in% 
                              c("COLTPLUS", "LANCER", "OUTLANDER", "ZINGER", "DELICA(廂車)", "DELICA(貨車)", "A190")))
CS18_22 <- CS18_22 %>%filter(!(year == 2018 & months %in% c(1,2,3,4,5,6)))

CC_CS <- cbind.data.frame(
  CS18_22 %>% select('Year'),
  CS18_22 %>% select('Month'),
  CS18_22 %>% select('Model'),
  CS18_22 %>% select('CarType'),
  CC18_22 %>% filter(BonusType == '外促') %>% select('Bonus'),
  CC18_22 %>% filter(BonusType == '專案') %>% select('Bonus'),
  CC18_22 %>% filter(BonusType == '共同') %>% select('Bonus'),
  CC18_22 %>% filter(BonusType == '常態') %>% select('Bonus'),
  CS18_22 %>% select('Sells'))
colnames(CC_CS) = c('Year','Month','model','CarType','外促', '專案', '共同', '常態', 'sales')
#獨立性
cor.test(CC_CS$外促, CC_CS$專案)#非獨立
cor.test(CC_CS$外促, CC_CS$共同)#非獨立
cor.test(CC_CS$外促, CC_CS$常態)#非獨立
cor.test(CC_CS$專案, CC_CS$共同)#非獨立
cor.test(CC_CS$專案, CC_CS$常態)#非獨立
cor.test(CC_CS$共同, CC_CS$常態)#非獨立

cor_matrix <- cor(CC_CS)
cov_matrix <- cov(CC_CS);cov_matrix
# 印出相關係數矩陣
print(cor_matrix)


#       外促       專案       共同        常態       sales
#外促   1.0000000  0.3844785  0.5171634  0.5310098 -0.1832526
#專案   0.3844785  1.0000000  0.5190044  0.5414325 -0.3377307
#共同   0.5171634  0.5190044  1.0000000  0.6195794 -0.3787773
#常態   0.5310098  0.5414325  0.6195794  1.0000000 -0.0207525
#sales -0.1832526 -0.3377307 -0.3787773 -0.0207525  1.0000000
#CC_CS <- subset(CC_CS, !(外促 == 0 & 專案 == 0 & 常態 == 0 & 共同 == 0))

z_CC_CS <- predict(preProcess(CC_CS, method = 'range'),CC_CS)
z_cor_matrix <- cor(z_CC_CS)

# 印出相關係數矩陣
print(z_cor_matrix)

z_CC_CS_lm = lm(sales ~ (.) ^ 2, CC_CS)
summary(z_CC_CS_lm);z_CC_CS_lm
#Residual standard error: 194.3 on 313 degrees of freedom
#Multiple R-squared:  0.5561,	Adjusted R-squared:  0.5419 
#F-statistic: 39.21 on 10 and 313 DF,  p-value: < 2.2e-16

z_CC_CS_lm = lm(sales ~  (專案 + 常態 + 共同) ^ 2, z_CC_CS)
summary(z_CC_CS_lm);z_CC_CS_lm
#Residual standard error: 0.1139 on 317 degrees of freedom
#Multiple R-squared:  0.5397,	Adjusted R-squared:  0.531 
#F-statistic: 61.95 on 6 and 317 DF,  p-value: < 2.2e-16
#專案、常態、共同為影響銷量的主要因素

#leave one out
Map(function(f) 
  caret::train(f , data = z_CC_CS, method = "glm", 
               trControl = caret::trainControl(method = "LOOCV")),
  c(sales ~  (專案 + 常態 + 共同) ^ 2)
)

#  RMSE       Rsquared   MAE       
# 0.1156684  0.5249024  0.09074419

#移除表現較差的外促
without_OUT <- z_CC_CS
#利用分割數據集評估模型
n <- nrow(without_OUT)
set.seed(2023)
subdata <- sample(seq_len(n), size = round(0.7 * n))
traindata <- without_OUT[subdata,]
testdata <- without_OUT[-subdata,]

train_rf <- randomForest(sales ~ . ,data = traindata, mtry = 3)
importance(train_rf);train_rf

# Mean of squared residuals: 0.007074479
# % Var explained: 74.92

predict_train <-predict(train_rf, traindata, importance = TRUE)
predict_test <- predict(train_rf, testdata, importance = TRUE)

mse_train <- mean((predict_train - traindata$sales) ^ 2);mse_train# 0.003573159
mse_test <- mean((predict_test - testdata$sales) ^ 2);mse_test# 0.005469728
#MSE在train和test資料集中相當接近，模型應有不錯的泛化性

###隨機森林###
#         IncNodePurity
#外促     0.2312787
#專案     2.8120428
#共同     0.8021267
#常態     1.8702097

library(pso)
psoptim(par = c(0,0,0,0),  fn = function(x){
  if( x[1] + x[2] + x[3] + x[4]  > 208000) return(0)
  cat("外促 = ", x[1], ", 專案 = ", x[2], ", 常態 = ", x[3], ", 共同 = ", x[4], "\n")
  return(predict(train_rf, newdata = data.frame(外促 = x[1], 專案 = x[2], 常態 = x[3],共同 = x[4] ))) 
}, lower = c(0,0,0,0), upper = c(26000, 35000, 47600, 99400), 
control = list(maxit = 500, s = 20, fnscale = -1))

#模擬退火
# 設定優化演算法
utility <- function(x){
  外促 <- x[1]
  專案 <- x[2]
  常態 <- x[3]
  共同 <- x[4]
  cost <- 外促 + 專案 + 常態 + 共同
  if (cost > 208000) {
    return(-Inf) # 超過預算，效用值為負無限大
  } else {
    # 使用隨機森林模型預測銷量
    pred <- predict(train_rf, data.frame(外促 = 外促,
                                         專案 = 專案,
                                         常態 = 常態,
                                         共同 = 共同))
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

###備用區
if(FALSE){
CC_CS <- cbind.data.frame(
  CS_without_0208 %>% select('Year'),
  CS_without_0208 %>% select('Month'),
  CS_without_0208 %>% select('Model'),
  CS_without_0208 %>% select('CarType'),
  CC_without_0208 %>% filter(BonusType == '外促') %>% select('Bonus'),
  CC_without_0208 %>% filter(BonusType == '專案') %>% select('Bonus'),
  CC_without_0208 %>% filter(BonusType == '共同') %>% select('Bonus'),
  CC_without_0208 %>% filter(BonusType == '常態') %>% select('Bonus'),
  CS_without_0208 %>% select('Sells'))
colnames(CC_CS) = c('Year','Month','model','CarType','外促', '專案', '共同', '常態', 'sales')

CC_CS <- model.matrix( ~ model + 外促 + 專案 + 共同 + 常態 + sales, data = CC_CS)
CC_CS <- CC_CS[,-1]
CC_CS <- as.data.frame(CC_CS)
}