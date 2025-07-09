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


#假設:促銷類型和銷量有相關性
unique(CC20_22$Model)
#20~22籌碼中出現過的車種:"NPZ" "SR"  "RE"  "AS" "JDV" "DE" "DET"    "JD" 
unique(CS20_22$Model)
#20~22銷售中出現過的車種:"NPZ" "SR"  "RE"  "AS" "JDV" "DE" "DET"    "JDP"  "ASP"
#選取20~22都有出現的車種
#移除20-CP 和 22-CT
CS20_22 <- subset(CS20_22, subset = (Model != "CP"))
CS20_22 <- subset(CS20_22, subset = (Model != "CT"))
CC20_22 <- subset(CC20_22, subset = (Model != "CP"))
CC20_22 <- subset(CC20_22, subset = (Model != "CT"))

CS_without_0208 <- subset(CS20_22, !(Year == '2021' & (Month == '2' | Month == '8')))
CC_without_0208 <- subset(CC20_22, !(Year == '2021' & (Month == '2' | Month == '8')))

#篩選資料
CC_CS_data <- function(ctype){
  df <- cbind.data.frame(CC20_22 %>% filter(Model == as.character(ctype) & BonusType == '外促') %>% select('Bonus'),
                         CC20_22 %>% filter(Model == as.character(ctype) & BonusType == '專案') %>% select('Bonus'),
                         CC20_22 %>% filter(Model == as.character(ctype) & BonusType == '共同') %>% select('Bonus'),
                         CC20_22 %>% filter(Model == as.character(ctype) & BonusType == '常態') %>% select('Bonus'),
                         CS20_22 %>% filter(Model == as.character(ctype)) %>% select('Sells'))
  colnames(df) = c('外促', '專案', '常態', '共同', 'sales')
  return(df)
}

#總體觀察
CC_CS_total <- cbind.data.frame(
  CC20_22 %>% filter(BonusType == '外促') %>% select('Bonus'),
  CC20_22 %>% filter(BonusType == '專案') %>% select('Bonus'),
  CC20_22 %>% filter(BonusType == '共同') %>% select('Bonus'),
  CC20_22 %>% filter(BonusType == '常態') %>% select('Bonus'),
  CS20_22 %>% select('Sells'))
colnames(CC_CS_total) = c('外促', '專案', '常態', '共同', 'sales')

total_rf <- randomForest(CC_CS_total[, -ncol(CC_CS_total)], CC_CS_total[, ncol(CC_CS_total)], ntree = 100)
importance(total_rf)
total_rf
predicted_values <- predict(total_rf, CC_CS_total)
total_rf_pre <- cbind(CC_CS_total, predicted_values)
colnames(total_rf_pre) = c('外促', '專案', '常態', '共同', 'sales','預測')


CC_CS_wo0208 <- cbind.data.frame(
  CC_without_0208 %>% filter(BonusType == '外促') %>% select('Bonus'),
  CC_without_0208 %>% filter(BonusType == '專案') %>% select('Bonus'),
  CC_without_0208 %>% filter(BonusType == '共同') %>% select('Bonus'),
  CC_without_0208 %>% filter(BonusType == '常態') %>% select('Bonus'),
  CS_without_0208 %>% select('Sells'))
colnames(CC_CS_wo0208) = c('外促', '專案', '常態', '共同', 'sales')


cor(CC_CS_wo0208[c('外促', '專案', '常態', '共同', 'sales')])
cor(CC_CS_total[c('外促', '專案', '常態', '共同', 'sales')])

z_CC_CS_total <- predict(preProcess(CC_CS_total, method = 'range'),CC_CS_total)
z_CC_CS_total_lm = lm(sales ~ (.) ^ 2, z_CC_CS_total)
summary(z_CC_CS_total_lm);z_CC_CS_total_lm
#Adjusted R-squared:  0.4726 
#Adjusted R-squared(without 2021-02 & 2021-08):0.5493


z_CC_CS_total_lm = lm(sales ~ 專案 + 共同 + 常態 + 專案*常態 + 專案*共同 , z_CC_CS_total)
summary(z_CC_CS_total_lm); z_CC_CS_total_lm
#Adjusted R-squared:  0.4376
#Adjusted R-squared(without 2021-02 & 2021-08): 0.4942 

Map(function(f) 
  caret::train(f , data = z_CC_CS_total, method = "lm", 
               trControl = caret::trainControl(method = "LOOCV")),
  c(sales ~ 專案 + 常態 + 共同 + 專案*常態 + 專案*共同)
)
  #RMSE       R-squared   MAE      
#0.1263753  0.4219855   0.0994942


#建立線性模型
CarModel_data <- function(ctype){
  z_cc <- predict(preProcess(CC_CS_data(as.character(ctype)), method = 'range'),
                  CC_CS_data(as.character(ctype)))
  z_20_22 <- rbind.data.frame(z_cc[1:12, ], z_cc[25:36, ])
  return(z_20_22)
}

lm_CarModel <- function(ctype){
  z_cc <- predict(preProcess(CC_CS_data(as.character(ctype)), method = 'range'),
                  CC_CS_data(as.character(ctype)))
  z_20_22 <- rbind.data.frame(z_cc[1:12, ], z_cc[25:36, ])
  z_20_22_lm = lm(sales ~ (.) ^ 2, data = z_20_22)
  return(summary(z_20_22_lm))
  return(z_20_22_lm)
}




##"NPZ"
lm_NPZ <- lm_CarModel('NPZ');lm_NPZ
#LOOCV
Map(function(f) 
  caret::train(f , data = CarModel_data('NPZ'), method = "lm", 
               trControl = caret::trainControl(method = "LOOCV")),
  c(sales ~ (.) ^ 2)
)
#Residual standard error: 0.1596 on 13 degrees of freedom
#Multiple R-squared:  0.6702,	Adjusted R-squared:  0.4165 
#F-statistic: 2.642 on 10 and 13 DF,  p-value: 0.05186
#RMSE       Rsquared   MAE      
#0.2508598  0.2471966  0.1976761

##SR
lm_SR <- lm_CarModel('SR');lm_SR
#LOOCV
Map(function(f) 
  caret::train(f , data = CarModel_data('SR'), method = "lm", 
               trControl = caret::trainControl(method = "LOOCV")),
  c(sales ~ (.) ^ 2)
)
#Residual standard error: 0.09874 on 13 degrees of freedom
#Multiple R-squared:  0.9329,	Adjusted R-squared:  0.8812 
#F-statistic: 18.06 on 10 and 13 DF,  p-value: 4.952e-06 
#RMSE       Rsquared   MAE      
#0.2204929  0.6467528  0.1349516

##RE
lm_RE <- lm_CarModel('RE');lm_RE
#LOOC
Map(function(f) 
  caret::train(f , data = CarModel_data('RE'), method = "lm", 
               trControl = caret::trainControl(method = "LOOCV")),
  c(sales ~ (.) ^ 2)
)
#Residual standard error: 0.1128 on 295 degrees of freedom
#Multiple R-squared:  0.5641,	Adjusted R-squared:  0.5493 
#F-statistic: 38.17 on 10 and 295 DF,  p-value: < 2.2e-16
#RMSE       Rsquared   MAE      
#0.2940177  0.1003976  0.2304305

##AS
lm_AS <- lm_CarModel('AS');lm_AS
#LOOC
Map(function(f) 
  caret::train(f , data = CarModel_data('AS'), method = "lm", 
               trControl = caret::trainControl(method = "LOOCV")),
  c(sales ~ (.) ^ 2)
)
#Residual standard error: 0.1899 on 13 degrees of freedom
#Multiple R-squared:  0.6564,	Adjusted R-squared:  0.3921 
#F-statistic: 2.484 on 10 and 13 DF,  p-value: 0.06342
#RMSE      Rsquared     MAE     
#13.28954  0.002990741  3.958806

##JDV
lm_JDV <- lm_CarModel('JDV');lm_JDV
#LOOC
Map(function(f) 
  caret::train(f , data = CarModel_data('JDV'), method = "lm", 
               trControl = caret::trainControl(method = "LOOCV")),
  c(sales ~ (.) ^ 2)
)
#Residual standard error: 0.257 on 16 degrees of freedom
#Multiple R-squared:  0.5211,	Adjusted R-squared:  0.3116 
#F-statistic: 2.487 on 7 and 16 DF,  p-value: 0.06223

#RMSE      Rsquared     MAE     
#0.3682901  0.06751518  0.2879435

warning()




##特別處理區-DE(外促全為0、共同全為28000)
DE <- CC_CS_data('DE')
DE <- subset(DE, select = c('專案','常態','sales'));DE
DE_20_22 <- predict(preProcess(DE, method = 'range'), DE)
DE_20_22 <- rbind.data.frame(DE_20_22[1:12, ], DE_20_22[25:36, ])
DE_20_22_lm = lm(sales ~ (.) ^ 2 , data = DE)
summary(DE_20_22_lm); DE_20_22_lm
#Adjusted R-squared:  -0.004885 

##特別處理區-DET(外促全為0、共同全為28000)
DET <- CC_CS_data('DET');DET
DET <- subset(DET, select = c('專案','常態','sales'));DET
DET_20_22 <- predict(preProcess(DET, method = 'range'), DET)
DET_20_22 <- rbind.data.frame(DET_20_22[1:12, ], DET_20_22[25:36, ])
DET_20_22_lm = lm(sales ~ (.) ^ 2 , data = DET)
summary(DET_20_22_lm); DET_20_22_lm
#Adjusted R-squared:  0.1533 

##僅SR效果較好
#預測
SR_20_22 <- predict(preProcess(CC_CS_data('SR'), method = 'range'), CC_CS_data('SR'))
SR_20_22 <- rbind.data.frame(SR_20_22[1:12, ], SR_20_22[25:36, ])
SR_20_22_lm = lm(sales ~ (.) ^ 2 , data = SR_20_22)
summary(SR_20_22_lm); SR_20_22_lm
#判斷重要性
caret::varImp(SR_20_22_lm)
#留下較為顯著的常態:共同
SR_20_22_lm = lm(sales ~ 常態 + 共同 + 常態*共同 , data = SR_20_22)
summary(SR_20_22_lm)
predict_sales <- predict(SR_20_22_lm, newdata = SR_20_22[13:24, ])

data_SR21 <- cbind(SR_20_22[13:24, ], predict_sales)
data_SR21 <- cbind(data_SR21, reframe(data_SR21, diff = (data_SR21$sales - data_SR21$predict_sales)))
data_SR21 <- cbind(data_SR21, as.data.frame(CS20_22$Month[1:12]))
colnames(data_SR21)[colnames(data_SR21) == 'CS20_22$Month[1:12]'] <- 'Month'
round(data_SR21,3)

ggplot(data_SR21, aes(x = Month, y = sales)) + 
  geom_line(color = "blue") + 
  geom_line(aes(y = predict_sales), color = "red") + 
  xlab("月份") + ylab("銷量") + scale_x_continuous(breaks = data_SR21$Month, labels = data_SR21$Month)



