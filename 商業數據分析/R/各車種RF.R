library(stats)
library(dplyr)
library(caret)
library(randomForest)
Sys.setlocale("LC_ALL", "Chinese")

#引入籌碼及銷量
CS18_22 <- read.csv(
  "C:/Users/user/Documents/中山/四下/CM505/CMC/AfterProcess/CarsSellsAP.csv",encoding="UTF-8")
CC18_22 <- read.csv(
  "C:/Users/user/Documents/中山/四下/CM505/CMC/AfterProcess/CarsChipsAP18_22.csv",encoding="UTF-8")
#JD、JDV、DE、DET、CT無"共同"，而為競賽


CS20_22 <-
  CS18_22 %>% filter(Year == '2020'|Year =='2021'|Year =='2022')
CC20_22 <-
  CC18_22 %>% filter(X.U.FEFF.Year == '2020'|X.U.FEFF.Year =='2021'|X.U.FEFF.Year =='2022')
#找出CC每個年分18-22都有的車款
oc <- list(
  '2018' = c(unique(filter(CC20_22, X.U.FEFF.Year == '2018')$Model)),
  '2019' = c(unique(filter(CC20_22, X.U.FEFF.Year == '2019')$Model)),
  '2020' = c(unique(filter(CC20_22, X.U.FEFF.Year == '2020')$Model)),
  '2021' = c(unique(filter(CC20_22, X.U.FEFF.Year == '2021')$Model)),
  '2022' = c(unique(filter(CC20_22, X.U.FEFF.Year == '2022')$Model))
)
all_cars <- unique(unlist(oc))
# 找出每個年份都有的車種
common_cars <- all_cars
for (year in names(oc)) {
  common_cars <- intersect(common_cars, oc[[year]])
}
# 列印結果
for (car in common_cars) {
  print(car)
}


#CC "NPZ""SR""RE""AS""JD""DE""DET"
#CS "NPZ""SR""RE""AS""DE" "DET" "JDP""JDV"
#前處理
#保留每年皆有的車款
#CC18_22 <- subset(CC18_22, subset = (Model %in% c("NPZ", "SR","RE", "AS", "DE", "DET")))
#CS18_22 <- subset(CS18_22, subset = (Model %in% c("NPZ", "SR","RE", "AS", "DE", "DET")))
#CS18_22 <- CS18_22 %>%filter(!(Year == 2018 & Month %in% c(1,2,3,4,5,6)))

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

CC_CS_model <- CC_CS[, c(3,5,6,7,8,9)]
CC_CS_model_z <- predict(preProcess(CC_CS_model, method = 'range'),CC_CS_model)

#建立線性模型
lm_of_car <- function(c){
  data <- filter(CC_CS_model_z, model == c)
  CC_CS_lm = lm(sales ~ (外促 + 專案 + 常態 + 共同) ^ 2, data)
  return(summary(CC_CS_lm))
}

unique(CC_CS_model$model)
parameter <- c("NPZ","SR","RE","AS","ASP","DE","DET","JDP","JDV","CP","CT")
all_lm <- Map(lm_of_car, parameter)
print(all_lm)
#Model | Residual standard error | Adjusted R-squared
#NPZ   | 0.06532                 | 0.1211 
#SR    | 0.01093                 | 0.7701 
#RE    | 0.03465                 | 0.1063 
#AS    | 0.05964                 | 0.3059 
#ASP   | 0.01873                 | 0.4386
#DE    | 0.03576                 | 0.2689 
#DET   | 0.06309                 | 0.02989 
#JDP   | 0.1246                  | -0.08507
#JDV   | 0.03702                 | 0.2056 

#建立線性隨機森林模型，依照model
rf_of_car <- function(c){
  data <- filter(CC_CS_model_z, model == c)
  n <- nrow(data)
  set.seed(2023)
  subdata <- sample(seq_len(n), size = round(0.7 * n))
  traindata <- data[subdata, ]
  train_rf<- randomForest(sales ~ (外促 + 專案 + 常態 + 共同) ,data = traindata, mtry = 3)
  return(train_rf)
}#每次選擇三個變數


all_rf <- Map(rf_of_car, parameter)
print(all_rf)

#NPZ: 8.75, SR: 63.32, RE: <0,  AS: 15.42, ASP: 21.29, DE、DET、JDP、JDV皆小於0
#SR在線性森林隨機模型中表現最好


