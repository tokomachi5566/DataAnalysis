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

#媒體廣宣預算
#2016~2021
M16_21 <- read.csv("C:/Users/user/Documents/CM505/CMC/AfterProcess/Media2016-2021AP.csv")
#21年每個月
M21_PerMonth <- read.csv("C:/Users/user/Documents/CM505/CMC/AfterProcess/Media2021PerMouthAP.csv")
#22年到11月
M22_PerMonth_to_NOV <- read.csv("C:/Users/user/Documents/CM505/CMC/AfterProcess/Media2022_Jan_to_Nov_AP.csv") 



######################################################################


#車輛銷售
#個別年份每月各車種銷售量折線圖
CSyearsline <- function(y){
  ggplot(filter(CS20_22, Year == y), mapping = aes(x = Month, y = Sells, color = Model)) + 
    ggtitle(y,'各車種銷售折線圖(每月)') + geom_line(size = 0.7) + geom_point(size = 2.5) +
    scale_x_continuous(breaks = CS20_22$Month, labels = CS20_22$Month) + 
    scale_y_continuous(breaks = seq(0, 1500, 100))
}
#個別年份每月單一車種銷售量折線圖
CS_cartypeline <- function(c){
  ggplot(filter(CS20_22, Model == c), mapping = aes(x = Month, y = Sells, color = Model)) + 
    ggtitle(c,'各年份銷售折線圖(每月)') + geom_line(linewidth = 0.7) + geom_point(size = 2.5) +
    scale_x_continuous(breaks = CS20_22$Month, labels = CS20_22$Month) + 
    scale_y_continuous(breaks = seq(0, 1500, 100)) + 
    facet_wrap( ~Year)
}
CS_cartypeline('NPZ')

CS20_22$Sells <- log(CS20_22$Sells) 
#並列
CS20_22linewrap <-
  ggplot(CS20_22, mapping = aes(x = Month, y = Sells, color = Model)) + 
  ggtitle('各年份車種銷售折線圖(每月)') + geom_line(linewidth = 0.7) + geom_point(size = 2.5) +
  scale_x_continuous(breaks = CS20_22$Month, labels = CS20_22$Month) + 
  scale_y_continuous(breaks = seq(0, 1500, 100)) +
  facet_wrap( ~Year)
CS20_22linewrap
#############################################################################
#籌碼
#個別車種年份折線圖
CCline <- function(c,y){
  cy <- str_c(c, as.character(y), sep = " ")
  ggplot(filter(CC20_22, CarType == as.character(c) & Year == y), mapping = aes(x = Month, y = Bonus, color = BonusType)) + 
    ggtitle(cy,'每月籌碼') + geom_line(size = 0.7) + geom_point(size = 2.5) +
    scale_x_continuous(breaks = CC20_22$Month, labels = CC20_22$Month) + 
    scale_y_continuous(breaks = seq(0, 100000, 10000))
}
CCline("NPZ", 2021)

#並列
CCbyCT <- function(y){
  ggplot(filter(CC20_22, Year == y), mapping = aes(x = Month, y = Bonus, color = BonusType)) + 
    ggtitle(y,'各車種每月籌碼') + geom_line(size = 0.7) + geom_point(size = 2.5) +
    scale_x_continuous(breaks = CC20_22$Month, labels = CC20_22$Month) + 
    scale_y_continuous(breaks = seq(0, 100000, 10000)) +
    facet_wrap( ~CarType)
}

CCbyCT(2020)
CCbyCT(2021)
CCbyCT(2022)

CCline_byCtype <- function(c){
  ggplot(filter(CC20_22, Model == as.character(c)), mapping = aes(x = Month, y = Bonus, color = BonusType)) + 
    ggtitle(c,'每月籌碼') + geom_line(linewidth = 0.7) + geom_point(size = 2.5) +
    scale_x_continuous(breaks = CC20_22$Month, labels = CC20_22$Month) + 
    scale_y_continuous(breaks = seq(0, 100000, 10000)) + 
    facet_wrap( ~Year)
}

CCline_byCtype("NPZ")

#各年分
CCline <- function(c){
  ggplot(filter(CC20_22, CarType == as.character(c)), mapping = aes(x = Month, y = Bonus, color = BonusType)) + 
    ggtitle(c,'每月籌碼') + geom_line(size = 0.7) + geom_point(size = 2.5) +
    scale_x_continuous(breaks = CC20_22$Month, labels = CC20_22$Month) + 
    scale_y_continuous(breaks = seq(0, 100000, 10000)) + 
    facet_wrap( ~Year)
}
CCline("NPZ")

#實體店詢問數
#個別年份每月詢問數折線圖
OCAyearsline <- function(y){
  ggplot(filter(OCA18_22, Year == y), mapping = aes(x = Month, y = NOI, color = CarType)) + 
    ggtitle(y,'各車種來店詢問數(每月)') + 
    geom_line(size = 0.7) + geom_point(size = 2.5) +
    scale_x_continuous(breaks = OCA18_22$Month, labels = OCA18_22$Month) + 
    scale_y_continuous(breaks = seq(0, 3000, 200))
}
OCAyearsline(2020)

#並列
OCA_each_year_line <-  
  ggplot(OCA18_22, mapping = aes(x = Month, y = NOI, color = CarType)) + 
  ggtitle(y,'各車種來店詢問數(每月)') + 
  geom_line(size = 0.7) + geom_point(size = 2.5) +
  scale_x_continuous(breaks = OCA18_22$Month, labels = OCA18_22$Month) + 
  scale_y_continuous(breaks = seq(0, 3000, 200)) +
  facet_wrap( ~CarType)

#車種詢問數變化
OCACartypeline <- function(c){
  OCA18_22$Year <- as.character(OCA18_22$Year)
  ggplot(filter(OCA18_22, CarType == as.character(c)),
         mapping = aes(x = Month, y = NOI, group = Year , color = Year)) + 
    ggtitle(c,'歷年來店詢問數(每月)') + geom_line(size = 0.7) + geom_point(size = 2.5) +
    scale_x_continuous(breaks = OCA18_22$Month, labels = OCA18_22$Month) + 
    scale_y_continuous(breaks = seq(0, 3000, 200))
}
OCACartypeline('AS')



#媒體廣宣
#各車種16-21廣宣預算
Myearsline <- function(c){
  ggplot(filter(M16_21, Model == as.character(c)), mapping = aes(x = Year, y = APV, color = MediaType)) + 
    ggtitle(c,'16-21年廣宣預算') + geom_line(size = 0.7) + geom_point(size = 2.5) +
    scale_x_continuous(breaks = M16_21$Year, labels = M16_21$Year) + 
    scale_y_continuous(breaks = seq(0, 35000000, 5000000))
}

Myearsline('Colt Plus')

############################################

#促銷種類與銷售量相關性

#選取20~22都有出現的車款
#移除20-CP 和 22-CT
CS20_22 <- subset(CS20_22, subset = (CarType != "CP"))
CS20_22 <- subset(CS20_22, subset = (CarType != "CT"))
CC20_22 <- subset(CC20_22, subset = (CarType != "CP"))
CC20_22 <- subset(CC20_22, subset = (CarType != "CT"))
#假設:促銷類型和銷量有相關性
shapiro.test(CC_CS_total$共同)
#不分年不分車款
# 2020 = 9*12, 2021 = 9*12, 2022 = 9*12, all = 324
CC_CS_total <- function(){
  df <- cbind.data.frame(CC20_22 %>% filter(BonusType == '外促') %>% select('Bonus'),
                         CC20_22 %>% filter(BonusType == '專案') %>% select('Bonus'),
                         CC20_22 %>% filter(BonusType == '共同') %>% select('Bonus'),
                         CC20_22 %>% filter(BonusType == '常態') %>% select('Bonus'),
                         CS20_22 %>% select("Sells"))
  colnames(df) = c('外促', '專案', '常態', '共同', 'sales')
  return(df)
};
CC_CS_total <- CC_CS_total()
is.numeric(CC_CS_total)

#使用max-min將資料標準化
z_CC_CS_total <- predict(preProcess(CC_CS_total(), method = 'range'), CC_CS_total())
#促銷類型對銷量作圖
gp1 = Map(function(x) ggplot(z_CC_CS_total, aes(z_CC_CS_total[,x], sales) ) + geom_point() + 
           xlab(x) + stat_smooth(method = lm),
         colnames(z_CC_CS_total[,-5]) )
gridExtra::marrangeGrob(gp1, ncol = 4, nrow= 1)

#套入線性模型
z_CC_CS_total_lm = lm(sales ~ ., z_CC_CS_total)
summary(z_CC_CS_total_lm); z_CC_CS_total_lm
#Adjusted R-squared:  0.2685 
#Coefficients:
#(Intercept)  外促         專案         常態         共同  
# 0.20123     -0.05221     -0.34998     -0.30480      0.54888 
#專案、常態、共同呈現顯著水準(***、***、***)

#使用loocv計算模型準確率跟可信度
Sales_model_formula = c(
  sales ~ 外促 + 專案 + 常態 + 共同
)

Map(function(f) 
  caret::train(f , data = z_CC_CS_total, method = "lm", 
               trControl = caret::trainControl(method = "LOOCV")),
  c(sales ~ 外促 + 專案 + 常態 + 共同)
)
#RMSE       Rsquared   MAE      
#0.144017   0.2498193  0.1147593
#微妙

#各籌碼重要性判斷
caret::varImp(z_CC_CS_total_lm)

#依重要性修正模型
z_CC_CS_total_lm_revise = lm(sales ~ 專案 + 常態 + 共同, z_CC_CS_total)
summary(z_CC_CS_total_lm_revise); z_CC_CS_total_lm_revise

#Adjusted R-squared:  0.2686
#Coefficients:
#(Intercept) 專案         常態         共同  
#0.2038      -0.3525      -0.3187       0.5313 



############################################

#依照年度分別查看
CC_CS_alltype_df <- function(y){
  df <- cbind.data.frame(CC20_22 %>% filter(Year == y & BonusType == '外促') %>% select('Bonus'),
                         CC20_22 %>% filter(Year == y & BonusType == '專案') %>% select('Bonus'),
                         CC20_22 %>% filter(Year == y & BonusType == '共同') %>% select('Bonus'),
                         CC20_22 %>% filter(Year == y & BonusType == '常態') %>% select('Bonus'),
                         CS20_22 %>% filter(Year == y) %>% select("Sells"))
  colnames(df) = c('外促', '專案', '常態', '共同', 'sales')
  return(df)
}

#使用max-min標準化
z_CC_CS_alltype <- predict(preProcess(CC_CS_alltype_df(2021), method = 'range'), CC_CS_alltype_df(2021))
#作圖
gp = Map(function(x) ggplot(z_CC_CS_alltype, aes(z_CC_CS_alltype[,x], sales) ) + geom_point() + xlab(x) + stat_smooth(method = lm),
         colnames(z_CC_CS_alltype[,-5]) )
gridExtra::marrangeGrob(gp, ncol = 4, nrow= 1)

#建立線性模型
z_CC_CS_alltype_lm = lm(sales ~ ., z_CC_CS_alltype)
summary(z_CC_CS_alltype_lm); z_CC_CS_alltype_lm

#Coefficients:
#Adjusted R-squared:  0.05189 
#(Intercept)  外促         專案         常態         共同  
#0.36470      0.05513     -0.33509      0.16873     -0.21837  
#僅專案稍微顯著(*)

##使用loocv計算模型準確率跟可信度
Sales_model_formula = c(
  sales ~ 外促 + 專案 + 常態 + 共同
)

Map(function(f) 
  caret::train(f , data = z_CC_CS_alltype, method = "lm", 
               trControl = caret::trainControl(method = "LOOCV")),
  c(sales ~ 外促 + 專案 + 常態 + 共同)
)

#各籌碼重要性判斷
caret::varImp(z_CC_CS_alltype_lm)

#依重要性修正模型
z_CC_CS_alltype_lm_revise = lm(sales ~ 專案 + 常態 + 共同, z_CC_CS_alltype)
summary(z_CC_CS_alltype_lm_revise); z_CC_CS_alltype_lm_revise
#Adjusted R-squared:  -0.01763 (蛤?)
#Coefficients:
#(Intercept)         專案         常態         共同  
#0.45805     -0.19810     -0.09205     -0.02307 



#

############################################

#依照車種查看
CC_CS_data <- function(ctype){
  df <- cbind.data.frame(CC20_22 %>% filter(Model == as.character(ctype) & BonusType == '外促') %>% select('Bonus'),
                         CC20_22 %>% filter(Model == as.character(ctype) & BonusType == '專案') %>% select('Bonus'),
                         CC20_22 %>% filter(Model == as.character(ctype) & BonusType == '共同') %>% select('Bonus'),
                         CC20_22 %>% filter(Model == as.character(ctype) & BonusType == '常態') %>% select('Bonus'),
                         CS20_22 %>% filter(Model == as.character(ctype)) %>% select('Sells'))
  colnames(df) = c('外促', '專案', '常態', '共同', 'sales')
  return(df)
  }

CC_CS_data('NPZ')

#使用max-min標準化
z_CC_CS <- predict(preProcess(CC_CS_data('NPZ'), method = 'range'), CC_CS_data('NPZ'))
#作圖
gp = Map(function(x) ggplot(z_CC_CS, aes(z_CC_CS[,x], sales) ) + geom_point() + xlab(x) + stat_smooth(method = lm),
         colnames(z_CC_CS[,-5]) )
gridExtra::marrangeGrob(gp, ncol = 4, nrow= 1)

#建立線性模型
z_CC_CS_lm = lm(sales ~ ., data = z_CC_CS)
summary(z_CC_CS_lm); z_CC_CS_lm
# R-squared = 0.1978垃圾
#Coefficients:
#(Intercept)  外促         專案         常態         共同  
#0.26813      0.47529     -0.12198      0.09802     -0.35567 
#外促**

##使用loocv計算模型準確率跟可信度
Sales_model_formula = c(
  sales ~ 外促 + 專案 + 常態 + 共同
)

Map(function(f) 
  caret::train(f , data = z_CC_CS, method = "lm", 
               trControl = caret::trainControl(method = "LOOCV")),
  c(sales ~ 外促 + 專案 + 常態 + 共同)
)

#RMSE       Rsquared   MAE      
#0.2123705  0.1171094  0.1677136

#各籌碼重要性判斷
caret::varImp(z_CC_CS_lm)

#依重要性修正模型
z_CC_CS_lm_revise = lm(sales ~ 專案 + 常態 + 共同, z_CC_CS)
summary(z_CC_CS_lm_revise); z_CC_CS_lm_revise
#Adjusted R-squared:  0.224
#Coefficients:
#(Intercept)  外促         共同  
#0.2924       0.4681      -0.3629  


############################################

#各年份及車種分別查看
CC_CS_Ctype_year <- function(y,ctype){
  df <- cbind.data.frame(
    CC20_22 %>% filter(CarType == as.character(ctype) & Year == y & BonusType == '外促') %>% select('Bonus'),
    CC20_22 %>% filter(CarType == as.character(ctype) & Year == y & BonusType == '專案') %>% select('Bonus'),
    CC20_22 %>% filter(CarType == as.character(ctype) & Year == y & BonusType == '共同') %>% select('Bonus'),
    CC20_22 %>% filter(CarType == as.character(ctype) & Year == y & BonusType == '常態') %>% select('Bonus'),
    CS20_22 %>% filter(CarType == as.character(ctype) & Year == y ) %>% select('Sells'))
  colnames(df) = c('外促', '專案', '常態', '共同', 'sales')
  return(df)
}


CC_CS_Ctype_year(2021,'NPZ')

#使用max-min標準化
z_CC_CS_Ctype_year <- predict(preProcess(CC_CS_Ctype_year(2021,'NPZ'), method = 'range'), 
                                   CC_CS_Ctype_year(2021,'NPZ'))
#作圖
gp = Map(function(x) ggplot(z_CC_CS_Ctype_year, aes(z_CC_CS_Ctype_year[,x], sales) ) + 
           geom_point() + xlab(x) + stat_smooth(method = lm),
         colnames(z_CC_CS_Ctype_year[,-5]) )
gridExtra::marrangeGrob(gp, ncol = 4, nrow= 1)

#建立線性模型
z_CC_CS_Ctype_year_lm = lm(sales ~ ., data = z_CC_CS_Ctype_year)
summary(z_CC_CS_Ctype_year_lm); z_CC_CS_Ctype_year_lm
#Adjusted R-squared:  0.5405(阿怎麼你表現最好)
#Coefficients:
#(Intercept)   外促         專案         常態         共同  
#-0.20370      0.60959      0.37990     -0.05005      0.35952 
#皆不顯著

##使用loocv計算模型準確率跟可信度
Sales_model_formula = c(
  sales ~ 外促 + 專案 + 常態 + 共同
)

Map(function(f) 
  caret::train(f , data = z_CC_CS_Ctype_year_lm, method = "lm", 
               trControl = caret::trainControl(method = "LOOCV")),
  c(sales ~ 外促 + 專案 + 常態 + 共同)
)

#RMSE       Rsquared   MAE      
#0.2195214  0.4066277  0.1885887

#各籌碼重要性判斷
caret::varImp(z_CC_CS_Ctype_year_lm)

#依重要性修正模型
z_CC_CS_Ctype_year_lm_revise = lm(sales ~ 專案 + 常態 + 共同, z_CC_CS_Ctype_year)
summary(z_CC_CS_Ctype_year_lm_revise); z_CC_CS_Ctype_year_lm_revise
#Adjusted R-squared:  0.5973 
#Coefficients:
#(Intercept)   外促         專案         共同  
#-0.2384       0.6444       0.3762       0.3520 

#####################################

#重要性?

#競賽跟共同可能要分(依照不同車種EX:商車)
#籌碼變化跟銷量變化的相關
#關注2月及8月的變化
#先找出每年都有的車款進行評估
#先NPZ

#觀察資料後發現，21年的2月和8月為籌碼加強月份
#將20和22年的資料作為基準，建立模型，並帶入21年的籌碼資料，預測其銷量
#以NPZ為例

#將資料照min_max標準化
CC20_22 %>% filter(Model == 'ASP')
CS20_22 %>% filter(Model == 'ASP')
unique(CC20_22 %>% filter(Model == 'DE'))
unique(CS20_22$Model)


z_CC_20_22 <- predict(preProcess(CC_CS_data('AS'), method = 'range'), CC_CS_data('AS'))

Z_NPZ_20_22 <- rbind.data.frame(z_CC_20_22[1:12, ], z_CC_20_22[25:36, ])
Z_NPZ_20_22_lm = lm(sales ~ ., data = Z_NPZ_20_22)
summary(Z_NPZ_20_22_lm); Z_NPZ_20_22_lm
#這模型真爛
#Multiple R-squared:  0.4907,	Adjusted R-squared:  0.3835 
#Coefficients:
#(Intercept)         外促         專案         常態         共同  
#   0.344145     0.584197     0.008252    -0.283063     0.752565  

predict_sales_21 <- predict(Z_NPZ_20_22_lm, newdata = z_CC_20_22[13:24, ])
data_21 <- cbind(z_CC_20_22[13:24, ], predict_sales_21)

data_21 <- cbind(data_21, reframe(data_21, diff = (data_21$sales - data_21$predict_sales_21)))

#計算與平均值的差(sells & 籌碼)
help("unique")

#=IF(B2="NPZ","乘用車",IF(B2="SR","乘用車",IF(B2="RE","乘用車",IF(B2="AS ","乘用車",IF(B2="ASP","乘用車",IF(B2="JD","商用車",IF(B2="JDV","商用車",IF(B2="DE ","商用車",IF(B2="DET ","商用車",IF(B2="CT","商用車",IF(B2="CP","商用車",IF(B2="JDP","商用車"))))))))))))
#=IF(B3="NPZ","COLTPLUS",IF(B3="SR","LANCER",IF(B3="RE","OUTLANDER",IF(B3="AS ","ZINGER",IF(B3="ASP","ZINGER PICK UP",IF(B3="JD","A190",IF(B3="JDV","A180",IF(B3="DE ","DELICA(廂車)",IF(B3="DET ","DELICA(貨車)",IF(B3="CT","中華堅兵",IF(B3="CP","菱利",IF(B3="JDP","A190"))))))))))))


lm_CarModel <- function(ctype){
  z_cc <- predict(preProcess(CC_CS_data(as.character(ctype)), method = 'range'),
                  CC_CS_data(as.character(ctype)))
  z_20_22 <- rbind.data.frame(z_cc[1:12, ], z_cc[25:36, ])
  z_20_22_lm = lm(sales ~ ., data = z_20_22)
  return(summary(z_20_22_lm))
  return(z_20_22_lm)
}

lm_CarModel('AS')

lm_NPZ <- lm_CarModel('NPZ');lm_NPZ
lm_SR <- lm_CarModel('SR');lm_SR
lm_RE <- lm_CarModel('RE');lm_RE
lm_AS <- lm_CarModel('AS');lm_AS
lm_JDV <- lm_CarModel('JDV');lm_JDV



ctype <- list(unique(CS20_22$Model))
Map(function(x) lm_CarModel(x, ctype))

unique(OCA20_22$Model)
unique(CS20_22$Model)

##轉換率
OCA20_22 <- OCA18_22 %>% filter(Year == '2020'|Year == '2021'|Year == '2022') %>% select('Year', 'Month', 'Model', 'NOI')
OCA20_22 <- OCA20_22 %>% filter(Model %in%  c("SR","RE","AS","ASP","DE","DET","JDP","CP","CT"))
OCA20_22 <- group_indices(OCA20_22$Model)
CS20_22_oca <- CS20_22 %>% filter(Model %in%  c("SR","RE","AS","ASP","DE","DET","JDP","CP","CT"))
exc <- cbind(OCA20_22, )
exc <- refram(data_21, diff = (data_21$sales - data_21$predict_sales_21)))