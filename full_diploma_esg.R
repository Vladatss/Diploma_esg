install.packages("knitr")
install.packages("plm")
install.packages("ggplot2")
install.packages("texreg")
install.packages("tidyverse")
install.packages("tidysynth")
install.packages("dplyr")
install.packages("tableone")
install.packages("tinytable")
install.packages("ggpubr")
install.packages("vioplot")
install.packages("table1")
install.packages("scipub")
install.packages("htmlTable")
install.packages("corrplot")
install.packages("lmtest")
install.packages('mctest')
install.packages("desk")
install.packages("gridExtra")
#install.packages("WeightIt")
library(WeightIt)
#install.packages("MatchIt")
library(MatchIt)
#install.packages("tableone")
library(tableone)
#install.packages("RItools")
library(RItools)
#install.packages("cobalt")
library(cobalt)
#install.packages("readr")
library(readr)
#install.packages("grf")
library(grf) 
#install.packages("caTools")
library(caTools) 
#install.packages("ggplot2")
library(ggplot2)
#install.packages("hdm")
library(hdm)
#install.packages("glmnet")
library(glmnet)
#install.packages("caret")
library(caret)
library(gridExtra)
library(desk)
library(mctest)
library("knitr") 
library("texreg")
library("tidysynth")
library("dplyr")
library("tidyverse")
library("ggplot2") 
library("plm")
library("tableone")
library("modelsummary")
library("readxl")
library(ggpubr)
library("vioplot")
library("table1")
library(stargazer)
library("scipub")
library("htmlTable")
library("corrplot")
library("lmtest")
library(forecast)
library(zoo)
library("sandwich")
install.packages("Matrix") 
library(Matrix)
install.packages("survey") 
library(survey)
install.packages("nlme") 
library(nlme)
install.packages("lmtest")
library(lmtest)
install.packages("lattice")                   
library("lattice")
install.packages("usdm")
library(usdm)

#Убираю наблюдения с сильным разбросом
full_model_esg_1 = full_model_esg[full_model_esg$Tobin_Q>min(full_model_esg$Tobin_Q)&full_model_esg$MBR<max(full_model_esg$MBR)&
                                    full_model_esg$Debt_equity<max(full_model_esg$Debt_equity),]
str(full_model_esg)

model_esg_1 <- pdata.frame(full_model_esg_1,
                           index = c("Company_name", "Period"),
                           row.names = TRUE)


#Описательная статистика переменных
model_esg_stata <- model_esg_1 %>% select(5:22)
stargazer(model_esg_stata,type="html",digits = 2, iqr= TRUE, summary=TRUE, out="full_model_esg_stata.html")

#Корреляционная матрица 


as_tibble(model_esg_stata) %>% cor()%>% 
  corrplot(type = "lower",        
           method = "color",      
           addCoef.col = "black", 
           number.cex = .3
  )


data_viol <- model_esg_2[,c(5:8)]
view(data_viol)  

# Визуализация входных данных (Violin plot) 

vioplot(data_viol, col = 2:5,rectCol = "pink")
means <- colMeans(data_viol)        
points(means, pch = 18, col = "red", cex = 1) #белая медиана, красная - среднее значение

#Распределение esg относительно цены акции
plot(Price_stock~ESG,pch = 16, col = "skyblue",model_esg_1)
abline(lm(model_esg_1$Price_stock ~ model_esg_1$ESG), col = "red")

#2вариант распределения 
box_esg1<-model_esg_1[c(5,6,7,8)]
boxplot(box_esg1)

box_esg2<-model_esg_1[c(5,6,7,8)]

palette()
#Изменение ESG по периодам
boxplot(model_esg_1$ESG~model_esg_1$Period, col="#2297E6", xlab = "Отчетный период", ylab="ESG",
        main="Диаграмма размаха общего рейтинга ESG")

#Ящик с усами для рын показателей
box_esg3<-model_esg_1[c(9,10,18)]
boxplot(box_esg3) 


#Гистограмма ROE и ROA
hist(model_esg_1$ROE,,ylim=c(0,6),xlab = "ROE и ROA",ylab = "Плотность", freq = FALSE,
     main = "Распределение плотности и гистограммы рентабельности активов и собственного капитала",cex.main=0.7, cex.lab=0.8)
lines(density(model_esg_2$ROE), lwd = 2, col = 'red')
hist(model_esg_2$ROA,add=TRUE, col='skyblue', 
     freq = FALSE)
lines(density(model_esg_2$ROA), lwd = 2, col = 'blue')


#Ящик с усами для фин показателей для 3 модели
box_esg2<-model_esg_2[c(16,17,21)]
boxplot(box_esg2,col=c("skyblue","pink","green","blue"),main = "Диаграмма размаха финансовых показателей")


#Тест Бокса-Кокса на спецификацию переменной EPS
#Графики для выбора спецификации -- без логарифма eps 
model <-lm(Price_stock ~ EPS, data = model_esg_1)
results <- bc.test(model, details = TRUE,hyp=TRUE)
plot(results)
create_diagnostic_plots <- function(model) {
plot1 <- ggplot(model, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

plot2 <- ggplot(model, aes(sample = .stdresid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", 
       y = "Standardized Residuals")
grid.arrange(plot1, plot2, ncol = 2)
}
create_diagnostic_plots(model)

#Выбор спецификации модели -FE/RE/OLS
model_pooled <- plm(log(Price_stock) ~ ESG +
                      log(EPS) + BV + Debt_equity + Cash_ratio+ROA, data = model_esg_1, model = "pooling")
summary(model_pooled)
#проверка на мультиколинеарность - все ок

model_pooled_1 <- lm(log(Price_stock) ~ ESG +
                       log(EPS) + BV + Debt_equity + Cash_ratio+ROA, data = model_esg_1)
summary(model_pooled_1)

vif_values_1 <- car::vif(model_pooled_1)
print(vif_values_1)
#проверка на нормальность остатков -все ок 
ols_plot_resid_qq(model_pooled_1)
#проверка на гетероскедастичность - на 1% ок
bptest(model_pooled)

#Графическое отображение выбранной спецификации
create_diagnostic_plots <- function(model_pooled_1) {
  
plot2 <- ggplot(model_pooled_1, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

plot3 <- ggplot(model_pooled_1, aes(sample = .stdresid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", 
       y = "Standardized Residuals")

plot4 <- ggplot(model_pooled_1, aes(.fitted, sqrt(abs(.stdresid)))) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess", color = "red") +
  labs(title = "Scale-Location (Spread-Location) Plot", x = "Fitted Values", 
       y = "√|Standardized Residuals|")

plot5 <- ggplot(model_pooled_1, aes(.hat, .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Leverage", x = "Leverage", y = "Standardized Residuals")
grid.arrange(plot2, plot3, plot4, plot5, ncol = 2)
}
create_diagnostic_plots(model_pooled_1)

model_re <- plm(log(Price_stock) ~  ESG +
                  log(EPS) + BV + Debt_equity + Cash_ratio+ROA, data = model_esg_1, model = "random")
summary(model_re)
model_fe <- plm(log(Price_stock) ~ ESG +
                  log(EPS) + BV + Debt_equity + Cash_ratio+ROA, data = model_esg_1, model = "within")
summary(model_fe)

#Тесты на короткую и длинную регрессии и Hausman-test 

#выбор спецификации модели --выбираю OLS pooling

test_1 <- plmtest(model_re, type = "bp")
print(test_1)

test_2 <- pFtest(model_fe, model_pooled)
print(test_2) 

hausman_test_3 <- phtest(model_fe, model_re)
print(hausman_test_3) 

#Сравнение R2 -- лучше всех показал результат pooled OLS

pooled_r2<- summary(model_pooled)$r.squared
print(pooled_r2)

fe_r2 <- summary(model_fe)$r.squared
print(fe_r2)

re_r2 <- summary(model_re)$r.squared
print(re_r2)

#Построение моделей pooling c ESG и ESG по отдельности на логарифм стоимости акций

#2модель 
model3_pooled <- plm(log(Price_stock) ~ ENV + log(EPS) + BV +
                        Debt_equity + Cash_ratio+ROA, data = model_esg_1,
                     model = "pooling")
summary(model3_pooled)

#проверка на мультиколинеарность -все ок
vif_values_2 <- car::vif(model3_pooled)
print(vif_values_2)

#проверка на нормальность остатков
model_pooled_3 <- lm(log(Price_stock) ~ ENV + log(EPS) + BV +
                       Debt_equity + Cash_ratio+ROA, data = model_esg_1)
summary(model_pooled_3)
ols_plot_resid_qq(model_pooled_3)
#проверка на гетероскедастичность - все ок
bptest(model_pooled_3)

#3модель
model4_pooled <- plm(log(Price_stock) ~ SOC + log(EPS) + BV +
                       Debt_equity + Cash_ratio+ROA, data = model_esg_1,
                     model = "pooling")
summary(model4_pooled)

#проверка на мультиколинеарность -все ок
vif_values_3 <- car::vif(model4_pooled)
print(vif_values_3)
#проверка на нормальность остатков
model_pooled_4 <- lm(log(Price_stock) ~ SOC + log(EPS) + BV +
                       Debt_equity + Cash_ratio+ROA, data = model_esg_1)
summary(model_pooled_4)

ols_plot_resid_qq(model_pooled_4)
#проверка на гетероскедастичность
bptest(model_pooled_4)
vcovHC(model_pooled_4, type = "HC0")
final_model_4 <- coeftest(model4_pooled,
                          vcov = vcovHC(model_pooled_4, type = "HC0"))

#4 модель
model5_pooled <- plm(log(Price_stock) ~ GOV + log(EPS) + BV +
                       Debt_equity + Cash_ratio+ROA, data = model_esg_1,
                     model = "pooling")
summary(model5_pooled)

#проверка на мультиколинеарность -все ок
vif_values_4 <- car::vif(model5_pooled)
print(vif_values_4)

#проверка на нормальность остатков -- в пределах нормы небольшие выбросы на концах
model_pooled_5 <- lm(log(Price_stock) ~ GOV + log(EPS) + BV +
                       Debt_equity + Cash_ratio+ROA, data = model_esg_1)
summary(model_pooled_5)
ols_plot_resid_qq(model_pooled_5)
#проверка на гетероскедастичность
bptest(model_pooled_5)


final_model_1
stargazer(model_pooled,model3_pooled,model4_pooled,model5_pooled,type="html",
            title = "Модели оценки влияния ESG-рэнкинга и каждого из аспектов на цены акций российских нефтегазовых компаний", out="full2_allmodel.html")


#Построение моделей pooling c ESG и ESG по отдельности на PE --результат схож
model56_pooled <- plm(log(PE) ~  ESG + log(EPS) + BV +
                         Debt_equity + Cash_ratio+ROA, data = model_esg_1, model = "pooling")
summary(model56_pooled) 

model57_pooled <- plm(log(PE) ~ ENV + log(EPS) + BV +
                        Debt_equity + Cash_ratio+ROA, data = model_esg_1, model = "pooling")
summary(model57_pooled) 
model58_pooled <- plm(log(PE) ~ SOC + log(EPS) + BV +
                        Debt_equity + Cash_ratio+ROA, data = model_esg_1, model = "pooling")
summary(model58_pooled) 
model59_pooled <- plm(log(PE) ~ GOV + log(EPS) + BV +
                        Debt_equity + Cash_ratio+ROA, data = model_esg_1, model = "pooling")
summary(model59_pooled) 
stargazer(model56_pooled,model57_pooled,model58_pooled,model59_pooled,type="html",
          title = "Модели оценки влияния ESG-рэнкинга и каждого из аспектов на коэффициент P/E акций российских нефтегазовых компаний", out="fullPE_allmodel.html")

#2 спецификация модели: рыночные коэффициенты и лаги PE/Tobin_Q/MBR  

library(tseries)

model6_pooled <- plm(log(PE) ~ lag(PE)+ ESG +log(EPS)+ MOEX_weight+ BV +
                       Debt_equity +ROA, data = model_esg_1, model = "pooling")
summary(model6_pooled) 

#Проверка на автокорреляцию остатков (график ACF)
residuals <- residuals(model6_pooled)
acf(residuals, main = "ACF of Residuals")

#Тест Льюнга-Бокса для модели с P/E
lb_test <- Box.test(residuals, type = "Ljung-Box", lag = 1)
print(lb_test)


model7_pooled <- plm(log(Tobin_Q) ~ lag(Tobin_Q) +log(EPS)+ESG + MOEX_weight+ BV +
                       Debt_equity+ROA, data = model_esg_1,
                     model = "pooling")
summary(model7_pooled)
#Тест Льюнга-Бокса для модели с коэффициентом Тобина
#проверка на автокорреляцию
residuals2 <- residuals(model7_pooled)
acf(residuals2, main = "ACF of Residuals")
lb_test2 <- Box.test(residuals2, type = "Ljung-Box", lag = 1)
print(lb_test2)


model8_pooled <- plm(log(Price_stock) ~ lag(Price_stock)+ ESG + log(EPS) + MOEX_weight+ BV +
                       Debt_equity +ROA, data = model_esg_1,
                     model = "pooling")
summary(model8_pooled)
#Тест Льюнга-Бокса для модели с ценой акции
#проверка на автокорреляцию
residuals3 <- residuals(model8_pooled)
acf(residuals3, main = "ACF of Residuals")
lb_test3 <- Box.test(residuals3, type = "Ljung-Box", lag = 1)
print(lb_test3)


model9_pooled <- plm(log(PE) ~ ESG + log(EPS) + MOEX_weight + BV +
                       Debt_equity+ROA, data = model_esg_1, model = "pooling")
summary(model9_pooled)

model10_pooled <- plm(log(Tobin_Q) ~ ESG + log(EPS) +MOEX_weight+ BV +
                        Debt_equity+ROA, data = model_esg_1, model = "pooling")
summary(model10_pooled)

model11_pooled <- plm(log(MBR) ~ lag(MBR)+ESG +log(EPS)+ MOEX_weight + BV +
                        Debt_equity+ROA, data = model_esg_1, model = "pooling")
summary(model11_pooled)

#проверка на автокорелляцию 
residuals4 <- residuals(model11_pooled)
acf(residuals4, main = "ACF of Residuals")
lb_test4 <- Box.test(residuals4, type = "Ljung-Box", lag = 1)
print(lb_test4)

model12_pooled <- plm(log(MBR) ~ ESG +log(EPS)+ MOEX_weight + BV +
                        Debt_equity+ROA, data = model_esg_1, model = "pooling")
summary(model12_pooled)

#2 версия
stargazer(model10_pooled,model7_pooled,model12_pooled,model11_pooled,type="html", 
          title = "Модели оценки влияния ESG-рэнкинга на рыночные показатели российских нефтегазовых компаний", out="lags1.html")
stargazer(model9_pooled,model6_pooled,type="html", 
          title = " Модель оценки влияния ESG-рэнкинга на коэффициент P/E российских нефтегазовых компаний", out="lags2.html")

#1 версия
stargazer(model10_pooled,model7_pooled,model12_pooled,model11_pooled,type="html", 
          title = "Модели оценки влияния ESG-рэнкинга на рыночные показатели российских нефтегазовых компаний", out="fullmarketmodelwlagа.html")
stargazer(model9_pooled,model6_pooled,type="html", 
          title = " Модель оценки влияния ESG-рэнкинга на коэффициент P/E российских нефтегазовых компаний", out="2modelQTobinfullmarketmodelwlagа.html")

#3 спецификация модели:оценка влияния на финансовые показатели
model13_pooled <- plm(ROA ~ ESG + Assets_growth + MOEX_weight+BV+
                        Debt_equity+Cash_ratio, data = model_esg_1, model = "pooling")
summary(model13_pooled)

model14_pooled <- plm(ROE ~ESG + Assets_growth +MOEX_weight +BV+
                        Debt_equity+Cash_ratio, data = model_esg_1, model = "pooling")
summary(model14_pooled)

stargazer(model13_pooled,model14_pooled,type="html", 
          title = "Модели оценки влияния ESG-рэнкинга на финансовые показатели российских нефтегазовых компаний", out="financemodel.html")

install.packages("ggpubr")
library("ggpubr")

treatment_fin_data <- model_esg_1 %>% mutate(group_data = ifelse( Company_name=="Газпром"|Company_name=="ЛУКОЙЛ"| Company_name=="Роснефть"|Company_name=="Группа «Татнефть»"|Company_name=="НОВАТЭК", "Treatment", "Control"))
as.numeric(treatment_fin_data$ESG)
as.numeric(treatment_fin_data$group_data)
#Treatment group - компании с частой публикацией нефин отчетности
#Control group - компании с редкой публикацией нефин отчетности

bxp <- ggboxplot(treatment_fin_data, x = "group_data", y = "ESG")
plot(bxp)
#График рассеивания 
dp <- ggdotplot(treatment_fin_data, x = "group_data", y = "ESG")
dp
#Накопленные баллы по рейтингам ESG за период
bp <- ggbarplot(treatment_fin_data, x = "Company_name", y = "ESG",
                sort.by.groups = TRUE
)+ font("x.text", size = 7)
bp
sp <- ggscatter(treatment_fin_data, x = "ESG", y = "Price_stock",
                add = "reg.line",               
                conf.int = TRUE,                
                color = "group_data", 
                shape = "group_data" 
)+
  stat_cor(aes(color = group_data), label.x = 3)
sp
ggarrange(bxp, dp,sp + rremove("x.text"), 
          labels = c("1", "2", "3"),
          ncol = 2, nrow = 2)
#Treatment group - компании с частой публикацией нефин отчетности
#Control group - компании с редкой публикацией нефин отчетности
treatment_group <- treatment_fin_data %>% filter(group_data = 1)
control_group <- treatment_fin_data %>% filter(group_data ==0)



hist(data=treatment_fin_data, x='ESG', hue='group_data', bins=50)


#E
t.test(treatment_fin_data_2$ENV~treatment_fin_data_2$group_data)
#Среднее различие по экологическому критерию -4,68
ATE_ENV<- mean(treatment_fin_data[which(treatment_fin_data$group_data== "Treatment"),]$ENV) -
  mean(treatment_fin_data[which(treatment_fin_data$group_data=="Control"),]$ENV)
ATE_ENV 

#S
t.test(treatment_fin_data_2$SOC~treatment_fin_data_2$group_data)
#Среднее различие по SOC -4,55
ATE_SOC<- mean(treatment_fin_data[which(treatment_fin_data$group_data== "Treatment"),]$SOC) -
  mean(treatment_fin_data[which(treatment_fin_data$group_data=="Control"),]$SOC)
ATE_SOC

#G
t.test(treatment_fin_data_2$GOV~treatment_fin_data_2$group_data)
#Среднее различие по GOV -3,96
ATE_GOV<- mean(treatment_fin_data[which(treatment_fin_data$group_data== "Treatment"),]$GOV) -
  mean(treatment_fin_data[which(treatment_fin_data$group_data=="Control"),]$GOV)
ATE_GOV

#Среднее различие по PE - 21,9
ATE_PE<- mean(treatment_fin_data[which(treatment_fin_data$group_data== "Treatment"),]$PE) -
  mean(treatment_fin_data[which(treatment_fin_data$group_data=="Control"),]$PE)
ATE_PE

mean_data <- treatment_fin_data_2 %>% group_by(Company_name) %>%
  summarise(mean_group_data=last(group_data),mean_ENV=mean(ENV),mean_SOC=mean(SOC),
            mean_GOV=mean(GOV),mean_ESG=mean(ESG),
            mean_Price_stock=mean(Price_stock),mean_EPS=mean(EPS),
            mean_PE=mean(PE),mean_Cap_company=mean(Cap_company),mean_MBR=mean(MBR),
            mean_Tobin_Q=mean(Tobin_Q),mean_MOEX_weight=mean(MOEX_weight),mean_ROA=mean(ROA),
            mean_ROE=mean(ROE),mean_BV=mean(BV),mean_RD=mean(RD),
            mean_Debt_equity=mean(Debt_equity),mean_Assets_growth=mean(Assets_growth),mean_Cash_ratio=mean(Cash_ratio))

mean_data

ATE<- mean(mean_data[which(mean_data$mean_group_data==1),]$mean_ESG) -
  mean(mean_data[which(mean_data$mean_group_data==0),]$mean_ESG)
ATE
#Cредний эффект воздействия положительный= 4,34 балла
treatment_fin_data_2 <- model_esg_1 %>% mutate(group_data = ifelse( Company_name=="Газпром"|Company_name=="ЛУКОЙЛ"| Company_name=="Роснефть"|Company_name=="Группа «Татнефть»"|Company_name=="НОВАТЭК", "1", "0"))
as.numeric(treatment_fin_data_2$ESG)
as.numeric(treatment_fin_data_2$group_data)
#Тест Уэлча показал различие в средних значениях и стандартных отклонениях по ESG
t.test(mean_data$mean_ESG~mean_data$mean_group_data)

bxp <- ggboxplot(mean_data, x = "mean_group_data", y = "mean_ESG")
plot(bxp)
#График рассеивания 
dp <- ggdotplot(mean_data, x = "mean_group_data", y = "mean_ESG")
dp
#Накопленные баллы по рейтингам ESG за период
bp <- ggbarplot(mean_data, x = "Company_name", y = "mean_ESG",
                sort.by.groups = TRUE
)+ font("x.text", size = 7)
bp
sp <- ggscatter(mean_data, x = "mean_ESG", y = "mean_Price_stock",
                add = "reg.line",               
                conf.int = TRUE,                
                color = "mean_group_data", 
                shape = "mean_group_data" 
)+
  stat_cor(aes(color = mean_group_data), label.x = 3)
sp
ggarrange(bxp, dp,sp + rremove("x.text"), 
          labels = c("1", "2", "3"),
          ncol = 2, nrow = 2)

#Баланс ковариат - наиб расхождения по капитализации и весу в индексе голубых фишек
bal.tab(mean_group_data ~ mean_Price_stock
        +mean_EPS+mean_PE+mean_Cap_company+mean_MBR+
          mean_Tobin_Q+mean_MOEX_weight+mean_ROA+mean_ROE
        +mean_BV+mean_RD+mean_Debt_equity+mean_Assets_growth+mean_Cash_ratio, data=mean_data, report =c('adj.mean.diffs','chisquare.test','p.values'))

table<-CreateTableOne(vars=c('mean_Price_stock',
                             'mean_EPS','mean_PE','mean_Cap_company','mean_MBR',
                             'mean_Tobin_Q','mean_MOEX_weight','mean_ROA','mean_ROE',
                             'mean_BV','mean_RD','mean_Debt_equity','mean_Assets_growth',
                             'mean_Cash_ratio'),
                      strata = 'mean_group_data', data=mean_data, test=TRUE) 
table
#Отсутствие баланса по переменным
mean_data$treat <- as.factor(mean_data$mean_group_data)
gg1<- ggplot(mean_data, aes(x=mean_MOEX_weight, fill=treat, color=treat)) +
  geom_histogram(position="identity", alpha=0.5)
gg2<-ggplot(mean_data, aes(x=mean_Cap_company, fill=treat, color=treat)) +
  geom_histogram(position="identity", alpha=0.5)
gg3<-ggplot(mean_data, aes(x=mean_Assets_growth, fill=treat, color=treat)) +
  geom_histogram(position="identity", alpha=0.5)
gg4<-ggplot(mean_data, aes(x=mean_ROA, fill=treat, color=treat)) +
  geom_histogram(position="identity", alpha=0.5)

ggarrange(gg1,gg2,gg3,gg4 + rremove("x.text"), 
          labels = c("1", "2", "3","4"),
          ncol = 2, nrow = 2)
m1<-matchit(mean_group_data ~ mean_Price_stock
            +mean_EPS+mean_PE+mean_Cap_company+mean_MBR+
              mean_Tobin_Q+mean_MOEX_weight+mean_ROA+mean_ROE
            +mean_BV+mean_RD+mean_Debt_equity+mean_Assets_growth+mean_Cash_ratio,data=mean_data,
            metod='nearest', distance = 'mahalanobis',estimand = 'ATT')
summary(m1)
plot(summary(m1),main = "Модель 1: мэтчинг по ближайшему соседу")
plot(m1, type = "qq", interactive = FALSE)
plot(m1, type = "ecdf") #После мэтчинга только по 2 переменных уменьшенная станд отклонение
model_1 <- lm(mean_ESG ~ mean_group_data, data = mean_data, weights = m1$weights) 
summary(model_1)

#1 вариант мэтчинга
match_m1<- match.data(m1)
bal.tab(mean_group_data ~ mean_Price_stock
        +mean_EPS+mean_PE+mean_Cap_company+mean_MBR+
          mean_Tobin_Q+mean_MOEX_weight+mean_ROA+mean_ROE
        +mean_BV+mean_RD+mean_Debt_equity+mean_Assets_growth+mean_Cash_ratio, data=match_m1, report =c('adj.mean.diffs','chisquare.test','p.values'))

#2 вариант мэтчинга
m2<-matchit(mean_group_data~ mean_Price_stock
            +mean_EPS+mean_PE+mean_Cap_company+mean_MBR+
              mean_Tobin_Q+mean_MOEX_weight+mean_ROA+mean_ROE
            +mean_BV+mean_RD+mean_Debt_equity+mean_Assets_growth+mean_Cash_ratio,data=mean_data,
            metod='nearest', distance = 'mahalanobis',
            exact= c('mean_MBR'),estimand = 'ATT')
summary(m2)
#Вывод что по компаниям нет гетерогенный различий по существующей выборке и подходит обыкновенный ATE
dp_env <- ggboxplot(mean_data, x = "mean_group_data", y = "mean_ENV")
dp_soc <- ggboxplot(mean_data, x = "mean_group_data", y = "mean_SOC")
dp_gov <- ggboxplot(mean_data, x = "mean_group_data", y = "mean_GOV")
ggarrange(dp_env,dp_soc,dp_gov + rremove("x.text"), 
          labels = c("1", "2", "3"),
          ncol = 2, nrow = 2)
#E
t.test(mean_data$mean_ENV~mean_data$mean_group_data)
#Среднее различие по экологическому критерию -4,63
ATE_mean_ENV<- mean(mean_data[which(mean_data$mean_group_data== 1),]$mean_ENV) -
  mean(mean_data[which(mean_data$mean_group_data==0),]$mean_ENV)
ATE_mean_ENV 

#S
t.test(treatment_fin_data_2$SOC~treatment_fin_data_2$group_data)
#Среднее различие по SOC -4,49
ATE_mean_SOC<- mean(mean_data[which(mean_data$mean_group_data== 1),]$mean_SOC) -
  mean(mean_data[which(mean_data$mean_group_data==0),]$mean_SOC)
ATE_mean_SOC

#G
t.test(treatment_fin_data_2$GOV~treatment_fin_data_2$group_data)
#Среднее различие по GOV -3,9
ATE_mean_GOV<- mean(mean_data[which(mean_data$mean_group_data== 1),]$mean_GOV) -
  mean(mean_data[which(mean_data$mean_group_data==0),]$mean_GOV)
ATE_mean_GOV

#Среднее различие по PE - 21,8
ATE_mean_PE<- mean(mean_data[which(mean_data$mean_group_data== 1),]$mean_PE) -
  mean(mean_data[which(mean_data$mean_group_data==0),]$mean_PE)
ATE_mean_PE
#Среднее различие по ROA - 0,06
ATE_mean_ROA<- mean(mean_data[which(mean_data$mean_group_data== 1),]$mean_ROA) -
  mean(mean_data[which(mean_data$mean_group_data==0),]$mean_ROA)
ATE_mean_ROA
