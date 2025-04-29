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
library("stargazer")
library("scipub")
library("htmlTable")
library("corrplot")
library("lmtest")
library(forecast)

install.packages("lmtest")
library(lmtest)

#Описательная статистика переменных

model_esg_1 <- pdata.frame(short_model_esg,
                 index = c("Company_name", "Year"),
                 row.names = TRUE)

model_esg_stata <- model_esg_1 %>% select(4:20)
stargazer(model_esg_stata,type="html",digits = 3, iqr= TRUE, summary=TRUE, out="model_esg_stata.html")

#Корреляционная матрица 

as_tibble(model_esg_stata) %>% cor()%>% 
  corrplot(type = "lower",        
           method = "color",      
           addCoef.col = "black", 
           number.cex = .3
  )

correltable(data=model_esg_stata,tri="lower",html=TRUE)

dataprep_esg <- model_esg_1 %>%
  arrange(Company_name, Year) %>%
  group_by(Year)
data_viol <- model_esg_1[,c(4:7)]
view(data_viol)  

# Визуализация входных данных (Violin plot) 

vioplot(data_viol, col = 2:5,rectCol = "pink")
means <- colMeans(data_viol)        
points(means, pch = 18, col = "red", cex = 1)


#Точечная диаграмма  - средение значения esg по компаниям
#bxp <- ggboxplot(dataprep_esg, x = "Company_name", y = "ESG",
#               color = "ESG", palette = "red", binwidth = 1)
#bxp



model_pooled <- plm(log(Price_stock) ~ ESG + MOEX_weight + Tobin_Q +
                     log(EPS) + Debt_equity + ROA, data = model_esg_1, model = "pooling")
summary(model_pooled)

model_re <- plm(log(Price_stock) ~  ESG + MOEX_weight + Tobin_Q +
                  log(EPS) + Debt_equity + ROA, data = model_esg_1, model = "random")
summary(model_re)
model_fe <- plm(log(Price_stock) ~ ESG + MOEX_weight + Tobin_Q +
                  log(EPS) + Debt_equity + ROA, data = model_esg_1, model = "within")
summary(model_fe)

# Тесты на короткую и длинную регрессии и Hausman-test -- выбор спецификации модели

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

#Вывод таблицы со спецификациями моделей 

models <- list(
  "Модель 1" = model_pooled,
  "Модель 2" = model_re,
  "Модель 3" = model_fe
 )

modelsummary(models,
             output = "html", 
             title = "Модели оценки влияния ESG-рэнкинга на цены акций",font.size="small")


#Только аспекты рейтинга

model2_pooled <- plm(log(Price_stock) ~ ENV +SOC + GOV + MOEX_weight + Tobin_Q +
  log(EPS) + Debt_equity + ROA, data = model_esg_1, model = "pooling")
summary(model2_pooled)
phtest(model2_pooled)

#Построение моделей pooling c ESG и ESG по отдельности на стоимость акций

model3_pooled <- plm(log(Price_stock) ~ ENV + MOEX_weight + Tobin_Q +
                       log(EPS) + Debt_equity + ROA, data = model_esg_1,
                     model = "pooling")
summary(model3_pooled)

model4_pooled <- plm(log(Price_stock) ~ SOC + MOEX_weight + Tobin_Q +
                       log(EPS) + Debt_equity + ROA, data = model_esg_1,
                     model = "pooling")
summary(model4_pooled)

model5_pooled <- plm(log(Price_stock) ~ GOV + MOEX_weight + Tobin_Q +
                       log(EPS) + Debt_equity + ROA, data = model_esg_1,
                     model = "pooling")
summary(model5_pooled)


stargazer(model_pooled,model3_pooled,model4_pooled,model5_pooled,type="html",
          title = "Модели оценки влияния ESG-рэнкинга и каждого из аспектов на цены акций российских нефтегазовых компаний", out="allmodel.html")

#Построение моделей pooling c ESG и ESG по отдельности на стоимость акций с лагами

model6_pooled <- plm(log(EPS) ~ lag(EPS) + ESG + MOEX_weight + 
                   Cash_ratio + ROE, data = model_esg_1, model = "pooling")
summary(model6_pooled)

residuals <- residuals(model6_pooled)

#Проверка на автокорреляцию остатков (график ACF)
acf(residuals, main = "ACF of Residuals")

#Тест Льюнга-Бокса
lb_test <- Box.test(residuals, type = "Ljung-Box", lag = 1)
print(lb_test)
#автокорреляция отсутствует 

model7_pooled <- plm(log(Price_stock) ~ lag(Price_stock) + ENV + MOEX_weight + Tobin_Q +
                   log(EPS) + Debt_equity + ROA, data = model_esg_1,  model = "pooling")
summary(model7_pooled)

#Влияние на другие рыночные показатели - EPS/Tobin_Q
 
model8_pooled <- plm(log(EPS) ~ ESG + MOEX_weight + ROA + 
                   Cash_ratio + Debt_equity, data = model_esg_1, model = "pooling")
summary(model8_pooled)

model9_pooled <- plm(log(EPS) ~ lag(EPS) + ESG + MOEX_weight + ROA + 
                   Cash_ratio+ Debt_equity, data = model_esg_1, model = "pooling")
summary(model9_pooled)

model10_pooled <- plm(log(Tobin_Q) ~ ESG + MOEX_weight + ROA + 
                       Cash_ratio+ Debt_equity, data = model_esg_1, model = "pooling")
summary(model10_pooled)

model11_pooled <- plm(log(Tobin_Q) ~ lag(Tobin_Q) + ESG + MOEX_weight + ROA + 
                       Cash_ratio+ Debt_equity, data = model_esg_1, model = "pooling")
summary(model11_pooled)

stargazer(model8_pooled,model9_pooled,model10_pooled,model11_pooled,type="html", 
          title = "Модели оценки влияния ESG-рэнкинга на рыночные показатели российских нефтегазовых компаний", out="marketmodelwlag.html")

#оценка влияния на финансовые показатели
model12_pooled <- plm(ROA ~ ESG + MOEX_weight + Assets_growth+ BV + 
                        Debt_equity, data = model_esg_1, model = "pooling")
summary(model12_pooled)

model13_pooled <- plm(ROE ~ ESG + MOEX_weight + Assets_growth + BV +
                        Debt_equity, data = model_esg_1, model = "pooling")
summary(model13_pooled)

model14_pooled <- plm(log(MBR) ~ ESG + MOEX_weight + Assets_growth + BV + 
                        Debt_equity, data = model_esg_1, model = "pooling")
summary(model14_pooled)


stargazer(model12_pooled,model13_pooled,model14_pooled,type="html", 
          title = "Модели оценки влияния ESG-рэнкинга на финансовые показатели российских нефтегазовых компаний", out="financemodel.html")




