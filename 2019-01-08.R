wine = read.csv('wine.csv')
str(wine)
summary(wine)

model1 = lm(Price ~ AGST, data=wine) # lm is Linear model.
summary(model1)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -3.4178     2.4935  -1.371 0.183710    
#AGST          0.6351     0.1509   4.208 0.000335 ***

#intercept B0 , AGST B1
# Linear regression model
# Price(Y) = -3.4178 + 0.6351(AGST)

model1$residuals # 25 observations
SSE = sum(model1$residuals^2) # SSE

model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2) # R-squared value is better than model1
# It is better model than model1.
# *** -> very significant
SSE = sum(model2$residuals^2) # SSE of model2
# In residuals, there are all of errors.

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -4.504e-01  1.019e+01  -0.044 0.965202    
#AGST         6.012e-01  1.030e-01   5.836 1.27e-05 ***
#  HarvestRain -3.958e-03  8.751e-04  -4.523 0.000233 ***
#  WinterRain   1.043e-03  5.310e-04   1.963 0.064416 .  
#Age          5.847e-04  7.900e-02   0.007 0.994172    
#FrancePop   -4.953e-05  1.667e-04  -0.297 0.769578

# Linear regression model
# Price(Y) =-4.504e-01 + 6.012e-01(AGST) + -3.958e-03(HarvestRain) + ...


# Now we will remove unsignificant variables.
# from summary() mothod, we can recognize which variable is unsignificant.

model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)
# Although model4's R-squared value is lower than model3's
# but model4 is good model.

# WinterRain과 Price간의 상관관계(correlation)
cor(wine$WinterRain, wine$Price)
# independent와 dependent variable간의 상관관계는 1에 가깝게
# independent variable 끼리는 상관관계가 0에 가까운 것이 좋다.
cor(wine$Age, wine$FrancePop) # negative correlation

# X 와 Y 가 완전히 동일하면 +1, 전혀 다르면 0, 반대방향으로 완전히 동일 하면 –1 을 가진다
# 부호를 보지 말고 절대값이 1에 가까울수록 더욱 상관관계가 비슷한 것이다.

cor(wine)


# Data set은 Training -> execute model, Test -> test model
# 여기서 Test model은 오버피팅 되거나, execute model이 더 좋은 모델이라는 것을 증명해준다.

wineTest = read.csv('wine_test.csv')
str(wineTest)
View(wineTest) # show as dataframe with new tab

predictTest = predict(model4, newdata=wineTest)
predictTest # input the winTest data to model4 and request to predict it.
# output is Price.

SSE = sum((wineTest$Price - predictTest)^2) # SSE
SST = sum((wineTest$Price - mean(wineTest$Price))^2) # 실제값 - 베이스라인모델 값
1 - SSE/SST # R-squared



