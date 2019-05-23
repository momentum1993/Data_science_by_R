WHO = read.csv('WHO.csv') # WHO.csv file loaded

str(WHO) # string of WHO file
# 'data.frame':	194 obs. -> obs = lines(=rows)
# 데이터의 column, row 개수 알려준다. 그리고 각 데이터의 타입과 값들을 알려준다.
#  $ Country                      : Factor w/ 194 levels "Afghanistan",..: 1 2 3 4 5 6 7 8 9 10 ...
# 위에서 levels는 unique한 값들의 개수 (중복 데이터 불포함)

summary(WHO) 
# 각 컬럼의 값에 대해서 해당 값이 나오는 횟수..
# 그리고 평균, 최솟값, 최댓값 등을 통계적으로 보여준다.

WHO_EUROPE = subset(WHO, Region == "Europe")
# in Dataframe WHO, can get region of Europe.

str(WHO_EUROPE)
ls()

write.csv(WHO_EUROPE, "WHO_EUROPE.csv")
# write csv file "WHO_EUROPE.csv"

rm(WHO_EUROPE) # remove variable "WHO_EUROPE"

WHO$Under15 #WHO 데이터프레임의 under15 column

mean(WHO$Under15) # WHO 데이터프레임의 under15 column의 평균

sd(WHO$Under15) # WHO under15의 표준편차

summary(WHO$Under15)
# 1st Qu. 는 25퍼센트의 데이터가 저 값보다 낮다는 것.
# 3st Qu. 는 75퍼센트의 데이터가 저 값보다 낮다는 것.

min(WHO$Under15) # WHO under15의 최소값.

which.min(WHO$Under15) # WHO의 Under15에서의 최솟값의 인덱스를 출력해줌.
WHO$Country[86]

which.max(WHO$Under15)
WHO$Country[124]

plot(WHO$GNI, WHO$FertilityRate)
# scattered plot을 그려준다.

Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers) # Outlier의 row개수.
Outliers[c("Country", "GNI", "FertilityRate")] 
# 내가 원하는 컬럼에 대해서만 데이터를 가져와 데이터프레임 생성.
# 벡터를 만들어서 넣어주어야함.

hist(WHO$CellularSubscribers) # 해당 히스토그램을 그려준다.

boxplot(WHO$LifeExpectancy ~ WHO$Region) # 박스 플롯을 만들어준다.

boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "", ylab = "Life Expentancy Plot", main="PLOT!")
# y축에는 Region, plot의 이름을 PLOT!으로 해서 플롯 생성해준다.

table(WHO$Region)
tapply(WHO$Under15, WHO$Region, mean)
# 같은 Region에 대해서 Under15값들을 평균해줌.
tapply(WHO$LiteracyRate, WHO$Region, min)
# NA값 때문에 제대로 나오지 않음
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm=TRUE)
# 그래서 NA를 지우고 해준다.

wine = read.csv('wine.csv')
str(wine)
summary(wine)

model1 = lm(Price ~ AGST, data=wine)
summary(model1)
# Residuals -> error value
# Coefficients  tvalue.
# 

modela= lm(Price~Age, data = wine)
summary(modela)

model1$residuals
SSE = sum(model1$residuals^2)
SSE

model2 = lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)

SSE = sum(model2$residuals^2)
SSE

model3 = lm(Price~ ., data = wine )
summary(model3)
SSE = sum(model3$residuals^2)
SSE

model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)

#Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

model5 = lm(Price ~ AGST + HarvestRain + WinterRain + FrancePop + Age + Year, data=wine)
summary(model5)

##### homework 3

wine = read.csv('wine.csv')
str(wine)
summary(wine)

modelhw = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(modelhw) # Q3, Q4

df = data.frame(wine$HarvestRain, wine$WinterRain)
df
cor(df) # Q5
