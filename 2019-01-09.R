quality = read.csv('quality.csv')
str(quality)
summary(quality) 
View(quality)

table(quality$PoorCare) 
# 0 -> good care,  1 -> poor care
# 여기서 확인할 수 있는 것은 0이 더 확률이 높기 때문에
# 베이스라인 모델의 경우 0으로 모든 값을 predict 해줄 것이다.

98/131 # 이것이 베이스라인 모델의 accuracy

install.packages("caTools") # "caTools"라는 패키지 다운
library(caTools) # caTools 불러오기

set.seed(88) # 같은 seed값에 따라 같은 값으로 split 될 것이다.

# 75%를 트레이닝 셋으로
# 25%를 테스트 셋으로 데이터셋을 분류 할 것이다.
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split

qualityTrain = subset(quality, split == TRUE) 
qualityTest = subset(quality, split == FALSE)

nrow(qualityTrain)
nrow(qualityTest)

QualityLog = glm(formula = PoorCare ~ OfficeVisits + Narcotics, family = binomial, data=qualityTrain)
# generalized linear model = Logistic Regression
QualityLog
summary(QualityLog)

predictTrain = predict(QualityLog, type="response")
summary(predictTrain) # value값이 0 ~ 1 이어야한다.

tapply(predictTrain, qualityTrain$PoorCare, mean)
# training set의 0,1 value를 가지는 값에 대한 평균값

table(qualityTrain$PoorCare, predictTrain > 0.5)
# confusion matrix of PoorCare Col. Threshold value = 0.5

10/(15 + 10) # sensitivity
70/(70 + 4) # specificity

table(qualityTrain$PoorCare, predictTrain > 0.7)
# confusion matrix of PoorCare Col. Threshold value = 0.7

8/(8+17) # sensitivity
73/(73+1) # specificity

table(qualityTrain$PoorCare, predictTrain > 0.2)
# confusion matrix of PoorCare Col. Threshold value = 0.2

16/(9+16) # sensitivity
54/(54+20) # specificity

