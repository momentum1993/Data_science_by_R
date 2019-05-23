# laod data
Titanic = read.csv("titanic.csv")


library(caTools)
set.seed(3000)
spl = sample.split(Titanic$Survived, SplitRatio = 0.7)
Train = subset(Titanic, spl == TRUE)
Test = subset(Titanic, spl == FALSE)

str(Train)
summary(Train)

# fill in missing values for Age
Train$Age[is.na(Train$Age)] = mean(Train$Age, na.rm = TRUE)
Test$Age[is.na(Test$Age)] = mean(Test$Age, na.rm = TRUE)

# fill in missing values for Fare
Train$Fare[is.na(Train$Fare)] = mean(Train$Fare, na.rm = TRUE)
Test$Fare[is.na(Test$Fare)] = mean(Test$Fare, na.rm = TRUE)

# Step 2: Create DF of independent/dependent variables
nonvars = c("PassengerId","Name","Ticket","Cabin","Embarked")
Train = Train[,c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare")]
str(Train)

# Step 3: Check for MultiCollinearity
Train$Sex = as.numeric(Train$Sex)
Test$Sex = as.numeric(Test$Sex)
cor(Train)

#baseline model
table(Titanic$Survived)
baseAcur = 815 / (815 + 494)

# CART model
library(randomForest)

Train$Survived = as.factor(Train$Survived)
Test$Survived = as.factor(Test$Survived)

titanicRF = randomForest(Survived~., data = Train, ntree = 200, nodesize = 25)

PredictRF = predict(titanicRF, newdata = Test)

table(Test$Survived, PredictRF)

accuracy = (223+108)/(223+22+40+108)
cat("Test\naccuracy: ", accuracy, " > ", "baseline: ", baseAcur)
