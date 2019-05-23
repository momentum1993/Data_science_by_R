Sequence = seq(0, 100, 2)
Sequence
Country = c("Korea", "China", "USA")
num = c(1,2,3)

df = data.frame(Country, num)
df
df$alphabet = c('a','b','c') # 데이터프레임에 새로운 컬럼 추가
df
newdf = data.frame(Country, num, alphabet)
newdf
Alldf = rbind(df, newdf) # df와 newdf를 합쳐준다.
# 그리고 다시 데이터프레임 반환


