# 청소년 성향에 대해 설문 조사한 데이터
# SNS 계정을 가진 4개의 기본정보와 36개의 관심분야
# 청소년 성장 코미디 : 
# 범죄성향, 운동성향, 외모지향, 무기력, 브레임
teens <- read.csv('./snsdata.csv') # 30000 x 40 종속변수가 X : 비지도학습
set.seed(100)
str(teens)
head(teens)

sum(is.na(teens$gradyear))
sum(is.na(teens$friends))
sum(is.na(teens))
colSums(apply(teens,2,is.na))
table(teens$gender)
summary(teens$age)
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
summary(teens$age)
mean(teens$age, na.rm = T)

# 졸업년도 별로 나이의 평균
(avg <- ave(teens$age, teens$gradyear, FUN=function(x) mean(x, na.rm=T)))
teens$age <- ifelse(is.na(teens$age), avg, teens$age) # 3항 연산자
teens$age[1:6]
summary(teens$age)

interests <- teens[5:40] # 1,2,3,4는 기본정보라서 제외
summary(interests)
interests_n <- data.frame(lapply(interests, scale))
summary(interests_n)
teen_clusters <- kmeans(interests_n, 5)
teen_clusters$size
table(teen_clusters$cluster)


# 그룹의 명명식 => 클러스터를 대표하는 것이 중심값
# # 범죄성향, 운동성향, 외모지향, 무기력, 브레임
teen_clusters$centers # 중심값으로 그룹성향을 네이밍
# 1 무기력
# 2 범죄성향
# 3 브레임
# 4 외모지향
# 5 운동성향

teens$cluster  <- teen_clusters$cluster # 클러스터 종속변수
aggregate(data = teens, age ~ cluster, mean) #클러스터 별로 나이의 평균을 구하시오
qplot(cluster, age, colour = gender, data = teens)
res <- aggregate(data = teens, gender=='F' ~ cluster, mean)
plot(res, type='b', lty=2)
aggregate(data = teens, softball + volleyball + hair + dress ~ gender =='F', mean)

# 문제) 클러스터별로 자기가 알고 싶은 성향을 확인해서 메일로 전송
# 3개 이상씩

str(teens)
# 4 외모지향
aggregate(data = teens, sexy + dance + music + hair ~ cluster == '4', mean)
# 1 무기력
aggregate(data = teens, die + death + god + church ~ cluster == '1', mean)
# 5 운동성향
aggregate(data = teens, football + soccer + swimming + tennis + sports ~ cluster == '5', mean )