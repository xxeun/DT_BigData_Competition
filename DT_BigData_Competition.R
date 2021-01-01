setwd("c:/data")
Sales = read.csv("Sales_data.csv")
attach(Sales)
library(tidyverse)
library(ggplot2)
library(dplyr)


#지수표기->숫자표기 format 변경
day1_won<-format(day1_won, scientific = FALSE)
day2_won<-format(day1_won, scientific = FALSE)
day3_won<-format(day1_won, scientific = FALSE)
day4_won<-format(day1_won, scientific = FALSE)
day5_won<-format(day1_won, scientific = FALSE)
day6_won<-format(day1_won, scientific = FALSE)
day7_won<-format(day1_won, scientific = FALSE)

#요일별 매출건수-주말 매출건수 회귀분석
daycnt<-Sales %>% select(day1_cnt, day2_cnt,day3_cnt,day4_cnt,day5_cnt,day6_cnt,day7_cnt)
day_cnt<-daycnt %>% map(~lm(Sales$주말_매출_건수~.x))%>%map(summary)
day_cnt%>%map('r.squared')
d<-lm(Sales$주말_매출_건수~day1_cnt+day2_cnt+day3_cnt+day4_cnt+day5_cnt+day6_cnt+day7_cnt)
d
plot(Sales$주말_매출_건수~day1_cnt+day2_cnt+day3_cnt+day4_cnt+day5_cnt+day6_cnt+day7_cnt)
plot(d)

#서비스 업종 코드 구분
table(service_code)
plot(table(service_code))

plot(day1_cnt)
#한식, 중식, 일식, 양식
shop1<-Sales%>%filter(service_code=="CS100001")
shop2<-Sales%>%filter(service_code=="CS100002")
shop3<-Sales%>%filter(service_code=="CS100003")
shop4<-Sales%>%filter(service_code=="CS100004")


#슈퍼마켓, 편의점
market1<-Sales%>%filter(service_code=="CS300001") 
market2<-Sales%>%filter(service_code=="CS300002")

apply(shop1,2, max)
apply(shop2,2, max)
apply(market1,2, max)
apply(market2,2, max)

#각 업종별-요일별 매출금액 합계 함수
day_sumfunction<- function(x){
for(i in 7) {
  y<-sum(x$day1_won, x$day2_won,x$day3_won,x$day4_won,x$day5_won,x$day6_won,x$day7_won)
}
  return(y)
}

#shop1 한식음식점 요일별 평균 매출 추이
income1<-c(mean(shop1$day1_won),mean(shop1$day2_won),mean(shop1$day3_won),mean(shop1$day4_won),mean(shop1$day5_won),mean(shop1$day6_won),mean(shop1$day7_won))
data_income1<-data.frame(day=c(1:7),income1)
ggplot(data=data_income1, aes(x=day, y=income1))+geom_point(color="blue",size=2)+geom_line(color="black", size=0.3)+ggtitle("한식음식점 요일별 평균 매출추이")

#중식음식점 요일별 평균 매출 추이
income2<-c(mean(shop2$day1_won),mean(shop2$day2_won),mean(shop2$day3_won),mean(shop2$day4_won),mean(shop2$day5_won),mean(shop2$day6_won),mean(shop2$day7_won))
data_income2<-data.frame(day=c(1:7),income2)
ggplot(data=data_income2, aes(x=day, y=income2))+geom_point(color="red",size=2)+geom_line(color="black", size=0.3)+ggtitle("중식음식점 요일별 평균 매출추이")

#일식음식점 요일별 평균 매출 추이
income3<-c(mean(shop3$day1_won),mean(shop3$day2_won),mean(shop3$day3_won),mean(shop3$day4_won),mean(shop3$day5_won),mean(shop3$day6_won),mean(shop3$day7_won))
data_income3<-data.frame(day=c(1:7),income3)
ggplot(data=data_income3, aes(x=day, y=income3))+geom_point(color="orange",size=2)+geom_line(color="black", size=0.3)+ggtitle("일식음식점 요일별 평균 매출추이")

#양식음식점 요일별 평균 매출 추이
income4<-c(mean(shop4$day1_won),mean(shop4$day2_won),mean(shop4$day3_won),mean(shop4$day4_won),mean(shop4$day5_won),mean(shop4$day6_won),mean(shop4$day7_won))
data_income4<-data.frame(day=c(1:7),income4)
ggplot(data=data_income4, aes(x=day, y=income4))+geom_point(color="yellow",size=2)+geom_line(color="black", size=0.3)+ggtitle("양식음식점 요일별 평균 매출추이")

#슈퍼마켓 요일별 평균 매출 추이
income5<-c(mean(market1$day1_won),mean(market1$day2_won),mean(market1$day3_won),mean(market1$day4_won),mean(market1$day5_won),mean(market1$day6_won),mean(market1$day7_won))
data_income5<-data.frame(day=c(1:7),income5)
ggplot(data=data_income5, aes(x=day, y=income5))+geom_point(color="purple",size=2)+geom_line(color="black", size=0.3)+ggtitle("슈퍼마켓 요일별 평균 매출추이")

#편의점 요일별 평균 매출 추이
income6<-c(mean(market2$day1_won)mean(market2$day2_won),mean(market2$day3_won),mean(market2$day4_won),mean(market2$day5_won),mean(market2$day6_won),mean(market2$day7_won))
data_income6<-data.frame(day=c(1:7),income6)
ggplot(data=data_income6, aes(x=day, y=income6))+geom_point(color="skyblue",size=2)+geom_line(color="black", size=0.3)+ggtitle("편의점 요일별 평균 매출추이")
