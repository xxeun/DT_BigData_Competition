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

shop1_day1won<-sum(shop1$day1_won)
shop1_day2won<-sum(shop1$day2_won)
shop1_day3won<-sum(shop1$day3_won)
shop1_day4won<-sum(shop1$day4_won)
shop1_day5won<-sum(shop1$day5_won)
shop1_day6won<-sum(shop1$day6_won)
shop1_day7won<-sum(shop1$day7_won)

shop2_day1won<-sum(shop2$day1_won)
shop2_day2won<-sum(shop2$day2_won)
shop2_day3won<-sum(shop2$day3_won)
shop2_day4won<-sum(shop2$day4_won)
shop2_day5won<-sum(shop2$day5_won)
shop2_day6won<-sum(shop2$day6_won)
shop2_day7won<-sum(shop2$day7_won)

shop3_day1won<-sum(shop3$day1_won)
shop3_day2won<-sum(shop3$day2_won)
shop3_day3won<-sum(shop3$day3_won)
shop3_day4won<-sum(shop3$day4_won)
shop3_day5won<-sum(shop3$day5_won)
shop3_day6won<-sum(shop3$day6_won)
shop3_day7won<-sum(shop3$day7_won)

shop4_day1won<-sum(shop4$day1_won)
shop4_day2won<-sum(shop4$day2_won)
shop4_day3won<-sum(shop4$day3_won)
shop4_day4won<-sum(shop4$day4_won)
shop4_day5won<-sum(shop4$day5_won)
shop4_day6won<-sum(shop4$day6_won)
shop4_day7won<-sum(shop4$day7_won)

market1_day1won<-sum(market1$day1_won)
market1_day2won<-sum(market1$day2_won)
market1_day3won<-sum(market1$day3_won)
market1_day4won<-sum(market1$day4_won)
market1_day5won<-sum(market1$day5_won)
market1_day6won<-sum(market1$day6_won)
market1_day7won<-sum(market1$day7_won)

market2_day1won<-sum(market2$day1_won)
market2_day2won<-sum(market2$day2_won)
market2_day3won<-sum(market2$day3_won)
market2_day4won<-sum(market2$day4_won)
market2_day5won<-sum(market2$day5_won)
market2_day6won<-sum(market2$day6_won)
market2_day7won<-sum(market2$day7_won)

shop1_alldaywon<-c(shop1_day1won, shop1_day2won,shop1_day3won,shop1_day4won,shop1_day5won,shop1_day6won,shop1_day7won)
shop2_alldaywon<-c(shop2_day1won, shop2_day2won,shop2_day3won,shop2_day4won,shop2_day5won,shop2_day6won,shop2_day7won)
shop3_alldaywon<-c(shop3_day1won, shop3_day2won,shop3_day3won,shop3_day4won,shop3_day5won,shop3_day6won,shop3_day7won)
shop4_alldaywon<-c(shop4_day1won, shop4_day2won,shop4_day3won,shop4_day4won,shop4_day5won,shop4_day6won,shop4_day7won)
market1_alldaywon<-c(market1_day1won, market1_day2won,market1_day3won,market1_day4won,market1_day5won,market1_day6won,market1_day7won)
market2_alldaywon<-c(market2_day1won, market2_day2won,market2_day3won,market2_day4won,market2_day5won,market2_day6won,market2_day7won)

df<-data.frame(shop1_alldaywon, shop2_alldaywon, shop3_alldaywon, shop4_alldaywon, market1_alldaywon, market2_alldaywon)
df

plot(df, col="#0012C0",main = "col = #0000FF (hexadecimal)")

