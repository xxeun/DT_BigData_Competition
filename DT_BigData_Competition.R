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

#한식, 중식, 일식, 양식
shop1<-Sales%>%filter(service_code=="CS100001")
shop2<-Sales%>%filter(service_code=="CS100002")
shop3<-Sales%>%filter(service_code=="CS100003")
shop4<-Sales%>%filter(service_code=="CS100004")

#슈퍼마켓, 편의점
market1<-Sales%>%filter(service_code=="CS300001") 
market2<-Sales%>%filter(service_code=="CS300002")

shop1
apply(shop1,2, max)
apply(shop2,2, max)
apply(market1,2, max)
apply(market2,2, max)
