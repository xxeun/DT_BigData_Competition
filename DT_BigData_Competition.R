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

#한식음식점 요일별 평균 매출 추이
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

#한식음식점 시간대별 평균 매출 추이
income1_t<-c(mean(shop1$time1_won),mean(shop1$time2_won),mean(shop1$time3_won),mean(shop1$time4_won),mean(shop1$time5_won),mean(shop1$time6_won))
data_income1_t<-data.frame(time=c(1:6),income1_t)
ggplot(data=data_income1_t, aes(x=time, y=income1_t))+geom_point(color="blue",size=2)+geom_line(color="black", size=0.3)+ggtitle("한식음식점 시간대별 평균 매출추이")

#중식음식점 시간대별 평균 매출 추이
income2_t<-c(mean(shop2$time1_won),mean(shop2$time2_won),mean(shop2$time3_won),mean(shop2$time4_won),mean(shop2$time5_won),mean(shop2$time6_won))
data_income2_t<-data.frame(time=c(1:6),income2_t)
ggplot(data=data_income2_t, aes(x=time, y=income2_t))+geom_point(color="red",size=2)+geom_line(color="black", size=0.3)+ggtitle("중식음식점 시간대별 평균 매출추이")

#일식음식점 시간대별 평균 매출 추이
income3_t<-c(mean(shop3$time1_won),mean(shop3$time2_won),mean(shop3$time3_won),mean(shop3$time4_won),mean(shop3$time5_won),mean(shop3$time6_won))
data_income3_t<-data.frame(time=c(1:6),income3_t)
ggplot(data=data_income3_t, aes(x=time, y=income3_t))+geom_point(color="orange",size=2)+geom_line(color="black", size=0.3)+ggtitle("일식음식점 시간대별 평균 매출추이")

#양식음식점 시간대별 평균 매출 추이
income4_t<-c(mean(shop4$time1_won),mean(shop4$time2_won),mean(shop4$time3_won),mean(shop4$time4_won),mean(shop4$time5_won),mean(shop4$time6_won))
data_income4_t<-data.frame(time=c(1:6),income4_t)
ggplot(data=data_income4_t, aes(x=time, y=income4_t))+geom_point(color="yellow",size=2)+geom_line(color="black", size=0.3)+ggtitle("양식음식점 시간대별 평균 매출추이")

#슈퍼마켓 시간대별 평균 매출 추이
income5_t<-c(mean(market1$time1_won),mean(market1$time2_won),mean(market1$time3_won),mean(market1$time4_won),mean(market1$time5_won),mean(market1$time6_won))
data_income5_t<-data.frame(time=c(1:6),income5_t)
ggplot(data=data_income5_t, aes(x=time, y=income5_t))+geom_point(color="purple",size=2)+geom_line(color="black", size=0.3)+ggtitle("슈퍼마켓 시간대별 평균 매출추이")

#편의점 시간대별 평균 매출 추이
income6_t<-c(mean(market2$time1_won),mean(market2$time2_won),mean(market2$time3_won),mean(market2$time4_won),mean(market2$time5_won),mean(market2$time6_won))
data_income6_t<-data.frame(time=c(1:6),income6_t)
ggplot(data=data_income6_t, aes(x=time, y=income6_t))+geom_point(color="skyblue",size=2)+geom_line(color="black", size=0.3)+ggtitle("편의점 시간대별 평균 매출추이")


#전체 서비스업종 코드별 필터
code1<-Sales%>%filter(service_code=="CS100001")
code2<-Sales%>%filter(service_code=="CS100002")
code3<-Sales%>%filter(service_code=="CS100003")
code4<-Sales%>%filter(service_code=="CS100004")
code5<-Sales%>%filter(service_code=="CS100005")
code6<-Sales%>%filter(service_code=="CS100006")
code7<-Sales%>%filter(service_code=="CS100007")
code8<-Sales%>%filter(service_code=="CS100008")
code9<-Sales%>%filter(service_code=="CS100009")
code10<-Sales%>%filter(service_code=="CS100010")
code11<-Sales%>%filter(service_code=="CS200001")
code12<-Sales%>%filter(service_code=="CS200002")
code13<-Sales%>%filter(service_code=="CS200003")
code14<-Sales%>%filter(service_code=="CS200005")
code15<-Sales%>%filter(service_code=="CS200006")
code16<-Sales%>%filter(service_code=="CS200007")
code17<-Sales%>%filter(service_code=="CS200008")
code18<-Sales%>%filter(service_code=="CS200016")
code19<-Sales%>%filter(service_code=="CS200017")
code20<-Sales%>%filter(service_code=="CS200019")
code21<-Sales%>%filter(service_code=="CS200024")
code22<-Sales%>%filter(service_code=="CS200024")
code23<-Sales%>%filter(service_code=="CS200026")
code24<-Sales%>%filter(service_code=="CS200028")
code25<-Sales%>%filter(service_code=="CS200029")
code26<-Sales%>%filter(service_code=="CS200030")
code27<-Sales%>%filter(service_code=="CS200031")
code28<-Sales%>%filter(service_code=="CS200032")
code29<-Sales%>%filter(service_code=="CS200033")
code30<-Sales%>%filter(service_code=="CS200034")
code31<-Sales%>%filter(service_code=="CS200036")
code32<-Sales%>%filter(service_code=="CS200037")
code33<-Sales%>%filter(service_code=="CS300001")
code34<-Sales%>%filter(service_code=="CS300002")
code35<-Sales%>%filter(service_code=="CS300003")
code36<-Sales%>%filter(service_code=="CS300004")
code37<-Sales%>%filter(service_code=="CS300006")
code38<-Sales%>%filter(service_code=="CS300007")
code39<-Sales%>%filter(service_code=="CS300008")
code40<-Sales%>%filter(service_code=="CS300009")
code41<-Sales%>%filter(service_code=="CS300010")
code42<-Sales%>%filter(service_code=="CS300011")
code43<-Sales%>%filter(service_code=="CS300014")
code44<-Sales%>%filter(service_code=="CS300015")
code45<-Sales%>%filter(service_code=="CS300016")
code46<-Sales%>%filter(service_code=="CS300017")
code47<-Sales%>%filter(service_code=="CS300018")
code48<-Sales%>%filter(service_code=="CS300019")
code49<-Sales%>%filter(service_code=="CS300020")
code50<-Sales%>%filter(service_code=="CS300021")
code51<-Sales%>%filter(service_code=="CS300022")
code52<-Sales%>%filter(service_code=="CS300024")
code53<-Sales%>%filter(service_code=="CS300025")
code54<-Sales%>%filter(service_code=="CS300026")
code55<-Sales%>%filter(service_code=="CS300027")
code56<-Sales%>%filter(service_code=="CS300028")
code57<-Sales%>%filter(service_code=="CS300029")
code58<-Sales%>%filter(service_code=="CS300031")
code59<-Sales%>%filter(service_code=="CS300032")
code60<-Sales%>%filter(service_code=="CS300033")
code61<-Sales%>%filter(service_code=="CS300035")
code62<-Sales%>%filter(service_code=="CS300036")
code63<-Sales%>%filter(service_code=="CS300043")


#각 업종별-요일별 매출금액 합계 함수
day_sumfunction<- function(x){
  for(i in 7) {
    y<-sum(x$day1_won, x$day2_won,x$day3_won,x$day4_won,x$day5_won,x$day6_won,x$day7_won)
  }
  return(y)
}

incomes<-c(day_sumfunction(code1),day_sumfunction(code2),day_sumfunction(code3),
           day_sumfunction(code4),day_sumfunction(code5),day_sumfunction(code6),
           day_sumfunction(code7),day_sumfunction(code8),day_sumfunction(code9),
           day_sumfunction(code10),day_sumfunction(code11),day_sumfunction(code12),
           day_sumfunction(code13),day_sumfunction(code14),day_sumfunction(code15),
           day_sumfunction(code16),day_sumfunction(code17),day_sumfunction(code18),
           day_sumfunction(code19),day_sumfunction(code20),day_sumfunction(code21),
           day_sumfunction(code22),day_sumfunction(code23),day_sumfunction(code24),
           day_sumfunction(code25),day_sumfunction(code26),day_sumfunction(code27),
           day_sumfunction(code28),day_sumfunction(code29),day_sumfunction(code30),
           day_sumfunction(code31),day_sumfunction(code32),day_sumfunction(code33),
           day_sumfunction(code34),day_sumfunction(code35),day_sumfunction(code36),
           day_sumfunction(code37),day_sumfunction(code38),day_sumfunction(code39),
           day_sumfunction(code40),day_sumfunction(code41),day_sumfunction(code42),
           day_sumfunction(code43),day_sumfunction(code44),day_sumfunction(code45),
           day_sumfunction(code46),day_sumfunction(code47),day_sumfunction(code48),
           day_sumfunction(code49),day_sumfunction(code50),day_sumfunction(code51),
           day_sumfunction(code52),day_sumfunction(code53),day_sumfunction(code54),
           day_sumfunction(code55),day_sumfunction(code56),day_sumfunction(code57),
           day_sumfunction(code58),day_sumfunction(code59),day_sumfunction(code60),
           day_sumfunction(code61),day_sumfunction(code62),day_sumfunction(code63))
incomes_df<-data.frame(code=c(1:63),incomes)
all_incomes_graph<-ggplot(data=incomes_df, aes(x=code, y=incomes))+geom_point(color="gray",size=2)+geom_line(color="purple", size=0.3)+ggtitle("전체 업종별 전체 요일 합계 매출추이")
