#####################

# 질적변수 2개 * 양적변수 1개

##############_---------------------------------------------

library(tidyverse)
library(readxl)
library(ggplot2)
library(ggridges)
install.packages("ggmosaic")
library(ggmosaic)
library(ggExtra)
my_check = read_xlsx("C:/Users/admin/Desktop/Data_viz/my_data.xlsx",sheet = 1)
my_check_2 = read_xlsx("C:/Users/admin/Desktop/Data_viz/my_data.xlsx",sheet = 2)
CarSales = read.csv("C:/Users/admin/Desktop/Data_viz/CarSales.csv")
checkout2 = read.csv("C:/Users/admin/Desktop/Data_viz/checkout2.csv")
my_check$id = as.factor(my_check$id)
my_check$prod_name = as.factor(my_check$prod_name)
my_check$amount = as.factor(my_check$amount)

# 

############----------------------------

my_check %>% 
  group_by(id, prod_name) %>% 
  summarise(total=sum(amount))
#각 고객의 상품별 구입 총액을 그래프로 , 해석


my_check %>% 
  group_by(id, prod_name) %>% 
  summarise(avg=mean(amount)) %>% 
  ggplot(aes(id, prod_name, fill=avg)) +
  geom_tile()



my_check %>% 
  group_by(id, prod_name) %>% 
  summarise(total=sum(amount)) %>% 
  ggplot(aes(id, prod_name, fill=total)) +
  geom_tile()+
  scale_fill_distiller(palette="Reds") 



my_check %>% 
  group_by(id, prod_name) %>% 
  summarise(total=sum(amount)) %>% 
  ggplot(aes(id, prod_name, fill=total, label=total)) +
  geom_tile()+
  scale_fill_distiller(palette="Blues") +
  geom_text()


#사용되는 변수는 양적변수 2개와 질적변수 2개
#n은 주문횟수
my_check %>% 
  group_by(id, prod_name) %>% 
  summarise(total=sum(amount), n=n()) %>% 
  ggplot(aes(id, prod_name, fill=total, label=n)) +
  geom_tile()+
  scale_fill_distiller(palette="Blues",direction=1) +
  geom_text()



# my_check_2 데이터 
my_check_2 %>% 
  group_by(cust_id, prod_name) %>% 
  summarise(total=sum(amount), n=n()) %>% 
  ggplot(aes(cust_id, prod_name, fill=total, label=total)) +
  geom_tile()+
  scale_fill_distiller(palette="Blues", direction= 1) +
  geom_text()



my_check_2 %>% 
  group_by(cust_id, prod_name) %>% 
  summarise(total=sum(amount), n=n()) %>% 
  ggplot(aes(cust_id, prod_name, fill=total, label=c(n))) +
  geom_tile()+
  scale_fill_distiller(palette="Blues", direction= 1) +
  geom_text()




# 양적변수 근접화 질적변수 순위별 뱐수
# p.134
x=c(1,2,5,4,7,10,6)
#양적변수를 구간으로 묶어서 질적변수로 만들기
cut(1:10, breaks=c(1,5))
# (   ] 형태가 default
cut(1:10, breaks=c(1,5,10))
# 최소값을 포함할 수 있게
cut(1:10, breaks=c(0,5,10))
# default right=T, 폐구간은 오른쪽 (   ], 1이 NA
cut(1:10, breaks=c(1,5,10), right=T ,labels=c("LOW","HIGH"))
# right=F, 폐구간은 왼쪽 [  ), 10이 NA
cut(1:10, breaks=c(1,5,10), right=F ,labels=c("LOW","HIGH"))
#1에서 5 5에서 10을 오른쪽 라벨로 이름을 붙여줌

# default right=T, 폐구간은 오른쪽 (   ], 1이 NA이지만 포함하도록 최소값을 포함하도록 곱함
cut(1:10, breaks=c(1,5,10), right=T , include.lowest=T, labels=c("LOW","HIGH"))

# right=F, 폐구간은 왼쪽 [  ), 10이 NA이지만 포함하도록
cut(1:10, breaks=c(1,5,10), right=F , include.lowest=T ,labels=c("LOW","HIGH"))
cut(1:10, breaks=c(1,3,7,10), include.lowest=T, labels=paste0("V",seq(1:3)))
#include.lowest 끝점을 포함



###-----------------------------------------------------

# 데이터 결합하기 ------------------------------------------------------------
# 데이터프레임 3개 
p = data.frame(id=c(1,2),
               math=c(100,90), 
               history=c(70,80))
q = data.frame(id=c(3,4,5), 
               math=c(100,80,70), 
               history=c(90,90,60))

r=data.frame(id=c(1,2), 
             music=c(100,100))
p
q
r



rbind(p,q) # 같은 변수를 가지는 데이터프레임 
cbind(p,r) # id가 중복된다. 
cbind(p,q) # 의미 없음



# merge는 열로 결합하는 것으로 이해
#by=id id가 같은것 끼리 묶어라
merge(p,r, by='id') # cbind(p,r) 과 비교 
merge(q,r, by='id') 
merge(q,r, by='id', all=T)
merge(p,q, by='id')
merge(p,q, by='id', all.x = T)
merge(p,q, by='id', all=T)



## 연습문제 11-15
# 1) mydata.xlsx의 sheet 7과 8을 읽어 grade, info 로 저장하시오.
grade= read_xlsx("C:/Users/admin/Desktop/my_data.xlsx",sheet = 7)
info= read_xlsx("C:/Users/admin/Desktop/my_data.xlsx",sheet = 8)
# 2) merge를 이용하여 grade에 info의 gender, name 변수를 추가하여 grade2로 저장하시오. 
grade2 = merge(grade, info)
view(grade2)



#_----------------------------------
library(readr)
customer = read_csv("C:/Users/admin/Desktop/Data_viz/customer.csv")
checkout = read.csv("C:/Users/admin/Desktop/Data_viz/checkout.csv", fileEncoding = "UTF-8")

nrow(checkout)
head(checkout)
head(customer)


idx=sample(1:nrow(checkout),5000)
#sample표본 무작위로 5000개를 뽑음

checkout=checkout[idx,]


# customer의 성별과 나이를 checkout에 추가 

head(customer %>% select(1:3), 5) #앞에 자료 3개 가져옴 앞에 5개 값만 봄
head(checkout)
temp = customer %>% select(1:3)
checkout2 = merge(checkout, temp, by="cust_id")
head(checkout2)



checkout2 = checkout2  %>% 
  as_tibble() %>% 
  mutate(age_group=cut(age, breaks=seq(20,100,10), 
                       include.lowest=T, labels=paste0(seq(20,90,10), "대"))) %>% 
  filter(category > "지")

checkout2 = checkout2  %>% 
  mutate(age_group=cut(age, breaks=seq(20,100,10), 
                       include.lowest=T, labels=paste0(seq(20,90,10), "대"))) %>% 
  filter(category > "지") #한글사전으로 지보다 큰것만 


head(checkout2)
tail(checkout2)

checkout2 %>% group_by(age_group, category) %>% 
  summarise(total=sum(amount)


checkout2 %>% group_by(age_group, category) %>% 
  summarise(total=sum(amount)) %>% 
  ggplot(aes(age_group, category, fill=total)) +
  geom_tile()+
  scale_fill_distiller(palette="Blues", direction=1)



checkout2 %>% group_by(age_group, category) %>%
  summarise(total=sum(amount)) %>% 
  ggplot(aes(age_group, total, fill=category)) +
  geom_col()



checkout2 %>% group_by(age_group, category) %>% 
  summarise(total=sum(amount)) %>% 
  ggplot(aes(age_group, total, fill=category)) +
  geom_col(position = "fill")



### CarSales 데이터

CarSales %>%  group_by(employeeName, productName) %>% 
  summarise(total=sum(quantityOrdered * priceEach))


CarSales %>%  group_by(employeeName, productName) %>% 
  summarise(total=sum(quantityOrdered * priceEach))   %>% # arrange(desc(total))
  slice_max(total, n=5)  #각 직원별로 판매실적이 높은 5개 상품



CarSales %>%  group_by(employeeName, productName) %>% 
  summarise(total=sum(quantityOrdered * priceEach))   %>% # arrange(desc(total))
  slice_max(total, n=5) %>%  #각 직원별로 판매실적이 높은 5개 상품
  ggplot(aes(employeeName, productName, fill=total)) +
  geom_tile()+
  scale_fill_distiller(palette="Blues", direction=1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

CarSales %>%  group_by(employeeName, productName) %>% 
  summarise(total=sum(quantityOrdered * priceEach))   %>% # arrange(desc(total))
  ggplot(aes(employeeName, productName, fill=total)) +
  geom_tile()+
  scale_fill_distiller(palette="Blues", direction=1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## 연습문제 11-16

# 11-15의 grade2를 이용하여 다음에 답하시오.
# 1) 성별과 과목에 따른 점수의 평균을 그래프로 표현하시오.
grade2 %>% group_by(gender, subject) %>%
  summarise(average=mean(score)) %>% 
  ggplot(aes(gender, subject, fill=average, label=average)) +
  geom_tile()+
  geom_text()

# 2) 이름별로 과목에 대한 점수의 평균을 그래프로 표현하시오.
grade2 %>% group_by(name, subject) %>%
  summarise(average=mean(score)) %>% 
  ggplot(aes(name, subject, fill=average, label=average)) +
  geom_tile()+
  geom_text()





