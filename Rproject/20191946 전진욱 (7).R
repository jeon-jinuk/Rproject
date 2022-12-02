library(tidyverse)
library(readxl)
library(ggplot2)
library(ggridges)
my_check = read_xlsx("C:/Users/admin/Desktop/Data_viz/my_data.xlsx",sheet = 1)
my_check_2 = read_xlsx("C:/Users/admin/Desktop/Data_viz/my_data.xlsx",sheet = 2)
CarSales = read.csv("C:/Users/admin/Desktop/Data_viz/CarSales.csv")
checkout2 = read.csv("C:/Users/admin/Desktop/Data_viz/checkout2.csv")



## 연,월을 범주형변수로 생각하면

# 연도별 주문액

# 월별 주문횟수를 생각할 수 있다.

#### 날짜 변수 다루기 p.209, Sec. 5.13.1

########--------------------------------------

getwd()
checkout=read.csv("checkout.csv", fileEncoding="UTF-8")
str(checkout) # datetime변수는 문자
checkout %>% tibble %>% head(5)


checkout$datetime[1] # "2020-05-25 00:00:33"



as.Date(checkout$datetime[1])#, format="%Y-%m-%d %H:%M:%s")



# 날짜 변수로 변경

checkout$datetime = as.Date(checkout$datetime)
str(checkout)
checkout %>% tibble %>% head(5)

# 연도만, 월까지만

format(checkout$datetime, format="%Y")[1]
format(checkout$datetime, format="%m-%d")[1]
format(checkout$datetime, format="%y-%m")[1]
format(checkout$datetime, format="%Y-%m-%d")[1]

# date 변수는 group_by 에 사용 가능
checkout %>% group_by(datetime) %>% 
  summarise(sum(amount))

checkout %>% group_by(datetime) %>% 
  summarise(total=sum(amount)) %>% 
  ggplot(aes(datetime, total)) + 
  geom_line() + # x축 표시는 기본
  scale_x_date(date_labels = "%m-%d", date_breaks = "1 day") +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 모든 자료에 대한 분포를 boxplot으로 제대로 볼 수 없음 
checkout %>%
  ggplot(aes(amount)) + 
  geom_boxplot()


# 100개만으로
checkout %>% head(100) %>% 
  ggplot(aes(amount)) + 
  geom_boxplot()


# 날짜 별로 판매액의 분포를 상자그림을 통해 
checkout %>% 
  ggplot(aes(as.factor(datetime), amount)) + 
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


###===================

# CarSales 이용
CarSales$orderDate
str(CarSales)
# 
CarSales$orderDate = as.Date(CarSales$orderDate)



#1. 일별 주문량 합 
CarSales %>% group_by(orderDate) %>% 
  summarise(total=sum(quantityOrdered*priceEach))


CarSales %>% group_by(orderDate) %>% 
  summarise(total=sum(quantityOrdered*priceEach)) %>% 
  ggplot(aes(orderDate, total)) + 
  geom_point()+
  geom_line() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +  # "1 day"로 하면 너무 복잡
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#2. 월별 주문량 합 
format(CarSales$orderDate[1], format="%Y-%m")
format(CarSales$orderDate[1], format="%Y")



# 연월만 있는 새로운 변수 만들고, 월별에 따른 판매량
df <- CarSales %>%  mutate(ym = format(orderDate, format="%Y-%m")) %>% 
  group_by(ym) %>% 
  summarise(total=sum(quantityOrdered*priceEach))
df %>%  head(5)



# 각 월을 각 하나의 그룹으로 간주한다.
# 모든 자료가 하나의 그룹이라고 지정해줘야 geom_line()이 작성된다.
ggplot(df, aes(ym, total, group=1)) + 
  geom_point()+
  geom_line() +  #geom_col() 도 가능한 그림 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

substr('123456789', 1,5)



# format 대신 substr 사용함
CarSales %>%  mutate(year_month = substr(CarSales$orderDate,1,7)) %>% 
  group_by(year_month) %>% 
  summarise(total=sum(quantityOrdered*priceEach)) %>% 
  ggplot(aes(year_month, total, group=1)) + 
  geom_line() +  #geom_col() 도 가능한 그림 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# 각 월별로 판매액의 분포를 상자그림을 통해 본다. 
CarSales %>%  mutate(year_month = substr(CarSales$orderDate,1,7)) %>% 
  mutate(total= quantityOrdered*priceEach) %>% 
  ggplot(aes(year_month, total)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



library(ggridges)



df <- CarSales %>%  mutate(year_month = substr(CarSales$orderDate,1,7)) %>% 
  mutate(total= quantityOrdered*priceEach) %>% 
  filter(year_month>="2004-01") # 2004 년 이후



df %>% 
  ggplot(aes(x = total , y = year_month, fill = year_month)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")



######################==================================

# 과제 11-12

# 1) CarSales 연도별 주문액의 평균을 구하시오.
df <- CarSales %>%  mutate(y = format(orderDate, format="%Y")) %>% 
  group_by(y) %>% 
  summarise(average=mean(quantityOrdered*priceEach))
df
# 2) CarSales 연도별 주문액의 평균을 막대그래프로 표현하시오.
df %>%
  ggplot(aes(x=y, y=average)) +
  geom_col()
  
# 3) CarSales 연도별 주문액의 분포를 geom_density()로 표현하시오.
CarSales %>%  mutate(year= substr(CarSales$orderDate,1,4)) %>% 
  mutate(total= quantityOrdered*priceEach) %>% 
  ggplot(aes(x= year, fill= year)) + 
  geom_density(color="red", alpha=0.3)
  







#################################











##################=====================================================
# 질적변수 2개 
# 성별, 종교
# 고객, 상품
####--------------------------------------------------------------------
# 2차원 빈도표. 분할표
# --------------------------------------------------------------
my_check_2

table(my_check_2$cust_id , my_check_2$prod_name)
addmargins(table(my_check_2$cust_id , my_check_2$prod_name))

ggplot(my_check_2, aes(x = cust_id)) + 
  geom_bar()

ggplot(my_check_2, aes(x = prod_name)) + 
  geom_bar()

ggplot(my_check_2, aes(x = cust_id, fill = prod_name)) + 
  geom_bar()

ggplot(my_check_2, aes(x = cust_id, fill = prod_name)) + 
  geom_bar(position = "dodge")




install.packages("ggmosaic")
library(ggmosaic)



#  사용되는 변수는 질적변수이어야 한다.

# 개별 비율

ggplot(data = my_check_2) +
  geom_mosaic(aes(x = product(cust_id), fill=prod_name))



ggplot(data = my_check_2) +
  geom_mosaic(aes(x = product(prod_name), fill=cust_id))







################################



## mtcars 이용



mc = as_tibble(mtcars)
str(mc)
mc$cyl = as.factor(mc$cyl)
mc$gear = as.factor(mc$gear)
str(mc)
addmargins(table(mtcars$gear, mtcars$cyl ))


ggplot(mc, aes(x = gear, fill = cyl )) + 
  geom_bar(position = "dodge")


ggplot(mc, aes(x = gear, fill = cyl )) + 
  geom_bar()


ggplot(data = mc) +
  geom_mosaic(aes(x = product(gear), fill=cyl))

#모자이크는 빈도를 가장 잘나타낸다
mosaicplot(~ gear + cyl, data = mc, color = TRUE, las = 1)
#############################################################

#######################################################################

## 연습문제 ========================

#  11-13. mtcars의 carb와 실린더 갯수(cyl)의 분할표를 그리고
#  그래프로 표현하시오.
mc = as_tibble(mtcars)
str(mc)
mc$cyl = as.factor(mc$cyl)
mc$carb = as.factor(mc$carb)
str(mc)
addmargins(table(mtcars$carb, mtcars$cyl ))

ggplot(mc, aes(x = carb, fill = cyl )) + 
  geom_bar(position = "dodge")


ggplot(mc, aes(x = carb, fill = cyl )) + 
  geom_bar()


ggplot(data = mc) +
  geom_mosaic(aes(x = product(carb), fill=cyl))






######_---------------------------------

#----------------------- Berkeley Admission ----------------------------

data(UCBAdmissions)
str(UCBAdmissions)
tbl = apply(UCBAdmissions, c(2,1), sum)
mosaicplot(tbl, color=c("red","grey"), main = "UC Berkeley Admissions")



mosaicplot(~ Dept + Gender + Admit, data=UCBAdmissions, 
           
           color=c("red","blue"), dir=c("v","v","h"), off=1, main = "UC Berkeley Admissions")

##------------------------------------------



############============================

#######################################################################

## 연습문제 ========================

#  11-14. exam 데이터를 그래프로 표현하는 다음 프로그램을 완성하시오.


exam = read_xlsx("C:/Users/admin/Desktop/Data_viz/my_data.xlsx", sheet=3)
exam$gender=factor(exam$gender)
exam$pass=factor(exam$pass)
addmargins(table(exam$gender, exam$pass))



ggplot(exam, aes(x =gender, fill= pass )) + 
  geom_bar()



ggplot(exam, aes(x =gender, fill =  pass)) + 
  geom_bar(position = "dodge")


ggplot(data = exam) +
  geom_mosaic(aes(x =product(gender), fill=pass))


mosaicplot( ~gender+ pass, data = exam, color=c("red","grey","blue"))
