library(tidyverse)

library(readxl)

library(dplyr)

library(magrittr)

library(ggplot2)

library(hrbrthemes)

library(ggridges)

library(wordcloud2) 

library(fmsb)

library(viridis)

library(gapminder)

library(ggExtra)

library(GGally)

library(babynames)
install.packages("ggmosaic")

my_check = read_xlsx("C:/Users/admin/Desktop/Data_viz/my_data.xlsx",sheet = 1)
my_check_2 = read_xlsx("C:/Users/admin/Desktop/Data_viz/my_data.xlsx",sheet = 2)
CarSales = read.csv("C:/Users/admin/Desktop/Data_viz/CarSales.csv")
checkout2 = read.csv("C:/Users/admin/Desktop/Data_viz/checkout2.csv")
my_check$id = as.factor(my_check$id)
my_check$prod_name = as.factor(my_check$prod_name)
my_check$amount = as.factor(my_check$amount)
body = read_xlsx("C:/Users/admin/Desktop/Data_viz/my_data.xlsx", sheet=4)
tqqq = read.csv("C:/Users/admin/Desktop/Data_viz/TQQQ.csv")
cumsum(c(5,2,3))





# 난수 생성
set.seed(2)
n=10
x = round(rnorm(n,1,2),1) # 하루 하루 이익(억)
x

cumsum(x) # 10일 누적이익



# 3명의 이익
# 데이터프레임
df=data.frame(x = rep(1:n,3), 
              profit = c( cumsum(rnorm(n)), cumsum(rnorm(n)) , cumsum(rnorm(n))), 
              group = factor(rep(c(1,2,3), each=n)))
df

# 3개의 라인차트 

ggplot(df, aes(x = x, y = profit, color = group)) +
  geom_line(linetype = 1,lwd = 1)



# 동전을 던져서 앞면이면 +1, 뒷면이면 -1인 실험을 100번 

n=100
rbernoulli(2)*1
rbernoulli(n)*1



set.seed(2)
x=rbernoulli(n)*2-1
cumsum(x)

# 이 실험을 3번 반복

n=10000
df=data.frame(x = rep(1:n,3), 
              value = c( cumsum(rbernoulli(n)*2-1), 
                         cumsum(rbernoulli(n)*2-1), 
                         cumsum(rbernoulli(n)*2-1)), 
              group = factor(rep(c(1,2,3), each=n)))
df
ggplot(df, aes(x = x, y = value, color = group)) +
  geom_line(linetype = 1,lwd = 0.6)









library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)



# Build dummy data

# 기온과 삼산전자 주가

##  단위가 다른 두 개의 line charts

n=100 # 100일 동안
data <- data.frame(
  day = as.Date("2019-01-01") + 0:(n-1),
  price = runif(n)*2000 + seq(100,1)^2 ,
  temperature = runif(100)*2 + seq(1,100)^1.5/100)+10



data$day

# Most basic line chart

p1 <- ggplot(data, aes(x=day, y=price)) +
  geom_line(color="grey",size=1) +
  ggtitle("Price") +
  scale_x_date(date_labels = "%m-%d", date_breaks = "1 month") +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme_ipsum()



p2 <- ggplot(data, aes(x=day, y=temperature)) +
  geom_line(color="#69b3a2", size=1) +
  ggtitle("Temperature") +
  scale_x_date(date_labels = "%y-%m-%d", date_breaks = "1 month") +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme_ipsum()



# 한 화면에 두개 차트

p1 + p2

######################################################

## 같은 화면에 



# 축을 맞추기 위해 적당한 값을 더해주고 곱해주고.

ggplot(data, aes(x=day)) +
  geom_line( aes(y=price, color="price")) +
  geom_line( aes(y = (temperature-10) * 1000, color="temperature"))+
  scale_y_continuous(sec.axis = sec_axis( ~ ./1000+10, name = "temperature"))



# 연습문제 13-0

# 다음을 완성하시오.

df3 <- data.frame(x = 1:10,
                  y1= (10:1),
                  y2= (1:10)*1000)



ggplot(df3, aes(x=x)) +
  geom_line( aes(y = y1     , color="y1")) +
  geom_line( aes(y = y2/1000, color="y2"))+
  scale_y_continuous(sec.axis = sec_axis( ~ .*, name = "y2"))



###############################



k_water= read.csv("C:/Users/admin/Desktop/Data_viz/K_water_drought.csv", fileEncoding = "EUC-KR")
k_water = k_water[,1:7]
colnames(k_water)=c("sido","gungu","myundong","type", "pop","start","end")
head(k_water)
str(k_water)
k_water$sido=as.factor(k_water$sido)
k_water$gungu=as.factor(k_water$gungu)
k_water$myundong=as.factor(k_water$myundong)
k_water$type=as.factor(k_water$type)
k_water$start=as.Date(k_water$start)
k_water$end=as.Date(k_water$end)



k_water %>% 
  group_by(sido) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=sido, y=n)) +
  geom_col()



k_water = k_water %>% mutate(year = format(start, format="%Y"))  



str(k_water)

k_water %>% 
  group_by(year) %>% 
  summarise(total=sum(pop)) %>% 
  ggplot(aes(total))+
  geom_boxplot()

# 과제 13-1

# histogram과 density 그림을 그리시오

# 히스토그램

k_water %>% 
  group_by(year) %>% 
  summarise(total=sum(pop)) %>% 
  ggplot(aes(total))+
  geom_histogram()


# density
k_water %>% 
  group_by(year) %>% 
  summarise(total=sum(pop)) %>% 
  ggplot(aes(total))+
  geom_density(fill="blue",color="red",alpha=0.2)




k_water %>% 
  group_by(year) %>% 
  summarise(total=sum(pop)) %>% 
  ggplot(aes(year, total))+
  geom_col()



k_water %>% 
  group_by(year) %>% 
  summarise(total=sum(pop)) %>% 
  ggplot(aes(x=year, y=total, group=1))+
  geom_point() + geom_line()



k_water %>% 
  group_by(sido) %>% 
  summarise(total=sum(pop), n=n()) %>% 
  ggplot(aes(sido, total))+
  geom_col()



k_water %>% 
  group_by(sido) %>% 
  summarise(total=sum(pop),n=n()) %>% 
  ggplot(aes(sido, n))+
  geom_col()



# 과제 13-2

# 1) 피해 인구수 막대그래프는 인구수가 많은 순서로 그리고, x축의 라벨을 수직으로 쓰시오.
k_water %>% 
  group_by(sido) %>% 
  summarise(total=sum(pop),n=n()) %>% 
  ggplot(aes(sido, total))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




# 2) 피해 건수의 파이차트를 그리시오. 파이차트에 피해 건수와 피해 인구수를 쓰시오.
data1 <- k_water%>%
  group_by(sido) %>% 
  summarise(total=sum(pop),n=n())
ggplot(data1,aes(x="",y=n,fill=sido))+
  geom_col(width=1) +
  coord_polar("y")+
  theme_void()+
geom_label(aes(label = n),
           position = position_stack(vjust = 0.5)
           ) +
  geom_text(aes(label = total), color="white",
            position = position_stack(vjust = 0.2)
            )

head(k_water)



# 피해를 가장 자주 본 10개 군구

k_water %>% 
  group_by(gungu) %>% 
  summarise(total=sum(pop),n=n()) %>% 
  slice_max(n, n=10) %>% 
  ggplot(aes(gungu, n))+
  geom_col()



# 피해인구가 가장 많은 10개 군구

k_water %>% 
  group_by(gungu) %>% 
  summarise(total=sum(pop),n=n()) %>% 
  slice_max(total, n=10) %>% 
  ggplot(aes(gungu, total))+
  geom_col()



# 과제 13-3
# 파이차트로 나타내시오. 가장 아름답게
# x 축이 크기 순서 
data1 <- k_water%>%
  group_by(gungu) %>% 
  summarise(total=sum(pop),n=n())%>% 
  slice_max(total, n=10) %>% 
ggplot(aes(x="",y=n,fill=gungu))+
  geom_col(width=1) +
  coord_polar("y")+
  theme_void()+
  geom_label(aes(label = n),
             position = position_stack(vjust = 0.5)
  ) +
  geom_text(aes(label = total), color="white",
            position = position_stack(vjust = 0.2)
  )
data1





# 연도에 따른 제한운반, 제한급수와 운반급수의 빈도

k_water %>% 
  group_by(year, type) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(fill=type, y=n, x=year)) + 
  geom_bar(position="stack", stat="identity")



k_water %>% 
  group_by(year, type) %>% 
  summarise(n=n(), .groups = "drop") %>% 
  complete(year, type, fill = list(n = 0)) %>% 
  ggplot(aes(fill=type, y=n, x=year)) + 
  geom_bar(position="stack", stat="identity")





# 연도별 시도 피해

k_water %>% 
  group_by(year, sido) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(fill=sido, y=n, x=year)) + 
  geom_bar(position="stack", stat="identity")



k_water %>% 
  group_by(year, sido) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(fill=sido, y=n, x=year)) + 
  geom_col()



# 연도별 가장 피해가 많은 2 개 시도

k_water %>% 
  group_by(year, sido) %>% 
  summarise(total=sum(pop)) %>% 
  slice_max(total,n=2) %>%
  ggplot(aes(fill=sido, y=total, x=year)) + 
  geom_col()



# 2017년 시도별 피해

k_water %>% 
  group_by(year, sido) %>% 
  summarise(total=sum(pop)) %>% 
  filter(year==2017) %>% 
  ggplot(aes(y=total, x=sido)) + 
  geom_col()



# 시도별 연도 피해

k_water %>% 
  group_by(year, sido) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(fill=year, y=n, x=sido)) + 
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# 시도별 피해건수가 가장 많은 3개 군구 

k_water %>% 
  group_by(sido, gungu) %>% 
  summarise(total=sum(pop)) %>% 
  slice_max(total, n=3) %>% 
  ggplot(aes(y=total, x=sido, fill=gungu)) + 
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# 연도를 묶어서 

k_water$year=as.numeric(k_water$year)



k_water %>% 
  mutate(year_group=cut(as.numeric(year), breaks=c(2006,2010,2015,2021), 
                        include.lowest=T, labels=c("2010이전","2015이전","2020이전"))) %>% 
  group_by(year_group) %>% 
  summarise(t=sum(pop),n=n())





#과제 13-4

# "2010이전","2015이전","2020이전" 각각에 가장 피해인구가 많은 3개 시도를 찾고 
# 이를 그래프로 표현하시오.
# 
k_water %>% 
  mutate(year_group=cut(as.numeric(year), breaks=c(2006,2010,2015,2021), 
                        include.lowest=T, labels=c("2010이전","2015이전","2020이전"))) %>% 
  group_by(year_group,sido) %>% 
  summarise(total=sum(pop),n=n())%>%
  slice_max(total, n=3) %>% 
  ggplot(aes(y=total, x=year_group, fill=sido)) + 
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#과제 13-5

#### 연도별 피해 건수와 피해 인구수(버블)를 버블차트로 나타내시오. 그리고 각 버블에
# 가장 피해인구가 많은 시도를 나타내시오. 
k_water %>% 
  group_by(year, sido) %>% 
  summarise(total=sum(pop),n=n()) %>% 
  ggplot(aes(x=year, y=total, size=n, fill=sido)) +
  geom_point(alpha=0.7, shape=21, color="black") +
  scale_size(range = c(3, 10)) 




market = read_xlsx("C:/Users/admin/Desktop/Data_viz/my_data1.xlsx", sheet=9)
market %>% 
  ggplot(aes(x=`점유율`, y=`판매량`, size=`판매증가율`)) +
  geom_point(alpha=0.7, shape=21, color="black") +
  scale_size(range = c(3, 10)) 



#연습문제 13-5
# 위 버블차트에 회사이름을 추가하시오. 
market$`회사이름`=as.factor(market$`회사이름`)
market %>% 
  ggplot(aes(x=`점유율`, y=`판매량`, size=`판매증가율`, fill=`회사이름`)) +
  geom_point(alpha=0.7, shape=21, color="black") +
  scale_size(range = c(3, 10)) 
