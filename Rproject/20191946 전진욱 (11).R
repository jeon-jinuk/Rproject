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
str(body)
#시계열

a=c(3,2,4,1,5)
b=c(5,3,4,5,5)
c=c(4,5,4,3,1)



# Make a basic graph
#data.frame(a,b) %>% ggplot(aes(x=a,y=b)) +
ggplot(data.frame(a,b), aes(a,b)) +
  geom_line( color="black",linetype = 2) +
  geom_point(shape=22, color="red", fill="blue", size=6) +
  theme_ipsum() +
  ggtitle("Basic Connected scatterplot ")



# Libraries

library(ggplot2)
library(dplyr)
library(hrbrthemes)



# Load bitcoin dataset from github

bitcoin <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
str(bitcoin)
bitcoin$date <- as.Date(bitcoin$date)

str(tqqq)

# Plot

bitcoin %>%
  #tail(100) %>%
  ggplot( aes(x=date, y=value)) +
  geom_line( color="grey") +
  #geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
  theme_ipsum() +
  ggtitle("Evolution of bitcoin price")
# 과제
#yahoo finance tqqq자료 이용
tqqq %>%
  #tail(100) %>%
  ggplot( aes(x=Date, y=Close)) +
  geom_line( color="grey") +
  #geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
  theme_ipsum() +
  ggtitle("Evolution of bitcoin price")

#################################################

## 흐름도 

library(ggplot2)
library(dplyr)
library(babynames)
library(ggrepel)
library(tidyr)





d=data.frame( n=c(1,2,3),  m=c(2,3,5),z=c('a','b','c'))
ggplot(d,aes( x=n, y=m, label=z)) +
  geom_point(col="red") +
  geom_segment( aes( xend=c(2, 2.5, 2.75), yend=c(4,4,3)) ) + 
  # segment는 geom_point의 각 점에서 하나씩 시작해서 그린다. 
  # 그 끝을 여기에서 지정한다.  
  geom_text_repel() # 라벨을 추가  

ggplot(d,aes( x=n, y=m, label=z)) +
  geom_point(col="red")+
  geom_line()


ggplot(d,aes( x=n, y=m, label=z)) +
  geom_point(col="red") +
  geom_segment( aes( xend=c(2,3,NA), yend=c(3,5,NA)),
        arrow=arrow(length=unit(3,"cm"),type="closed"),  # 화살표크기
        arrow.fill = "red",
# lineend = "butt", #text position
        linejoin = "round") + 
  geom_text_repel() 



x=1:10
head(x)
head(x,n=3)
head(x,n=-1)
tail(x,2)
tail(x,n=3)
tail(x,n=-1)



df=read_xlsx("C:/Users/admin/Desktop/Data_viz/my_data.xlsx", sheet=5)
df1=df[1:5,]
df1

df1$income
c(tail(df1$income, n=-1), NA)
c(tail(df1$exp_life, n=-1), NA)



df1 %>% 
  ggplot(aes(x=income, y=exp_life, label=year)) +
  geom_point(color=rgb(1,0,0)) +
  geom_text_repel() +
  geom_segment(color=rgb(0,0,1), 
  aes( xend=c(tail(income, n=-1), NA), 
           yend=c(tail(exp_life, n=-1), NA)),
  arrow=arrow(length=unit(0.5,"cm"))
  ) 



df %>% 
  ggplot(aes(x=income, y=exp_life, label=year)) +
  geom_point(color=rgb(1,0,0)) +
  geom_text_repel() +
  geom_segment(color=rgb(0,0,1), 
  aes( xend=c(tail(income, n=-1), NA), 
  yend=c(tail(exp_life, n=-1), NA)),
  arrow=arrow(length=unit(0.3,"cm"))
  ) 





library(dplyr)
score <- data.frame(
  subject = c('hist','math','sci','stat'),
  kim = c(100, 90, 100,95),
  lee = c(80, 90, 72, 99),
  park = c(70, 60, 40, 90)
)

# long format

score_long <- score %>% gather(name, grade, -1) 

# subject는 그대로 사용하고,  name과 grade변수를 생성한다. 
# name에는 변수 이름이 값이 되고
# score에 있던 값이  grade의 값이 된다. 
# wide format 

score_long %>% spread(name, grade,-1) 

# name의 값이 변수가 된다.
# grade의 값이 만든 데이터프레임 값이 된다. 







head(babynames) # 이름의 연도별 빈도
str(babynames)
view(babynames)

# 이름이 얼마나 많이 사용되는가

data <- babynames %>% 
  filter(name %in% c("Ashley", "Amanda")) %>%
  filter(sex=="F") %>%
  filter(year>1970) %>%
  select(year, name, n)%>% 
  spread(name, n)





# plot 

data %>% 
  ggplot(aes(x=Amanda, y=Ashley, label=year)) +
  geom_point(color="#69b3a2") +
  geom_text_repel() +
  geom_segment(color="#69b3a2", 
               aes(
                 xend=c(tail(Amanda, n=-1), NA), 
                 yend=c(tail(Ashley, n=-1), NA)
               ),
               arrow=arrow(length=unit(0.3,"cm"))
  ) +
  theme_ipsum()



# 연습문제 12-1

# 1) Elizabeth, Mary의 연도별 빈도 변화를 나타내는 흐름도 그래프를 그리시오. 
data1 <- babynames %>% 
  filter(name %in% c("Mary", "Elizabeth")) %>%
  filter(sex=="F") %>%
  filter(year>1970) %>%
  select(year, name, n)%>% 
  spread(name, n)


data1 %>% 
  ggplot(aes(x=Elizabeth, y=Mary, label=year)) +
  geom_point(color="#69b3a2") +
  geom_text_repel() +
  geom_segment(color="#69b3a2", 
               aes(
                 xend=c(tail(Elizabeth, n=-1), NA), 
                 yend=c(tail(Mary, n=-1), NA)
               ),
               arrow=arrow(length=unit(0.3,"cm"))
  ) +
  theme_ipsum()
# 2) 3번의 수학 성적과 영어 성적은 다음과 같다. 이를 wide format으로 변경하여
   score = data.frame( number=rep(c(1,2,3)), sub= rep(c('math', 'sci'), each=3),
                            score=c(100,90,80 ,90,100,90))
   score
   data1 <- score %>% spread(sub, score,-1) 
   
   # plot 
   
   data1 %>% 
     ggplot(aes(x=math, y=sci, label=number)) +
     geom_point(color="#69b3a2") +
     geom_text_repel() +
     geom_segment(color="#69b3a2", 
                  aes(
                    xend=c(tail(math, n=-1), NA), 
                    yend=c(tail(sci, n=-1), NA)
                  ),
                  arrow=arrow(length=unit(0.3,"cm"))
     ) +
     theme_ipsum()
   
#    성적의 변화를 나타내는 흐름도 그래프를 그리시오. 
#   score = data.frame( number=rep(c(1,2,3)), sub= rep(c('math', 'sci'), each=3),
#                            score=c(100,90,80 ,90,100,90))
#================================================================

########################################################



# parallel coordinate plot
# 여러개의 양적변수의 변화 + 한개의 질적변수 

library(hrbrthemes)
library(GGally)
library(viridis)



ggparcoord(data = body,
           columns = 1:3,
           groupColumn = "gender")



ggparcoord(data = body,
           columns = 1:3) +
  facet_wrap(~ gender)



# iris 데이터 

str(iris)
ggparcoord(iris,
           columns = 1:4, groupColumn = 5, 
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3)




#diamonds data 

data(diamonds, package="ggplot2")
str(diamonds)
n=nrow(diamonds)
sample(1:100,5)

# 너무 큰 데이터는 그래프가 너무 복잡해, 100개만으로 확인 

df <- diamonds[sample(1:n, 100), ]

# basic parallel coordinate plot, using default settings
ggparcoord(data = df, columns = c(1, 5:10))

# this time, color by diamond cut
ggparcoord(data = df, columns = c(1, 5:10), groupColumn = 2)



# 연습문제 12-2

#  iris의 Sepal.Length와 Sepal.Width를 이용한 parallel coordinate plot을 그리시오. 
# 각 Species마다 다른창에서 그리시오(facet_wrap 이용). 
ggparcoord(iris,
           columns = 1:2, groupColumn = 5, 
           showPoints = TRUE, 
           title = "species with Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3)+
  facet_wrap(~Species)



##########################################################################
##
## 폭포수 차트
#누적된것을 나타낸다


install.packages("ggalluvial")
library(ggalluvial)
install.packages("waterfalls")
library(waterfalls)



## 데이터 생성 

group <- LETTERS[1:8]
value <- c(2000, 4000, 2000, -1500, -1000, -2500,1000,-2000)
df <- data.frame(x = group, y = value) 
waterfall(df)

# OR

waterfall(values = value, labels = group)


x=c(3,5,1)
seq_along(x)



## 은행 잔고의 변화 
## 처음 잔고, 판매금, 반환, 지불, 법정 손실, 법정 이익, 계약, 현금 종료    

balance <- data.frame(desciption = factor(c("처음 잔고",
                                            "판매금 입금", "환불금", "세금", "특허 패소",
                                            "세금 환불", "지점입금", "판매금 입금")), 
                      amount = c(2000, 3400, -1100, -100, -6600, 3800, 1400, 2800))
waterfall(balance, fill_by_sign=F, fill_colours = 2:7, calc_total=T) #마지막 누적값 계산

# fill_by_sign= F 되어야, fill_colours = 2:7이 유효함
#마지막막대는 최종 금액이다
##

# 연습문제 12-3
# 우리회사는 판매, 서비스, 고정경비, 변동경비, 세금(단위:억)으로 각각
# 101, 52, -23, -15, -100 을 사용하였다. 이를 폭포수차트로 표현하시오.  
mycom <- data.frame(desciption = factor(c("판매",
                                            "서비스", "고정경비", "변동경비", "세금(단위:억)")), 
                      amount = c(101, 52, -23, -15, -100))
waterfall(mycom, fill_by_sign=F, fill_colours = 2:7, calc_total=T) 


##################################################################

# line charts



# Libraries

library(tidyverse)
library(hrbrthemes)
library(plotly)
install.packages("plotly")
library(patchwork)
install.packages("patchwork")
library(babynames)
library(viridis)



# Load dataset from github

don <- babynames %>% 
  filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
  filter(sex=="F")



# Plot

don %>%
  ggplot( aes(x=year, y=n, group=name, color=name)) +
  geom_line() +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  ylab("Number of babies born")



# 연습문제 12-4

# ALice, Emma, Anns, Mary의 연도별 돗수를 나타내는 선그래프를 그리시오. 
don1 <- babynames %>% 
  filter(name %in% c("Alice", "Emma", "Ann", "Mary")) %>%
  filter(sex=="F")

# Plot

don1 %>%
  ggplot( aes(x=year, y=n, group=name, color=name)) +
  geom_line() +
  ggtitle("Alice, Emma, Ann, Mary's A line graph showing frequency by year ") +
  theme_ipsum() +
  ylab("Number of babies born")






