###########=======================

##  2D-density plot, 산점도

###-----------------------------------

##

## 산점도의 목적은 연관성을 찾는 것

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
str(body)

#산점도 기본

body %>% ggplot(aes(height, weight)) +
  geom_point() 



body %>% ggplot(aes(height, weight)) +
  geom_point() +
  geom_smooth()



body %>% ggplot(aes(height, weight)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, se=F)
#회귀곡선을 그려준다 
#회색부분은 신뢰대(cinfidence band이다 





#점에 관해

body %>% ggplot(aes(height, weight)) +
  geom_point(
    color="red", #테두리
    fill="blue",
    shape=22,
    alpha=0.5,
    size=2,
    stroke = 3) + # 테두리 굵기
  theme_ipsum() # 바닥면



# 2양적변수 * 1질적변수의 산점도(color 지정)

body %>% ggplot(aes(x=height, y=weight, color=gender)) +
  geom_point(size=5)+
  theme_ipsum()



# 연습문제  12-1

str(iris)
# 1) iris 데이터의 Sepal.Length와 Sepal.Width로 산점도를 그리시오. 
iris %>% ggplot(aes(Sepal.Length, Sepal.Width)) +
  geom_point() +
  geom_smooth()
# 2) 위 1) 산점도에 Species에 따른 색상을 다르게 지정하시오.  
iris %>% ggplot(aes(Sepal.Length, Sepal.Width, col=Species)) +
  geom_point() +
  geom_smooth()


## bubble chart(3개 양적변수 * 1개 질적변수)
body %>%
  ggplot( aes(x=height , y=weight )) +
  geom_point(alpha=0.7)



body %>%
  ggplot( aes(x=height , y=weight, col= gender )) +
  geom_point(alpha=0.7)





body %>%   #그래프 해석
  ggplot( aes(x=height , y=weight , size = age )) +
  geom_point(alpha=0.7)


body %>%
  ggplot( aes(x=height , y=weight , size = age )) +
  geom_point(alpha=0.7) +
  scale_size(range = c(3, 10))



body %>%
  ggplot( aes(x=height , y=weight , size = age, fill=gender )) +
  geom_point(alpha=0.7, shape=21, color="black") +
  scale_size(range = c(3, 10)) 



# The dataset is provided in the gapminder library
library(gapminder)
str(gapminder)
head(gapminder)
nrow(gapminder)
tail(gapminder$year)
df <- gapminder %>% filter(year== 2007) %>% dplyr::select(- year)
#dplyr:select 하는이유는 변수를 선택하기 위해 (변수6개)

# Most basic bubble plot

df %>%
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop)) +
  geom_point(alpha=0.4)



df %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size = pop)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 20), name="Population (M)")



df %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 20), name="Population (M)")





df %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, fill=continent)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 15), name="Population (M)") +
  scale_fill_viridis(discrete=TRUE, option="A") +
  theme_ipsum() +
  #theme(legend.position="bottom") +
  ylab("Life Expectancy") +
  xlab("Gdp per Capita") 



# 연습문제  12-2
# iris 데이터의 Species에 따른 색상을 지정하고
# 점의 크기는 Petal.Length에 비례하는 Sepal.Length와 Sepal.Width로 산점도를 그리시오. 
iris%>%
  ggplot( aes(x=Sepal.Length, y=Sepal.Width, size = Petal.Length, color=Species)) +
  geom_point(alpha=0.4)


# 산점도에 각 변수의 분포를 나타내는 히스토그램, density, boxplot 추가
library(ggExtra)

# classic plot :
(p <- body %>% ggplot(aes(x=height, y=weight, color=gender)) +
    geom_point(size=5) )
body %>% ggplot(aes(x=height)) + geom_density()
body %>% ggplot(aes(x=weight)) + geom_density()
# with marginal histogram
# 각 변수의 분포를 같이 고려함 
ggMarginal(p) #  default는 density
# marginal density
ggMarginal(p, type="density",
           xparams = list(bw=3),
           yparams = list(bw=7))

ggMarginal(p, type="histogram",
           xparams = list(bins=7))
#xparams x의갯수를 지정
ggMarginal(p, type="histogram", bins=7)
#bin (x,y)막대기수를 7개로 하라


# marginal boxplot
ggMarginal(p, type="boxplot")



# gender를 고려한 그래프 
# default는 density
ggMarginal(p, groupColour = TRUE, groupFill = TRUE)
ggMarginal(p,  type="histogram", bins=7,groupColour = TRUE, groupFill = TRUE)





# iris data
piris <- ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
  geom_point()
piris
ggMarginal(piris, groupColour = TRUE, groupFill = TRUE)



# # 연습문제  12-3
# 1) iris 의  Petal.Length와 Petal.Width를 이용하여 위 그래프와 같이 산점도과 각 변수의 
#  분포를 density로 표현하시오.
piris <- ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point()
piris
ggMarginal(piris, groupColour = TRUE, groupFill = TRUE)
# 2) 위 그래프에 분포를 히스토그램으로 표현하시오. 
ggMarginal(piris,  type="histogram", bins=7,groupColour = TRUE, groupFill = TRUE)




## 분포 

# 자료는 어디에 많이 모여 있는가? 
#분포를 파악하는 것이 목적

body %>% ggplot(aes(height, weight)) + 
  geom_point() +
  geom_density_2d() +
  xlim(100,200) + # x축과 y축의 범위를 결정
  ylim(0, 100)





body %>% ggplot(aes(height, weight)) + 
  geom_density_2d() +
  geom_point() + facet_wrap(vars(gender)) + #성별에 따라 따로 그려라
  xlim(100,200) + 
  ylim(0, 100)




str(faithful)
m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point() +
  xlim(0.5, 6) + # x축과 y축의 범위를 결정
  ylim(40, 110)
m

# contour lines
m + geom_density_2d()



# contour bands
m + geom_density_2d_filled(alpha = 0.5, bins=5)
#밀도는 가능성 상대적으로 말하기
#bin 등고선의 갯수 지정


# contour bands and contour lines
m + geom_density_2d_filled(alpha = 0.5) +
  geom_density_2d(size = .5, colour = "black") # size는 등고선 굵기



## 상관분석 --------------------------------

library(GGally)
#gender 변수x
ggpairs(body[,-4], title="correlogram with ggpairs()") 
#gender 변수ㅇ
ggpairs(body,  ggplot2::aes(color=gender)) 



# ## 연습문제 12-4
str(flea)
# flea 데이터를 이용한다.뜀벼룩갑충 3 종의 신체 측정. 
# 다리 제1관절 길이,..,기관의 너비,..,머리크기 
 head(flea)
 dfflea <- flea %>% dplyr::select(- species, -tars1, -tars2, -head)
 ggpairs(dfflea)
# 1) 변수 aede1, aede2, aede3의 상관계수 행렬과  산점도를 그리시오.

   ggpairs(flea, columns =5:7)

# 2-1) 1)에서 그린 그림에 color=species를 추가하시오. 
    ggpairs(flea, columns =5:7,ggplot2::aes(color=species))

#  2-2) species가 Concinna 일 때 aede1과 aede2의 상관계수는?
    -0.194
#  2-3) species가 Heptapot 일 때 aede2과 aede3의 상관계수는?
    0.049
#  아래의 프로그램을 참고하시오
#flea %>% ggplot(aes(x=     , y= , size=      , fill=      )) +

#  geom_point(alpha=0.5, shape=1, color="blue")

# 3) aede1과 aede2의 산점도를 그리시오.
    flea %>% ggplot(aes(x=aede1, y=aede2)) +
    geom_point(alpha=0.5, shape=1, color="blue")
    
# 4) aede1과 aede2의 산점도에 점의 크기를 aede3로 지정하시오.
flea %>% ggplot(aes(x=aede1, y=aede2, size= aede3)) +
  geom_point(alpha=0.5, shape=1, color="blue")
    
    
# 5) 4)의 그림에 species로 색을 지정하시오.
flea %>% ggplot(aes(x=aede1, y=aede2, size= aede3, color=species)) +
  geom_point()


# 6) tars1의 분포를 나타내는  density 를 그리시오.
flea %>% ggplot(aes(x=tars1)) + geom_density()
# 7) tars1과 tars2의 결합 분포를 나타내는 그림을 그리시오. 가장 밀도가 높은 곳의 중심 좌표를 쓰시오. 
mflea = flea %>% ggplot(aes(tars1, tars2)) + 
  geom_point() +
  geom_density_2d() +
  xlim(100,250) + 
  ylim(100, 150)

mflea + geom_density_2d_filled(alpha = 0.5, bins=5)
(182,127) (199,124)
# 8) tars1과 tars2의 결합 분포를 나타내는 그림을 facet_wrap를 이용하여 species 별로 그리시오.

mflea + facet_wrap(vars(species)) 
 





