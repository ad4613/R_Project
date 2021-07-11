library(treemap)
library(dygraphs)
library(xts)
library(ggplot2)
library(dplyr)
library(rJava)
library(RJDBC)
library(wordcloud)
library(RColorBrewer)
library(tidyr)
library(kormaps2014)
library(mapproj)
library(ggiraphExtra)
library(plotly)

###############DB연동#######################################
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver"
                   , classPath="C://ojdbc6.jar")

conn <- dbConnect(jdbcDriver, 
                  "jdbc:oracle:thin:@localhost:1521/xe", "scott", "tiger")




#######################################################################
## oracle에서 테이블 가져오기 ##
weather <- dbGetQuery(conn, "SELECT * FROM scott.weather")
hot <- dbGetQuery(conn, "SELECT * FROM scott.hot")

head(weather)
head(hot)

## 변수명 수정 ##
weather <- rename(weather,
                  date = DATE1,
                  mean_tem = MEAN_TEM,
                  max_tem = MAX_TEM,
                  min_tem = MIN_TEM,
                  prec = PREC,
                  area = AREA,
                  code=CODE)
hot <- rename(hot,
              time = TIME,
              location = LOCATION,
              field = FIELD,
              effect = EFFECT)

## year, month 열 생성 ##
weather <- transform(weather, year = substr(date, 1, 4))
weather <- transform(weather, month = substr(date, 6, 7))

##locate와 area를 분리 ##
hot <- separate(data=hot, col=location ,
                sep="_",into=c("locate","area"))

View(hot)
## 계절 추가 ##
weather[ weather$month %in% c("03","04","05"), "season"] = "봄"
weather[ weather$month %in% c("06","07","08"), "season"] = "여름"
weather[ weather$month %in% c("09","10","11"), "season"] = "가을"
weather[ weather$month %in% c("12","01","02"), "season"] = "겨울"
weather$season <- factor(weather$season, levels=c("봄","여름","가을","겨울"), order=T)

## 날짜를 Date타입으로 변경 ##
weather$date <- as.Date(weather$date)


str(weather)
View(weather)

str(hot)
View(hot)


#######################################################################

## 전국 월별 평균기온 평균##
month_tem<- weather %>% 
  group_by(month) %>% 
  summarise(mean_tem=mean(mean_tem))
month_tem

ggplot(data = month_tem, aes(x=month, y=mean_tem))+
  geom_col()


## 2010-2020년 제주특별자치도 월별 평균기온 막대그래프 ##
jeju_month_tem <- weather %>%
  filter(area == "제주특별자치도") %>% 
  group_by(month) %>% 
  summarise(mean_tem = mean(mean_tem))
jeju_month_tem

ggplot(data = jeju_month_tem, aes(x=month, y=mean_tem))+
  geom_col()


## 전국 월별 최대기온 평균 ##
max_tem <- weather %>%
  group_by(month) %>% 
  summarise(max_tem = mean(max_tem))
max_tem

ggplotly(ggplot(data = max_tem, aes(x=month, y=max_tem))+
           geom_col())

## 전국 월별 최저기온 평균 ##
min_tem <- weather %>%
  group_by(month) %>% 
  summarise(min_tem = mean(min_tem))
min_tem

ggplot(data = min_tem, aes(x=month, y=min_tem))+
  geom_col()


## 2015-2020년 강원도 월별 최저기온 인터랙티브 점 그래프 ##
weather1 <- weather %>%
  filter(area == "강원도") %>% 
  filter(year %in% c("2015","2016","2017","2018","2019","2020"))

p1 <- ggplot(data = weather1, aes(x = year, y = min_tem, col = month)) + 
  geom_point()
ggplotly(p1)

## 전국 계절별 강수량 평균 ##
mean_prec <- weather %>%
  group_by(season) %>% 
  summarise(mean_prec = round(mean(prec),1)) 

  mean_prec
  
  ggplotly(ggplot(data = mean_prec1, aes(x=season, y=mean_prec))+
             geom_col(width=0.3,fill="red",colour="blue"))
  

## 2016-2020년 경상북도 계절별 강수량 인터랙티브 막대 그래프 ##
weather4 <- weather %>%
  filter(area == "경상북도") %>%
  group_by(season,year) %>% 
  summarise(mean_prec = round(mean(prec),1)) %>% 
  filter(year %in% c("2016","2017","2018","2019","2020"))

p4 <- ggplot(data = weather4, aes(x = year, y = mean_prec, col = season)) + 
  geom_bar(stat='identity',position = "dodge")
ggplotly(p4)


## 2020년 월별 평균강수량,평균기온 트리맵 ##
weather7 <- weather %>%
  filter(year == "2020") %>% 
  group_by(month) %>% 
  summarise(mean_prec = mean(prec), mean_tem = mean(mean_tem))

cor.test(weather7$mean_prec,weather7$mean_tem)

treemap(weather7,         
        index=c("month"),      
        vSize="mean_prec",  
        vColor="mean_tem",        
        type="value",      
        bg.labels="yellow",
        fontsize.labels=c(50, 1))


## 2016-2020년 계절별 평균기온 선그래프 ##

weather6 <- weather %>%
  group_by(season,year) %>% 
  summarise(mean_tem = round(mean(mean_tem),1)) %>% 
  filter(year %in% c("2016","2017","2018","2019","2020"))

ggplot(data = weather6, aes(x = season, y = mean_tem, group=year, col=year)) + 
  geom_line(size=1.5) + geom_point(size=3)


## 2019년과 2020년의 8월 기온 비교 ##
nine_temp<-weather %>% 
  select(mean_tem,year,month) %>% 
  filter(year %in% c("2019"),weather$month %in% c("08")) 
nine_temp
mean(nine_temp$mean_tem)

twen_temp<-weather %>% 
  select(mean_tem,year,month) %>% 
  filter(year %in% c("2020"),weather$month %in% c("08")) 
twen_temp
mean(twen_temp$mean_tem)

test<-data.frame(nine_temp$mean_tem,twen_temp$mean_tem)
t.test(test,var.equal = TRUE)
test1 <- data.frame(mean_tem = c(mean(nine_temp$mean_tem),mean(twen_temp$mean_tem)),
                    month = c("2019년 8월","2020년 8월"))
ggplotly(ggplot(data = test1, aes(x=month, y=mean_tem))+
           geom_col(width = 0.2,fill="red",colour="blue")+
           coord_cartesian(ylim = c(26,27)))



## 2020년 여름 최고기온 지도 ##
weather5 <- weather %>% 
  filter(year == "2020") %>% 
  filter(season == "여름") %>% 
  arrange(desc(max_tem))

ggChoropleth(data = weather5,      
             aes(fill = max_tem,       
                 map_id = code, 
                 tooltip = area),
             map = kormap1, 
             interactive = T,)

## 대한민국 폭염 워드 클라우드 ##
wordcount <- table(unlist(hot$locate))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

pal <- brewer.pal(8,"Paired")
wordcloud(words = df_word$word,  
          freq = df_word$freq,   
          min.freq = 3,         
          max.words = 200,     
          random.order = F,      
          rot.per = .1,         
          scale = c(7, 1.3),     
          colors = pal)


## 경상북도 폭염 워드 클라우드 ##
hot1 <- hot %>% 
  filter(locate == "경상북도")

wordcount1 <- table(unlist(hot1$area))
df_word1 <- as.data.frame(wordcount1, stringsAsFactors = F)
df_word1 <- rename(df_word1,
                   word = Var1,
                   freq = Freq)

pal1 <- brewer.pal(8,"Dark2")
wordcloud(words = df_word1$word,  
          freq = df_word1$freq,   
          min.freq = 1,         
          max.words = 100,     
          random.order = F,      
          rot.per = .1,         
          scale = c(5, 0.1),     
          colors = pal1)


## 2010-2020년 대구광역시,인천광역시 최고기온 인터랙티브 시계열 그래프 ##
weather2 <- weather %>%
  filter(area == "대구광역시") 
weather3 <- weather %>% 
  filter(area == "인천광역시")

p2 <- xts(weather2$max_tem, order.by = weather2$date)
p3 <- xts(weather3$max_tem, order.by = weather3$date)
p23 <- cbind(p2, p3)
colnames(p23) <- c("대구광역시", "인천광역시")
dygraph(p23)


## 대구광역시와 인천광역시의 최고기온의 상관분석 ##
incheon_temp<-weather %>% 
  select(max_tem,area) %>% 
  filter(area %in% c("인천광역시")) 
incheon_temp

daegue_temp<-weather %>% 
  select(max_tem,area) %>% 
  filter(area %in% c("대구광역시")) 
daegue_temp

cor.test(incheon_temp$max_tem,daegue_temp$max_tem)

test2 <- data.frame(mean_tem = c(mean(incheon_temp$max_tem),
                                 mean(daegue_temp$max_tem)),
                    area = c("인천","대구"))
ggplotly(ggplot(data = test2, aes(x=area, y=mean_tem),)+
           geom_col(width=0.3,fill="red",colour="blue"))
