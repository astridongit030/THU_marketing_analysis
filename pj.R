library(tidyverse)
library(lubridate)
library(tidytext)

#data mapping

##有没有yqj

yqj <- c(unique(teach_data_yqj_char_final$pid_teach))

data <- list()
for(id in teach_data_p_char_final$pid_teach){
  if(id %in% yqj){
    data[id] = 1
  }else{
    data[id] = 0
  }
}

teach_data_p_char_final<- teach_data_p_char_final %>%
  mutate(yqj_or_not = data)


teach_data_p_char_final$yqj_or_not <- as.numeric(teach_data_p_char_final$yqj_or_not)


## 每个项目募到多少钱 teach_data_final

money <- aggregate(x= teach_data_final$money,
          by= list(teach_data_final$pid_teach),
          FUN=sum)

teach_data_p_char_final<- teach_data_p_char_final %>%
  mutate(all_donation = money$x,
         completion_rate = all_donation/target)

## 每个yqj募到多少钱 teach_data_yqj_char_final

yqj_agg_money <- aggregate(x= teach_data_final$money,
          by= list(teach_data_final$yqjid_teach),
          FUN=sum)

yqj_agg_money <- yqj_agg_money[-1,]

teach_data_yqj_char_final<- teach_data_yqj_char_final %>%
  mutate(donation = yqj_agg_money$x)

## 每个专案里面有多少yqj的金额

yqj_res<- aggregate(x= teach_data_yqj_char_final$donation,
          by= list(pid_teach = teach_data_yqj_char_final$pid_teach),
          FUN=sum)

names(yqj_res)[names(yqj_res) =="x"] <- "yqj_donation"

df <- left_join(teach_data_p_char_final, yqj_res, by = "pid_teach")

df$yqj_donation[is.na(df$yqj_donation)] <- 0

##一个专案里面的总捐款次数

total_donation_count <- aggregate(x= teach_data_final$organic,
                    by= list(pid_teach = teach_data_final$pid_teach),
                    length)
names(total_donation_count)[names(total_donation_count) =="x"] <- "total_donation_count"
df <- left_join(df, total_donation_count, by = "pid_teach")

##一个专案里面的来自主页面的捐款次数

mainpage_donation_count <- aggregate(x= teach_data_final$organic,
                                  by= list(pid_teach = teach_data_final$pid_teach),
                                  sum)

names(mainpage_donation_count)[names(mainpage_donation_count) =="x"] <- "mainpage_donation_count"
df <- left_join(df, mainpage_donation_count, by = "pid_teach")


df <- df %>%
  mutate(
    yqj_donation_count = total_donation_count - mainpage_donation_count,
    not_yqj_donation = all_donation - yqj_donation,
    avg_yqj = yqj_donation/yqj_donation_count,
    avg_mainpage = not_yqj_donation/mainpage_donation_count,
    avg_total = all_donation/total_donation_count,
    yqj_percent = yqj_donation/all_donation
  )


#叙述统计 teach_data_p_char_final




##去除離群值(只捐一次的)
c <- aggregate(x= teach_data_final$pid_teach,
               by= list(count = teach_data_final$pid_teach),
               FUN=length)
once <- which(c$x == 1)

df2 <- df[-once,]


## avg_total

### NPO

df2$NPO <- as.character(df2$NPO)

df2 %>%
  ggplot(aes(x = NPO,y = avg_total)) + 
  geom_boxplot()


### yqj比例

df2 %>%
  ggplot(aes(x = yqj_percent,y = avg_total)) + 
  geom_point()


### cateName的分布

df %>%
  ggplot(aes(x = cateName,y = avg_total)) + 
  geom_boxplot()

### target level
#1200<=i<60000:1,   
#60000<=x<150000:2,  
#150000<=x<298000:3, 
#298000<=x<15760000:4

df2$target <- as.numeric(df2$target)
df2$target <- ifelse(df2$target<60000, 1, ifelse(df2$target<150000, 2, ifelse(df2$target<298000,3,4)))
df2$target <- as.factor(df2$target)

df2 %>%
  ggplot(aes(x = target,y = avg_total)) + 
  geom_boxplot()


### GDPlevel

fourth <- c("北京","上海","江苏","福建","浙江","湖北")
third <- c("重庆","内蒙古","山东","陕西","安徽","湖南","江西","辽宁")
sec <- c("山西","四川","海南","宁夏","新疆","河南","云南","西藏","全国","中西部地区")

df2$proj_province[df2$proj_province %in% fourth] <- 4
df2$proj_province[df2$proj_province %in% third] <- 3
df2$proj_province[df2$proj_province %in% sec] <- 2
df2$proj_province[!(df2$proj_province %in% c("4","3","2"))] <- 1

df2 %>%
  ggplot(aes(x = proj_province,y = avg_total)) + 
  geom_boxplot()


## completion rate

### NPO

df2 %>%
  ggplot(aes(x = NPO,y = completion_rate)) + 
  geom_boxplot()

### yqj比例

df2 %>%
  ggplot(aes(x = yqj_percent,y = completion_rate)) + 
  geom_point()

### cateName的分布

df2 %>%
  ggplot(aes(x = cateName,y = completion_rate)) + 
  geom_boxplot()

### GDPlevel

df2 %>%
  ggplot(aes(x = proj_province,y = completion_rate)) + 
  geom_boxplot()


#others
### yqj比例, cateName
df2 %>%
  ggplot(aes(x = cateName,y = yqj_percent)) + 
  geom_boxplot()

## completion rate 和 proj_province

df %>% 
  ggplot(aes(x = completion_rate,y = proj_province)) + 
  geom_boxplot()
