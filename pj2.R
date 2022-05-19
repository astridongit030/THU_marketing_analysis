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
table(teach_data_p_char_final$yqj_or_not)

## all_donation 每个项目募到多少钱 teach_data_final

money <- aggregate(x= teach_data_final$money,
          by= list(teach_data_final$pid_teach),
          FUN=sum)

## yqj_donation 每个专案里面有多少yqj的金额

yqj_res<- aggregate(x= teach_data_yqj_char_final$donation,
                    by= list(pid_teach = teach_data_yqj_char_final$pid_teach),
                    FUN=sum)

names(yqj_res)[names(yqj_res) =="x"] <- "yqj_donation"

df <- left_join(teach_data_p_char_final, yqj_res, by = "pid_teach")

df$yqj_donation[is.na(df$yqj_donation)] <- 0

## not_yqj_donation 每个专案里面有多少不是yqj的金额

df <- df %>% mutate(
  not_yqj_donation = all_donation - yqj_donation
)


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


## 每个yqj募到多少钱

yqj_agg_money <- aggregate(x= teach_data_final$money,
          by= list(teach_data_final$yqjid_teach),
          FUN=sum)

yqj_agg_money <- yqj_agg_money[-1,]



##create dataframe

df <- df %>%
  mutate(
    all_donation = money$x
    yqj_donation
    yqj_donation_count = total_donation_count - mainpage_donation_count,
    not_yqj_donation = all_donation - yqj_donation,
    avg_yqj = yqj_donation/yqj_donation_count,
    avg_mainpage = not_yqj_donation/mainpage_donation_count
  )

teach_data_p_char_final<- teach_data_p_char_final %>%
  mutate(,
         completion_rate = all_donation/target)

teach_data_yqj_char_final<- teach_data_yqj_char_final %>%
  mutate(donation = yqj_agg_money$x)



#setwd('C:/Users/asus/Desktop/code/THU/marketing_analytics')
#write.csv(df, file = 'df.csv', fileEncoding = "gb2312")

table(teach_data_p_char_final$cateName)


#叙述统计 teach_data_p_char_final

## NPO

#NPO数量
table(teach_data_p_char_final$NPO)
### 未添加标记

teach_data_p_char_final$NPO <- as.character(teach_data_p_char_final$NPO)
teach_data_p_char_final$target <- as.numeric(teach_data_p_char_final$target)
teach_data_p_char_final <- teach_data_p_char_final %>%
  mutate(target_thousand = target/1000)

teach_data_p_char_final %>%
  ggplot() + 
  geom_bar(aes(x=NPO)) + 
  labs(x = "NPO=1为NPO发起，其余为0", y="count")

#以NPO分组，target分布

teach_data_p_char_final %>%
  ggplot(aes(x = NPO, y = target_thousand)) +
  geom_point(aes(color = NPO))

teach_data_p_char_final %>%
  group_by(NPO) %>%
  summarise(total = sum(target_thousand),
            max = max(target_thousand),
            min = min(target_thousand))

#NPO的募款目的集中于哪些cateName

teach_data_p_char_final %>%
  ggplot() +
  geom_bar(mapping = aes(x = NPO, color = cateName),position = "dodge")

##cateName 募款目的

#件数
table(teach_data_p_char_final$cateName)

teach_data_p_char_final %>%
  ggplot() + 
  geom_bar(aes(x=cateName))

#以cateName分组，募款金额统计(eg平均数)

teach_data_p_char_final %>%
  group_by(cateName) %>%
  summarise(total = sum(target_thousand),
            max = max(target_thousand),
            min = min(target_thousand))

##proj_province

#件数

df_province <- teach_data_p_char_final %>% 
  count(proj_province, sort = TRUE)

df_province %>%
  ggplot() + 
  geom_bar(aes(x=reorder(proj_province,n), y=n), stat = "identity") + 
  coord_flip()

teach_data_p_char_final$pid_teach <- as.numeric(teach_data_p_char_final$pid_teach)
teach_data_yqj_char_final$pid_teach <- as.numeric(teach_data_yqj_char_final$pid_teach)

#以省份分组，cateName统计

#以省份分组，objTagName统计
#以省份分组，target统计

##objTagName

#件数
#以objTagName分组(>100)，省份统计

##cateTagName

#件数

##以cateName分組的總人均捐款次數、yqj人均捐款次數、not_yqj人均捐款次數



