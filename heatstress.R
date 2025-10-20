s24<-read_csv('sum2024.csv', skip=6)%>%
  select(1, 14, 20,23)
names(s24)<-c("date", "tempF", "dewF", "relhum")
library(lubridate)
s24a<-s24%>%
  mutate(tempC=(tempF-32)*5/9, dewC=(dewF-32)*5/9)%>%
  mutate(THI=tempC+(.36*dewC)+41.2)%>%
  mutate(THI2=tempF-.55*((1-relhum/100)*(tempF-58)))%>%
  mutate(date=as.character(date))%>%
  mutate(date=mdy_hm(date))%>%
  mutate(mdyr=paste(lubridate::month(date), lubridate::mday(date), lubridate::year(date), sep="/"))%>%
  mutate(mdyr=mdy(mdyr))

s24sum<-s24a%>%
  group_by(mdyr)%>%
  summarize(meanHI=mean(THI2), maxHI=max(THI2))%>%
  mutate(cat=ifelse(maxHI>89, "emergency", ifelse(maxHI>79, "danger", ifelse(maxHI>72, "alert", "none") )))

ggplot(s24a, aes(x=mdyr, y=THI2))+geom_point(size=.25)+geom_hline(yintercept=72, color="yellow")+
  geom_hline(yintercept=79, color="orange")+ geom_hline(yintercept=89, color="red")+
 # geom_point(data=s24sum, aes(x=mdyr, y=meanHI))+ 
  geom_point(data=s24sum, aes(x=mdyr, y=maxHI, color=cat))+
 scale_color_manual(values=c("yellow", "orange", "red", "black"))+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_x_date(date_breaks="15 day")
  
  #geom_point(aes(x=date, y=THI), size=.25, color="blue")
