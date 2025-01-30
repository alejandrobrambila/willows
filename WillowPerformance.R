#Willow Performance-height and rodent damage 

#height vs month vs species 
ggplot(
  data=filter(WillowPerformance,Species %in% c("lucida","sericea","discolor","purpurea")),
  aes(x=factor(Month,level=MonthOrder),y=Height, color=Species))+
  geom_boxplot()+
  labs(x="Month")


#height vs planting style vs species
ggplot(
  data=filter(WillowPerformance,Species %in% c("lucida","sericea","discolor","purpurea")),
  aes(x=Species,y=FinalHeight, color=PlantingType))+
  geom_boxplot()+
  labs(y="Height")

 
#rodent damage vs month vs field
ggplot(
  data=filter(WillowPerformance,Species %in% c("lucida","sericea","discolor","purpurea")), 
  aes(x=Field,fill=RodentDamage))+
  facet_wrap("Month")+
  geom_histogram(stat="count")


