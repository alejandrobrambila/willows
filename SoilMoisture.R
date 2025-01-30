#Willow Soil Moisture
#Bar Graphs:
#Barberry-
  
ggplot(
  data= Barberry_Soil,
  mapping= aes(x=factor(PlotID,levels=c('521', '523', 'Uphill_P', '526', '530', 'Uphill_S', '533', '535', 'Uphill_L', '536', '538', 'Uphill_D', 'Fascine 1', 'Fascine 2', 'Uphill_F')), 
               y= WaterContent, fill= Species))+
  geom_bar(stat='identity')+
  facet_wrap(~factor(Month, levels=c('March', 'May', 'June', 'July')),nrow=4)+
  labs(x="PlotID")

#Underhill- 

ggplot(
  data= Underhill_Soil,
  mapping= aes(x=factor(PlotID, levels=c('511','515','Uphill P','516','518','Uphill S','501','502','Uphill L','506','510','Uphill D','Fascine 1','Fascine 2','Uphill F','Control 1','Control 2','Uphill C')),
              y= WaterContent, fill = Species))+
  geom_bar(stat='identity')+
  facet_wrap(~factor(Month, levels=c('March', 'May', 'June', 'July')),nrow=4)+
  labs(x="PlotID")+
  scale_fill_discrete(labels=c('Control', 'Fascine','Sericea','Purpurea','Discolor','Lucida'))

#GP1-
ggplot(
  data= GP1_Soil,
  mapping= aes(x=factor(PlotID, levels=c('548','550','Uphill P','557','559','Uphill S','551','554','Uphill L','543','545','Uphill D','Fascine 1','Fascine 2','Uphill F','Control 1','Control 2','Uphill C')),
               y= WaterContent, fill = Species))+
  geom_bar(stat='identity')+
  facet_wrap(~factor(Month, levels=c('March', 'May', 'June', 'July')),nrow=4)+
  labs(x="PlotID")+
  scale_fill_discrete(labels=c('Control','Discolor','Fascine','Lucida','Purpurea','Sericea'))
 
#GP2-
ggplot(
  data=GP2_Soil,
  mapping= aes(x= factor(PlotID,levels=c('573','574','Uphill P', '561','563','Uphill S','566','569','Uphill L','578','580','Uphill D','Fascine 1','Fascine 2','Uphill F')),
               y=WaterContent, fill=Species))+
  geom_bar(stat='identity')+
  facet_wrap(~factor(Month, levels=c('March', 'May', 'June', 'July')),nrow=4)+
  labs(x="PlotID")+
  scale_fill_discrete(labels=c('Discolor','Lucida','Purpurea','Sericea','Fascine'))
  

#Scatter- WC vs Month 
ggplot(
       data=AllSoilMoisture, aes(x= factor(Month,level= MonthOrder),y=WaterContentAvg, color=SectionType))+
       geom_jitter(width=.15)+
       facet_wrap(~Field)+
      labs(y="WaterContent%", x="Month")
 #include what normal levels should be in caption

#WC vs Date, up vs downhill
ggplot(
  data=filter(AllSoilMoisture,Far_Near=="NearDitch"), aes(x= factor(Month, level=MonthOrder),y=WaterContentAvg,color=Field))+
  geom_boxplot()+
  #facet_wrap(~Field)+
  labs (y="Water Content %", x="Date")+
  theme(axis.text.x = element_text(angle = 45))

#WC vs Month, control vs willows vs fascine
ggplot(
  data=AllSoilMoisture, aes(x= factor(Month,level=MonthOrder),y=WaterContentAvg,color=))+
  geom_boxplot()+
  labs (y="Water Content %", x="Month")+
  theme(axis.text.x = element_text(angle = 45))


AllSoilMoisture |> 
      mutate(Up_Downhill= ifelse(test= PlotID%in% c("Uphill_F", "Uphill_L","Uphill_D","Uphill_S","Uphill_C", "Uphill_P"), yes="Uphill", no="Downhill"),.before=1)

AllSoilMoisture<- read_csv("AllSoilMoisture.csv")

MonthOrder<- c("March","May","June","July","August","September","October")

DateOrder<- c("3/19/2024","3/20/2024","5/7/2024", "5/10/2024" , "5/13/2024","6/19/2024","7/22/2024","7/23/2024","8/21/2024","9/13/2024","9/26/2024","9/27/2024","10/9/2024")
