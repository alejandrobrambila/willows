#Willow Survival Rates
library(tidyverse)

SurvivalRates<- read_csv("SurvivalRates3.csv")%>%
  mutate(season=paste(Season, Year, sep="_"))%>%
  mutate(season=factor(season, levels = c("Fall_2023","Spring_2024","Summer_2024","Fall_2024","Spring_2025")))%>%
  mutate(percent=Count/FirstCount*100)


ggplot(
  data= SurvivalRates,
  mapping= aes(x=season, y= ((Count)), color= Species, shape=Tube))+
  geom_jitter(width=.15) +
  labs(y="Survivors", color="Legend")+ facet_wrap(~PlantingType)


ggplot(
  data= SurvivalRates,
  mapping= aes(x=season, y= ((percent)), color= Species, shape=Field))+
  geom_jitter(width=.15) +
  labs(y="% Survivors", color="Legend")+ facet_wrap(~PlantingType)


ggplot(
  data= SurvivalRates,
  mapping= aes(x=season, y= (1/(Count/80)), color= Species, shape=Tube))+
  geom_jitter(width=.15)+scale_y_log10()+
  labs(y="Row Spacing (in ft)", color="Legend")+ facet_wrap(~PlantingType)

ggplot(
  data= SurvivalRates,
  mapping= aes(x=season, y= (1/(Count/80)), color= Species, shape=Field))+
  geom_jitter(width=.15)+scale_y_log10()+
  labs(y="Row Spacing (in ft)", color="Legend")+ facet_wrap(~PlantingType)
