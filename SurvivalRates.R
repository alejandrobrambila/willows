#Willow Survival Rates
library(tidyverse)

SurvivalRates<- read_csv("SurvivalRates.csv")%>%
  mutate(season=paste(Season, Year, sep="_"))%>%
  mutate(season=factor(season, levels = c("Fall_2023","Spring_2024","Summer_2024","Fall_2024","Spring_2025")))


ggplot(
  data= SurvivalRates,
  mapping= aes(x=season, y= ((Count)), color= Species, shape=Tube))+
  geom_jitter(width=.15) +
  labs(y="Survivors", color="Legend")+ facet_wrap(~PlantingType)

ß

ggplot(
  data= SurvivalRates,
  mapping= aes(x=season, y= (1/(Count/80)), color= Species, shape=Tube))+
  geom_jitter(width=.15)+scale_y_log10()+
  labs(y="Row Spacing (in ft)", color="Legend")+ facet_wrap(~PlantingType)
