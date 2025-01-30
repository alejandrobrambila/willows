#Willow Survival Rates
#> SurvivalRates<- read_csv("SurvivalRates.csv")

ggplot(
  data= SurvivalRates,
  mapping= aes(x=Species, y= PercentSurvived, color= Tube))+
  geom_jitter(width=.15)+
  labs(y="Percent Survived", color="Legend")

  