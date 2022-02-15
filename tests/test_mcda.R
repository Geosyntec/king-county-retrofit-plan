source(here::here('R','promethee_2.R'))
library(tidyverse)
#test using original Bertand Data Set

Evaluations = data.frame(
  name = c("Tourism B", "luxury 1", "Tourism A", "Luxury 2", "Economic", "Sport"),
  Price = c(25500,38000,26000,35000,15000,29000),
  Power = c(85,90,75,85,50,110),
  Consumption = c(7,8.5,8,9,7.5,9),
  Habitability = c(4,4,3,5,2,1),
  Comfort = c(3,5,3,4,1,2)
)


minmax = c(
  "min",
  "max",
  "min",
  "min",
  "max"
  # ,
  # "min",
  # "max"
)
weight =
 c(2,
           2,
           3,
   1,
   1

   )

aaa<-promethee_2(dataset = Evaluations,name_col = "name",weighting = weight,minmax = minmax)
aaa
outrankingflows <- aaa[[2]]

sum(outrankingflows$outrankingFlowsPos)
sum(outrankingflows$outrankingFlowsNeg)

#
#
# # The evaluation table
# dataset = Evaluations
# name_col = "name"
# performanceTable <- Evaluations %>% select(-name) %>% as.matrix()
# rownames(performanceTable) <- Evaluations$name
# colnames(performanceTable) <- Evaluations %>% select(-name)%>% colnames()
#
# # The preference functions
# preferenceFunction<-c(rep('Usual',ncol(performanceTable)))
#
# #Preference threshold
# preferenceThreshold<-c(rep(0,ncol(performanceTable)))
# names(preferenceThreshold)<-colnames(performanceTable)
#
# #Indifference threshold
# indifferenceThreshold<-c(rep(0,ncol(performanceTable)))
# names(indifferenceThreshold)<-colnames(performanceTable)
#
# #Parameter of the Gaussian preference function
# gaussParameter<-c(rep(0,ncol(performanceTable)))
# names(gaussParameter)<-colnames(performanceTable)
#
# #weights
#
# criteriaWeights<-weight
# names(criteriaWeights)<-colnames(performanceTable)
#
# # criteria to minimize or maximize
#
# criteriaMinMax<-minmax
# names(criteriaMinMax)<-colnames(performanceTable)
#
#
# # Outranking flows
#
# outrankingFlows<-PROMETHEEOutrankingFlows(performanceTable, preferenceFunction,
#                                           preferenceThreshold,indifferenceThreshold,
#                                           gaussParameter,criteriaWeights,
#                                           criteriaMinMax)
#
