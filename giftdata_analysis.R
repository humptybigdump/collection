
#install.packages("readxl")

library("readxl")
data <- read_excel('/Gift-Data.xlsx')

attach(data)


#Build graph with ggplot2 -
#Example:
# http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization#line-plot-with-multiple-groups
#install.packages("ggplot2")
library("ggplot2")

# draw average productivity for both teams over the 4 weeks


#install.packages("dplyr")

library(dplyr)
dataavg <- data %>%
  group_by(Team, Week) %>% 
  summarise_at(vars("ComputersAssembled"), mean)


ggplot(dataavg, aes(x=Week, y=ComputersAssembled, group=Team)) +
  geom_line(aes(color=Team))+
  geom_point(aes(color=Team))


#c) Run regressions to identify the main drivers of the number of computers assembled in the first week. 
#Also run a regression to compare the productivity of both teams in the first week. Briefly discuss the findings.

firstweek <- lm(ComputersAssembled ~ Age + SeasonsWorkedBefore + TimeWorkedThisSeason + AvgComplexityOfCustomDesign, data=subset(data, Week==1))
summary(firstweek)

firstweekteam <- lm(ComputersAssembled ~ Age + SeasonsWorkedBefore + TimeWorkedThisSeason + AvgComplexityOfCustomDesign + Team, data=subset(data, Week==1))
summary(firstweekteam)

# d) Run another regression to compare the productivity of both teams focussing on the week of the gift. Briefly discuss the findings.
giftweek <- lm(ComputersAssembled ~ Age + SeasonsWorkedBefore + TimeWorkedThisSeason + AvgComplexityOfCustomDesign + Team, data=subset(data, Week==2))
summary(giftweek)


# e) Run another regression to compare the productivity of both teams, while taking into account all four weeks. Use the coefficients resulting from your regression to compute the effects the gift
# had on the members of ‘team 1’. Compute the productivity foregone by not paying the extra
# payment.

week2 <- lm(ComputersAssembled ~ Age + SeasonsWorkedBefore + TimeWorkedThisSeason + AvgComplexityOfCustomDesign + Team, data=subset(data, Week==2))
summary(week2)
# 15.9

week3 <- lm(ComputersAssembled ~ Age + SeasonsWorkedBefore + TimeWorkedThisSeason + AvgComplexityOfCustomDesign + Team, data=subset(data, Week==3))
summary(week3)
# 10.8

week4 <- lm(ComputersAssembled ~ Age + SeasonsWorkedBefore + TimeWorkedThisSeason + AvgComplexityOfCustomDesign + Team, data=subset(data, Week==4))
summary(week4)
# 4.4

print(15.9+10.8+4.4)
