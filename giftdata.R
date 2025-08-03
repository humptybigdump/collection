
install.packages("readxl")

library("readxl")
data <- read_excel('yourpath/Gift-Data.xlsx')

attach(data)


#Build graph with ggplot2 -
#Example: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization#line-plot-with-multiple-groups
install.packages("ggplot2")
library("ggplot2")

