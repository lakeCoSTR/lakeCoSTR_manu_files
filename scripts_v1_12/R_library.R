
#libraries and themes
library(tidyverse) #v 1.3.0
# library(readxl) 
library(ggthemes)
library(mcr)
library(cowplot)
library(car)
# library(Metrics)
library(deming)

final_theme=theme_bw() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"),
        plot.title=element_text(size=12, face='bold', hjust=0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5))

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
