
#libraries and themes
library(tidyverse) #v 1.3.0
library(readxl) 
library(ggthemes)
library(reticulate)
library(mcr)
library(cowplot)

final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5))

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
