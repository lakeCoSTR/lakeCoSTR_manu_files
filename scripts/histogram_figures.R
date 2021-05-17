library(tidyverse)
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5))


temp_histo <- read.csv('C:/Users/steeleb/Downloads/histograms.csv')

temp_histo <- temp_histo %>% 
  gather(date, area, -bin) %>% 
  mutate(date = substr(date, 2, 9),
         date = as.Date(date, format = '%Y%m%d'),
         bin = as.numeric(bin),
         area = as.numeric(area)) %>% 
  rename(temp_degC = bin)

datelist <- unique(temp_histo$date)

max(temp_histo$area,na.rm = T)

temp_histo_totals <- temp_histo %>% 
  group_by(date) %>% 
  summarise(total_area = sum(area, na.rm = T)) %>% 
  mutate(max = max(total_area, na.rm = T)) %>% 
  mutate(percent_area = round((total_area/max)*100, digits = 2))

temp_histo <- temp_histo_totals %>% 
  select(date, percent_area) %>% 
  full_join(temp_histo, .)

pdf(file='Sunapee Temperature Histograms.pdf',width=11,height=8.5)

#print PDF of histograms
for(i in 1:length(datelist)){
  a <- temp_histo %>% 
    filter(date == datelist[i])
  hist <- ggplot(a, aes(x = temp_degC, y = area)) +
    geom_line() +
    final_theme +
    labs(title = paste0(datelist[i]),
         x = 'LS-derived skin temperature (deg C)',
         y = 'total area of lake (ha)') +
    annotate('text', label = paste0(a$percent_area[1], ' percent of lake in LS scene'), x = 1, y = max(a$area), hjust = 0)
  print(hist)
}

#Close pdf graphics device
dev.off()
