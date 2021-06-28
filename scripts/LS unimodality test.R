# this file loads in the histograms from the colab output; creates histos, and then assesses unimodality

library(tidyverse)

final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5))

#point to data directory
datadir = 'data/colab-output/2021-06-08/'

#load file
temp_histo <- read.csv(file.path(datadir, 'sunapee_histograms_0p1bin.csv'))

#transform and rename
temp_histo <- temp_histo %>% 
  gather(date, count, -bin) %>% 
  mutate(date = substr(date, 2, 9),
         date = as.Date(date, format = '%Y%m%d'),
         bin = as.numeric(bin),
         count = as.numeric(count)) %>% 
  rename(temp_degC = bin)


#total number of pixels in lake sunapee
sun_n_pixels <- 23640
image_pixels <- temp_histo %>% 
  group_by(date) %>% 
  summarise(tot_pix = sum(count)) %>% 
  mutate(perc_lake = round((tot_pix/sun_n_pixels)*100, digits = 2))

#filter for counts that aren't null
temp_histo_all <- temp_histo %>% 
  filter(count > 0)

write.csv(temp_histo_all, file.path(datadir, 'sunapee_histograms_0p1bin_reformatted.csv'))

#make a list of all dates
datelist <- unique(temp_histo_all$date)
date_modal <- as.data.frame(datelist) %>% 
  rename(date = datelist) %>% 
  mutate(unimodal = '')



#determine unimodality from density plot, if one derivative change, unimodal if more it's not.
for(j in 1:length(datelist)) {
  temp_histo_date <- temp_histo_all %>% 
    filter(date == datelist[j])
  temp_histo_date$diff <- NA_real_
  temp_histo_date$simple_change <- ''
  for(i in 2:nrow(temp_histo_date)){
      temp_histo_date$diff[i] = temp_histo_date$count[i] - temp_histo_date$count[i-1]
  }
      temp_histo_date <- temp_histo_date %>% 
        filter(abs(diff) > 0.01*sun_n_pixels) #difference has to be more than 1% of pixels for a meaningful multimodality
      if (nrow(temp_histo_date)>2) {
        for(k in 2:nrow(temp_histo_date)){
          temp_histo_date$simple_change[k] = case_when(temp_histo_date$diff[k] >0 & temp_histo_date$diff[k-1]< 0 ~ 'y',
                                                   temp_histo_date$diff[k] <0 & temp_histo_date$diff[k-1]> 0 ~ 'y',
                                                   TRUE~ NA_character_)
        }
        n_change <- nrow(temp_histo_date %>% 
                           filter(simple_change == 'y'))
        date_modal$unimodal[j] = case_when(n_change > 1 ~ 'not unimodal',
                                     TRUE ~ 'unimodal')
      } else{
        date_modal$unimodal[j] = 'unk'
      }
     
}

full_join(date_modal, image_pixels) %>% 
  write.csv(., file.path(datadir, 'date_modalanalysis.csv'), row.names = F)

histo_unique_data <- full_join(temp_histo_all, image_pixels) %>% 
  full_join(., date_modal)

#set up pdf device
pdf(file.path(datadir, 'histograms.pdf'),width=11,height=8.5)

#print PDF of histograms
for(i in 1:length(datelist)){
  a <- histo_unique_data %>% 
    filter(date == datelist[i])
  hist <- ggplot(a, aes(x = temp_degC, y = count)) +
    geom_col(width = 0.4) +
    final_theme +
    labs(title = paste0(datelist[i]),
         x = 'LS-derived skin temperature (deg C)',
         y = 'count') +
    annotate('text', label = paste0(a$perc_lake[1], ' percent of lake in LS scene'), x = min(a$temp_degC), y = max(a$count), hjust = 0)+
    annotate('text', label = paste0(a$unimodal[1]), x = min(a$temp_degC), y = max(a$count)*.9, hjust = 0)
  print(hist)
}

#Close pdf graphics device
dev.off()
