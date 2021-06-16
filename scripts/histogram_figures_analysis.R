# this file loads in the histograms from the colab output; creates histos, and then assesses unimodality

library(tidyverse)
library(diptest)
library(multimode)
library(parameters)

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
  mutate(perc_lake = tot_pix/sun_n_pixels)

#filter for counts that aren't null
temp_histo_all <- temp_histo %>% 
  filter(count > 0)
#make a list of all dates
datelist <- unique(temp_histo_all$date)
date_modal <- as.data.frame(datelist) %>% 
  mutate(dip_statistic = NA_real_,
         dip_pval = NA_real_,
         acr_statistic = NA_real_,
         acr_pval = NA_real_) %>% 
  rename(date = datelist)


for(j in 1:length(datelist)) {
  temp_histo_date <- temp_histo_all %>% 
    filter(date == datelist[j])
  for(i in 1:nrow(temp_histo_date)){
    if (i == 1) {
      temp_degC <- NULL
      temp_degC <- rep(temp_histo_date[i,1], temp_histo_date$count[i])
    } else {
      tmp <- NULL
      tmp <- rep(temp_histo_date[i,1], temp_histo_date$count[i])
      temp_degC = c(temp_degC,tmp)
    }
  }
  dip <- modetest(temp_degC, method = 'HH')
  date_modal$dip_statistic[j] = dip$statistic
  date_modal$dip_pval[j] = dip$p.value
  acr <- modetest(temp_degC, method = 'ACR')
  date_modal$acr_statistic[j] = acr$statistic
  date_modal$acr_pval[j] = acr$p.value
}

write.csv(date_modal, file.path(datadir, 'date_modalanalysis.csv'), row.names = F)

ggplot(date_modal, aes(x = date, y = dip_pval)) +
  geom_hline(yintercept = 0.05) +
  geom_point() +
  geom_point(aes(x=date, y= acr_pval)) +
  final_theme

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
    annotate('text', label = paste0(a$acr_pval[1], ' ACR p-value'), x = min(a$temp_degC), y = max(a$count)*.9, hjust = 0) +
    annotate('text', label = paste0(a$dip_pval[1], ' HH p-value'), x = min(a$temp_degC), y = max(a$count)*.8, hjust = 0) 
  print(hist)
}

#Close pdf graphics device
dev.off()
