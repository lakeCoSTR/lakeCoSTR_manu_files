#this script plays with some other insitu filters to look at robustness for validation

# last modified 2021-10-11
# written by B. Steele 

# Set up Workspace ####

# read in libraries and functions
source('scripts/R_library.R')

# point to directories
C2_datadir <- '~/GitHub/ids-ne-lakes/data/colab-output/C2/'
fig_dir <- '~/GitHub/ids-ne-lakes/figures/'

# Read in Data ####
C2_data <- read.csv(file.path(C2_datadir, 'sunapee_paired_C2_QAQCflag_v2021-11-09.csv'))

# filter for subsets 
C2_buoy <- C2_data %>% 
  filter(freeze_QAQC == 'P' & cloud_cover < 40) %>% 
  select(date, LSmission, loon_median, surface_temp_median) %>% 
  filter(!is.na(loon_median))

C2_buoy_2y <- C2_data %>% 
  filter(freeze_QAQC == 'P' & cloud_cover <40) %>% 
  select(date, LSmission, loon_median, surface_temp_median) %>% 
  filter(date >= '2019-01-01')%>% 
  filter(!is.na(loon_median))

C2_2018 <- C2_data %>% 
  filter(freeze_QAQC == 'P'& cloud_cover <40) %>% 
  filter(date >= '2018-01-01' & date < '2019-01-01')

C2_HCS <- C2_data %>% 
  filter(freeze_QAQC == 'P' & cloud_cover <40) %>% 
  select(date, LSmission, HerrickCoveSouth_median, surface_temp_median) %>% 
  filter(!is.na(HerrickCoveSouth_median))

C2_NB <- C2_data %>% 
  filter(freeze_QAQC == 'P'& cloud_cover <40) %>% 
  select(date, LSmission, Newbury_median, surface_temp_median) %>% 
  filter(!is.na(Newbury_median))


# Calculate Deming regression and statistics ####
#just buoy data
C2_buoy_deming = deming::deming(C2_buoy$surface_temp_median ~ C2_buoy$loon_median)
C2_buoy_deming
buoy_slope <- as.numeric(C2_buoy_deming$coefficients[2])
buoy_r <- cor(C2_buoy$loon_median, C2_buoy$surface_temp_median)
C2_buoy_deming_forresid = mcreg(x = C2_buoy$loon_median, y = C2_buoy$surface_temp_median, method.reg = 'Deming')
C2_buoy_deming$opt_resid = MCResult.getResiduals(C2_buoy_deming_forresid)$optimized

#2 y of buoy data
C2_2ybuoy_deming = deming::deming(C2_buoy_2y$surface_temp_median ~ C2_buoy_2y$loon_median)
C2_2ybuoy_deming
buoy2y_slope <- as.numeric(C2_2ybuoy_deming$coefficients[2])
buoy2y_r <- cor(C2_buoy_2y$surface_temp_median, C2_buoy_2y$loon_median)
C2_2ybuoy_deming_forresid = mcreg(x = C2_buoy_2y$loon_median, y = C2_buoy_2y$surface_temp_median, method.reg = 'Deming')
C2_buoy_2y$opt_resid = MCResult.getResiduals(C2_2ybuoy_deming_forresid)$optimized

#2018 only
C2_2018_deming = deming::deming(C2_2018$surface_temp_median ~ C2_2018$temp_med)
C2_2018_deming
all2018_slope <- as.numeric(C2_2018_deming$coefficients[2])
all2018_r <- cor(C2_2018$surface_temp_median, C2_2018$temp_med)
C2_2018_deming_forresid = mcreg(x = C2_2018$temp_med, y = C2_2018$surface_temp_median, method.reg = 'Deming')
C2_2018$opt_resid = MCResult.getResiduals(C2_2018_deming_forresid)$optimized

#Herrick Cove South only
C2_HCS_deming = deming::deming(C2_HCS$surface_temp_median ~ C2_HCS$HerrickCoveSouth_median)
C2_HCS_deming
HCS_slope <- as.numeric(C2_HCS_deming$coefficients[2])
HCS_r <- cor(C2_HCS$surface_temp_median, C2_HCS$HerrickCoveSouth_median)
C2_HCS_deming_forresid = mcreg(x = C2_HCS$HerrickCoveSouth_median, y = C2_HCS$surface_temp_median, method.reg = 'Deming')
C2_HCS$opt_resid = MCResult.getResiduals(C2_HCS_deming_forresid)$optimized

#Newbury only
C2_NB_deming = deming::deming(C2_NB$surface_temp_median ~ C2_NB$Newbury_median)
C2_NB_deming
NB_slope <- as.numeric(C2_NB_deming$coefficients[2])
NB_r <- cor(C2_NB$surface_temp_median, C2_NB$Newbury_median)
C2_NB_deming_forresid = mcreg(x = C2_NB$Newbury_median, y = C2_NB$surface_temp_median, method.reg = 'Deming')
C2_NB$opt_resid = MCResult.getResiduals(C2_NB_deming_forresid)$optimized

FigD_a <- ggplot(C2_buoy, aes(x = loon_median, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2 filtered for sub zero and maxIQR
  geom_abline(intercept = C2_buoy_deming$coefficients[1], slope = C2_buoy_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_buoy_deming$ci[1,1], slope = C2_buoy_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_buoy_deming$ci[1,2], slope = C2_buoy_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(buoy_r, digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('slope = ', round(buoy_slope, digits = 2)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
       y = 'median Landsat-derived\nsurface temperature (deg C)',
       title = 'Collection 2',
       subtitle = '`buoy` dataset') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigD_a

FigD_b <- ggplot(C2_buoy_2y, aes(x = loon_median, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2 filtered for sub zero and maxIQR
  geom_abline(intercept = C2_2ybuoy_deming$coefficients[1], slope = C2_2ybuoy_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_2ybuoy_deming$ci[1,1], slope = C2_2ybuoy_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_2ybuoy_deming$ci[1,2], slope = C2_2ybuoy_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(buoy2y_r, digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('slope = ', round(buoy2y_slope, digits = 2)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
       y = 'median Landsat-derived\nsurface temperature (deg C)',
       title = 'Collection 2',
       subtitle = '`buoy 2y` dataset') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigD_b

FigD_c <- ggplot(C2_2018, aes(x = temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2 filtered for sub zero and maxIQR
  geom_abline(intercept = C2_2018_deming$coefficients[1], slope = C2_2018_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_2018_deming$ci[1,1], slope = C2_2018_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_2018_deming$ci[1,2], slope = C2_2018_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(all2018_r, digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('slope = ', round(all2018_slope, digits = 2)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
       y = 'median Landsat-derived\nsurface temperature (deg C)',
       title = 'Collection 2',
       subtitle = '`2018` dataset') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigD_c

FigD_d <- ggplot(C2_HCS, aes(x = HerrickCoveSouth_median, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2 filtered for sub zero and maxIQR
  geom_abline(intercept = C2_HCS_deming$coefficients[1], slope = C2_HCS_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_HCS_deming$ci[1,1], slope = C2_HCS_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_HCS_deming$ci[1,2], slope = C2_HCS_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(HCS_r, digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('slope = ', round(HCS_slope, digits = 2)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
       y = 'median Landsat-derived\nsurface temperature (deg C)',
       title = 'Collection 2',
       subtitle = '`HCS` dataset') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigD_d

FigD_e <- ggplot(C2_NB, aes(x = Newbury_median, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2 filtered for sub zero and maxIQR
  geom_abline(intercept = C2_NB_deming$coefficients[1], slope = C2_NB_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_NB_deming$ci[1,1], slope = C2_NB_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_NB_deming$ci[1,2], slope = C2_NB_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(NB_r, digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('slope = ', round(NB_slope, digits = 2)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
       y = 'median Landsat-derived\nsurface temperature (deg C)',
       title = 'Collection 2',
       subtitle = '`NB` dataset') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigD_e

plot_grid(FigD_a, FigD_b, FigD_c, FigD_d, FigD_e,
          labels = c('a', 'b', 'c', 'd', 'e'))

ggsave(file.path(fig_dir, 'SFC_C2ValidationExample_v09Nov2021.jpg'), height = 8, width = 12, units = 'in', dpi = 300)
