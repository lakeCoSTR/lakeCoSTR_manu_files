# this script creates Figure C, which compares deming regression and residuals by x using varying QAQC filters

# last modified 2021-10-08
# written by B. Steele 

# Set up Workspace ####

# read in libraries and functions
source('scripts/R_library.R')

# point to directories
datadir = '~/GitHub/ids-ne-lakes/data/'
C1_datadir <- '~/GitHub/ids-ne-lakes/data/colab-output/2021-06-08/'
C2_datadir <- '~/GitHub/ids-ne-lakes/data/colab-output/C2/'
fig_dir <- '~/GitHub/ids-ne-lakes/figures/'

# Read in Data ####

# read in C1 single-channel and C2 surface temperature data
C1SC <- read_csv(file.path(C1_datadir, 'sunapee_insitu_landsat_paired.csv')) %>% 
  mutate(date = as.Date(substrRight(`system:index`, 8), format = '%Y%m%d')) %>% 
  mutate(LSmission = case_when(grepl('LT05', `system:index`) ~ 'LS 5',
                               grepl('LT04', `system:index`) ~ 'LS 4',
                               grepl('LE07', `system:index`) ~ 'LS 7',
                               grepl('LC08', `system:index`) ~ 'LS 8',
                               TRUE ~ NA_character_)) %>% 
  mutate(collection = 1,
         month = format(date, '%m'),
         doy = format(date, '%j'))
head(C1SC)

# rename LS-related columns
C1SC <- C1SC %>% 
  rename(earthsun_d = esd,
         sunazi = azimuth,
         sunelev = elev)

#DELETE AFTER C2 PAIR DATA ARRIVES #
#break out into temp data for join with C2
C1SC_temp <- C1SC %>% 
  select(date, LSmission, month, doy, temp_avg:StateBeach_count)

# REPLACE WITH PAIRED DATA #
C2ST <- read_csv(file.path(C2_datadir, 'sunapee_1980_2020_C2_stats.csv')) %>% 
  mutate(date = as.Date(substrRight(`system:index`, 8), format = '%Y%m%d')) %>% 
  mutate(LSmission = case_when(grepl('LT05', `system:index`) ~ 'LS 5',
                               grepl('LT04', `system:index`) ~ 'LS 4',
                               grepl('LE07', `system:index`) ~ 'LS 7',
                               grepl('LC08', `system:index`) ~ 'LS 8',
                               TRUE ~ NA_character_)) %>% 
  rename_at(vars('temp_max', 'temp_mean', 'temp_median', 'temp_min', 'temp_p25', 'temp_p75', 'temp_skew', 'temp_stdDev'),
            ~ paste('surface', ., sep = '_')) %>% 
  mutate(collection = 2,
         month = format(date, '%m'),
         doy = format(date, '%j'))
head(C2ST)

# DELETE ONCE PAIRED DATA ARE HERE #
C2ST <- C1SC_temp %>% 
  left_join(., C2ST) %>% 
  filter(!is.na(pixel_count)) 
head(C2ST)


#load in all high-frequency insitu data for historical data analysis
insitu <- read.csv(paste0(datadir, 'insitu_temp_data_v2021-05-17.csv')) %>% 
  filter(!is.na(lat_dd)) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='Etc/GMT+5'))

# Summarize in-situ to define spread and IQR limits ####

# get range per day during hours of flyover
range_per_date <- insitu %>% 
  mutate(hour = as.numeric(format(datetime, '%H')),
         date = as.Date(datetime)) %>% 
  filter(hour >= 9 & hour < 11) %>% 
  group_by(date) %>% 
  summarize(temp_range = max(temp_degC) - min(temp_degC),
            IQR = IQR(temp_degC, na.rm = T),
            n_locs = length(unique(location)))

#save max observed spread and IQR for further filtering
max_spread <- max(range_per_date$temp_range, na.rm = T)
max_IQR <- max(range_per_date$IQR, na.rm = T)

# Flag C2 data for freezing temps, spread P/F, IQR P/F ####
C2ST <- C2ST %>% 
  mutate(freeze_QAQC = case_when(surface_temp_min < 0 ~ 'F', #pass/fail for freezing temps
                                 TRUE ~ 'P')) %>% 
  # left_join(., modal_analysis) %>% #join with modal analysis
  # mutate(unimodal_QAQC = case_when(unimodal == 'unimodal' ~ 'P',
  #                                  TRUE ~ 'F')) %>%  # pass/fail for unimodality
  mutate(temp_spread = round(surface_temp_max, digits = 1) - round(surface_temp_min, digits = 1)) %>% #calc temp spread; pass/fail for temp spread
  mutate(spread_QAQC = case_when(temp_spread < max_spread*1.1 ~ 'P',
                                 TRUE ~ 'F')) %>% 
  mutate(IQR = round(surface_temp_p75, digits = 1) - round(surface_temp_p25, digits = 1))  %>%  #calc IQR; pass/fail for temp IQR
  mutate(IQR_QAQC = case_when(IQR < max_IQR*1.1 ~ 'P',
                              TRUE ~ 'F'))
write.csv(C2ST, file.path(C2_datadir,'temporary_sunapee_paired_C2_QAQCflag_v11Oct2011.csv'), row.names = F)
                       
C2ST_freeze <- C2ST %>% 
  filter(freeze_QAQC == 'P')

C2ST_freeze %>% 
  group_by(LSmission) %>% 
  summarize(count = length(LSmission))

C2ST_maxrange <- C2ST %>% 
  filter(freeze_QAQC == 'P' & 
           spread_QAQC == 'P')

C2ST_maxrange %>% 
  group_by(LSmission) %>% 
  summarize(count = length(LSmission))

C2ST_maxIQR <- C2ST %>% 
  filter(freeze_QAQC == 'P' & 
           IQR_QAQC == 'P')

C2ST_maxIQR %>% 
  group_by(LSmission) %>% 
  summarize(count = length(LSmission))

C2ST_cloud <- C2ST %>% 
  filter(freeze_QAQC == 'P' & cloud_cover <40)

C2ST_cloud %>% 
  group_by(LSmission) %>% 
  summarize(count = length(LSmission))


# Deming Regressions ####

# deming regression for C1
C1_deming = deming::deming(C1SC$surface_temp_median ~ C1SC$temp_med)
C1_deming_forresid = mcreg(x = C1SC$temp_med, y = C1SC$surface_temp_median, method.reg = 'Deming')
C1SC$opt_resid = MCResult.getResiduals(C1_deming_forresid)$optimized
C1SC$filter = 'none'

# deming regression for C2
C2_deming = deming::deming(C2ST$surface_temp_median ~ C2ST$temp_med)
C2_deming_forresid = mcreg(x = C2ST$temp_med, y = C2ST$surface_temp_median, method.reg = 'Deming')
C2ST$opt_resid = MCResult.getResiduals(C2_deming_forresid)$optimized
C2ST$filter = 'none'

# deming regression for C2 without freezing temps
C2_freeze_deming = deming::deming(C2ST_freeze$surface_temp_median ~ C2ST_freeze$temp_med)
C2_freeze_deming_forresid = mcreg(x = C2ST_freeze$temp_med, 
                           y = C2ST_freeze$surface_temp_median, 
                           method.reg = 'Deming')
C2ST_freeze$opt_resid = MCResult.getResiduals(C2_freeze_deming_forresid)$optimized
C2ST_freeze$filter = 'min below zero'

# deming regression for C2 without freezing temps and within range
C2_maxrange_deming = deming::deming(C2ST_maxrange$surface_temp_median ~ C2ST_maxrange$temp_med)
C2_maxrange_deming_forresid = mcreg(x = C2ST_maxrange$temp_med, 
                                  y = C2ST_maxrange$surface_temp_median, 
                                  method.reg = 'Deming')
C2ST_maxrange$opt_resid = MCResult.getResiduals(C2_maxrange_deming_forresid)$optimized
C2ST_maxrange$filter = '110% in-situ range'

# deming regression for C2 without freezing temps and within IQR
C2_maxIQR_deming = deming::deming(C2ST_maxIQR$surface_temp_median ~ C2ST_maxIQR$temp_med)
C2_maxIQR_deming_forresid = mcreg(x = C2ST_maxIQR$temp_med, 
                                  y = C2ST_maxIQR$surface_temp_median, 
                                  method.reg = 'Deming')
C2ST_maxIQR$opt_resid = MCResult.getResiduals(C2_maxIQR_deming_forresid)$optimized
C2ST_maxIQR$filter = '110% in-situ IQR'

# deming regression for C2 without freezing temps and within IQR and data from the buoy only
C2_cloud_deming = deming::deming(C2ST_cloud$surface_temp_median ~ 
                                        C2ST_cloud$temp_med)
C2_cloud_deming_forresid = mcreg(x = C2ST_cloud$temp_med, 
                                  y = C2ST_cloud$surface_temp_median, 
                                  method.reg = 'Deming')
C2ST_cloud$opt_resid = MCResult.getResiduals(C2_cloud_deming_forresid)$optimized
C2ST_cloud$filter = 'freeze and 40% cc'


C1_deming
cor(C1SC$surface_temp_median, C1SC$temp_med)

C2_deming
cor(C2ST$surface_temp_median, C2ST$temp_med)

C2_freeze_deming
cor(C2ST_freeze$surface_temp_median, C2ST_freeze$temp_med)

C2_maxrange_deming
cor(C2ST_maxrange$surface_temp_median, C2ST_maxrange$temp_med)

C2_maxIQR_deming
cor(C2ST_maxIQR$surface_temp_median, C2ST_maxIQR$temp_med)

C2_cloud_deming
cor(C2ST_cloud$surface_temp_median, C2ST_cloud$temp_med)

# Join all Deming data ####
all_surface_temp <- full_join(C1SC, C2ST) %>% 
  full_join(., C2ST_freeze) %>% 
  full_join(., C2ST_maxrange) %>% 
  full_join(., C2ST_maxIQR) %>% 
  full_join(., C2ST_cloud) %>% 
  mutate(collection = as.factor(collection),
         filter = as.factor(filter))

# Plot Deming regression for all filters ####
FigC_a <- ggplot(C1SC, aes(x = temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C1
  geom_abline(intercept = C1_deming$coefficients[1], slope = C1_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C1_deming$ci[1,1], slope = C1_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C1_deming$ci[1,2], slope = C1_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(cor(C1SC$surface_temp_median, C1SC$temp_med), digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('n = ', length(C1SC$date)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = '',
       y = 'median Landsat-derived\nsurface temperature (deg C)',
       title = 'Collection 1',
       subtitle = '') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigC_a

FigC_b <- ggplot(C2ST, aes(x = temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2
  geom_abline(intercept = C2_deming$coefficients[1], slope = C2_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_deming$ci[1,1], slope = C2_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_deming$ci[1,2], slope = C2_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(cor(C2ST$surface_temp_median, C2ST$temp_med), digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('n = ', length(C2ST$date)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = '',
       y = '\n',
       title = 'Collection 2',
       subtitle = '') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigC_b

FigC_c <- ggplot(C2ST_freeze, aes(x = temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2 filtered for freezing mins
  geom_abline(intercept = C2_freeze_deming$coefficients[1], slope = C2_freeze_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_freeze_deming$ci[1,1], slope = C2_freeze_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_freeze_deming$ci[1,2], slope = C2_freeze_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(cor(C2ST_freeze$surface_temp_median, C2ST_freeze$temp_med), digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('n = ', length(C2ST_freeze$date)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = '',
       y = '\n',
       title = 'Collection 2',
       subtitle = 'filtered for sub-zero temperatures') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigC_c

FigC_f <- ggplot(C2ST_maxrange, aes(x = temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2 filtered for sub zero and maxrange
  geom_abline(intercept = C2_maxrange_deming$coefficients[1], slope = C2_maxrange_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_maxrange_deming$ci[1,1], slope = C2_maxrange_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_maxrange_deming$ci[1,2], slope = C2_maxrange_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(cor(C2ST_maxrange$surface_temp_median, C2ST_maxrange$temp_med), digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('n = ', length(C2ST_maxrange$date)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
       y = '/n',
       title = 'Collection 2',
       subtitle = 'filtered for sub-zero and in-situ range') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigC_f

FigC_d <- ggplot(C2ST_maxIQR, aes(x = temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2 filtered for sub zero and maxIQR
  geom_abline(intercept = C2_maxIQR_deming$coefficients[1], slope = C2_maxIQR_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_maxIQR_deming$ci[1,1], slope = C2_maxIQR_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_maxIQR_deming$ci[1,2], slope = C2_maxIQR_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(cor(C2ST_maxIQR$surface_temp_median, C2ST_maxIQR$temp_med), digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('n = ', length(C2ST_maxIQR$date)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
       y = 'median Landsat-derived\nsurface temperature (deg C)',
       title = 'Collection 2',
       subtitle = 'filtered for sub-zero and in-situ IQR') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigC_d

FigC_e <- ggplot(C2ST_cloud, aes(x = temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2 filtered for sub zero and cloud
  geom_abline(intercept = C2_cloud_deming$coefficients[1], slope = C2_cloud_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_cloud_deming$ci[1,1], slope = C2_cloud_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_cloud_deming$ci[1,2], slope = C2_cloud_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(cor(C2ST_cloud$surface_temp_median, C2ST_cloud$temp_med), digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('n = ', length(C2ST_cloud$date)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
       y = '\n',
       title = 'Collection 2',
       subtitle = 'filtered for sub-zero and cloud cover') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigC_e


# # Plot residuals for all filters ####
# 
# FigC_f <- ggplot(C1SC, aes(x = temp_med, y = opt_resid)) +
#   geom_point() +
#   labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
#        y = 'Deming-optimized residual\n(deg C)') +
#   geom_abline(intercept =  0, slope = 0) +
#   coord_cartesian(ylim = c(-6, 6),
#                   xlim = c(0, 27)) +
#   final_theme
# FigC_f
# 
# FigC_g <- ggplot(C2ST, aes(x = temp_med, y = opt_resid)) +
#   geom_point() +
#  labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
#        y = '') +
#   geom_abline(intercept =  0, slope = 0) +
#   coord_cartesian(ylim = c(-6, 6),
#                   xlim = c(0, 27)) +
#   final_theme
# FigC_g
# 
# FigC_h <- ggplot(C2ST_freeze, aes(x = temp_med, y = opt_resid)) +
#   geom_point() +
#   labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
#        y = '') +
#   geom_abline(intercept =  0, slope = 0) +
#   coord_cartesian(ylim = c(-6, 6),
#                   xlim = c(0, 27)) +
#   final_theme
# FigC_h
# 
# FigC_i <- ggplot(C2ST_maxrange, aes(x = temp_med, y = opt_resid)) +
#   geom_point() +
#   labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
#        y = '') +
#   geom_abline(intercept =  0, slope = 0) +
#   coord_cartesian(ylim = c(-6, 6),
#                   xlim = c(0, 27)) +
#   final_theme
# FigC_i
# 
# FigC_j <- ggplot(C2ST_maxIQR, aes(x = temp_med, y = opt_resid)) +
#   geom_point() +
#   labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
#        y = '') +
#   geom_abline(intercept =  0, slope = 0) +
#   coord_cartesian(ylim = c(-6, 6),
#                   xlim = c(0, 27)) +
#   final_theme
# FigC_j


FigC_plot <- plot_grid(FigC_a, FigC_b, FigC_c, FigC_d, FigC_e,
                   FigC_f,
                   ncol = 3,
                   labels = c('a', 'b', 'c', 'd', 'e', 'f'),
                   label_x = 0.15)

FigC_plot

ggsave(file.path(fig_dir, 'FigureC_deming_filters.jpg'), 
       dpi = 300,
       height = 9,
       width = 12,
       units = 'in')


# Plot slope and intercept with 95%ci ####
slope_int_table <- NULL

slope_int_table$model = c('C1SC', 'C2ST', 'C2ST_freeze', 'C2ST_maxrange', 'C2ST_maxIQR', 'C2ST_cloud')
slope_int_table$slope = c(C1_deming$coefficients[2],
                          C2_deming$coefficients[2],
                          C2_freeze_deming$coefficients[2],
                          C2_maxrange_deming$coefficients[2],
                          C2_maxIQR_deming$coefficients[2],
                          C2_cloud_deming$coefficients[2])
slope_int_table$intercept = c(C1_deming$coefficients[1],
                              C2_deming$coefficients[1],
                              C2_freeze_deming$coefficients[1],
                              C2_maxrange_deming$coefficients[1],
                              C2_maxIQR_deming$coefficients[1],
                              C2_cloud_deming$coefficients[1])
slope_int_table$slope_se = c(C1_deming$se[2],
                             C2_deming$se[2],
                             C2_freeze_deming$se[2],
                             C2_maxrange_deming$se[2],
                             C2_maxIQR_deming$se[2],
                             C2_cloud_deming$se[2])
slope_int_table$int_se = c(C1_deming$se[1],
                             C2_deming$se[1],
                             C2_freeze_deming$se[1],
                             C2_maxrange_deming$se[1],
                             C2_maxIQR_deming$se[1],
                             C2_cloud_deming$se[1])
slope_int_table <- as.data.frame(slope_int_table)

slope_int_table <- slope_int_table %>% 
  pivot_longer(cols = c(slope, intercept), names_to = 'var', values_to = 'value') %>% 
  mutate(se = case_when(var == 'slope' ~ slope_se,
                        var == 'intercept' ~ int_se,
                        TRUE ~ NA_real_),
         u95 = value + se,
         l95 = value - se) %>% 
  select(-slope_se, -int_se) %>% 
  mutate(model = factor(model, 
                        levels = c('C1SC', 'C2ST', 'C2ST_freeze','C2ST_maxIQR', 'C2ST_cloud', 'C2ST_maxrange'),
                        labels = c('Collection 1', 'Collection 2', 'Collection 2\nsub-zero', 'Collection 2\nin-situ IQR', 'Collection 2\ncloud','Collection 2\nin-situ range'))) %>% 
  mutate(regression = 'Deming')

slope_fig <- slope_int_table %>% 
  filter(var == 'slope') %>% 
  ggplot(., aes(x = model, y = value)) +
  geom_point() +
  geom_pointrange(aes(ymin = l95, ymax = u95))+
  labs(x = NULL,
       y = 'model slope') +
  coord_cartesian(ylim = c(0.85, 1.15)) +
  final_theme
int_fig <- slope_int_table %>% 
  filter(var == 'intercept') %>% 
  ggplot(., aes(x = model, y = value)) +
  geom_point() +
  geom_pointrange(aes(ymin = l95, ymax = u95))+
  labs(x = NULL,
       y = 'model intercept') +
  coord_cartesian(ylim = c(-3.2, 0)) +
  final_theme

slope_int_fig <- plot_grid(slope_fig, int_fig,
                           ncol = 1,
                           labels = c('a', 'b'))
slope_int_fig

ggsave(file.path(fig_dir, 'FigureF_slopeintercept.jpg'), 
       dpi = 300,
       height = 6,
       width = 8,
       units = 'in')



# Look at C2 IQR data set residuals against other variables for SF A####

# by insitu temp
istemp <- ggplot(C2ST_cloud, aes(x = temp_med, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  labs(x = 'in-situ median water temp\n(deg C)',
       y = '\n') +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  final_theme

# by doy
doy <- ggplot(C2ST_cloud, aes(x = as.numeric(doy), y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  labs(x = 'day of year\n',
       y = '\n') +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  final_theme

# by pixel count
perclake <- ggplot(C2ST_cloud, aes(x = (pixel_count/17134)*100, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  labs(x = 'percent of lake\n',
       y = '\n') +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  final_theme

# by cloud cover
cloud <- ggplot(C2ST_cloud, aes(x = cloud_cover, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'cloud cover (percent)\n',
       y = '\n') +
  final_theme 

# by sun elevation
sunelev <- ggplot(C2ST_cloud, aes(x = sunelev, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'sun elevation (UNITS)\n',
       y = 'Deming-optimized residual\n(deg C)') +
  final_theme

# by earth sun distance
esd <- ggplot(C2ST_cloud, aes(x = earthsun_d, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'earth sun distance (UNITS)\n',
       y = '\n') +
  final_theme 

# by azimuth
azi <- ggplot(C2ST_cloud, aes(x = sunazi, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'sun azimuth (degrees)\n',
       y = '\n') +
  final_theme 

# depth of sensor
depth <- ggplot(C2ST_cloud, aes(x = depth_avg, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'average depth of sensor (m)\n',
       y = '\n') +
  final_theme 

# std dev in-situ temp
sd <- ggplot(C2ST_cloud, aes(x = t_stdev, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'standard deviation of temps\ncontributing to median (deg C)',
       y = '\n') +
  final_theme 

#in-situ count
count <- ggplot(C2ST_cloud, aes(x = insitu_count, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'number of insitu measurements\ncontributing to median',
       y = '\n') +
  final_theme 

plot_grid(istemp, doy, perclake, cloud, sunelev, esd, azi, depth, sd, count,
          labels = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'),
          ncol = 2,
          label_x = 0.1,
          label_y = 1.1)

ggsave(file.path(fig_dir, 'SFA_resid_summary_C2cloud_v14Oct2021.jpg'), 
       height = 12, width = 8, units = 'in', dpi = 300)



# Calculate error for various models ####
alldata_error <- all_surface_temp %>% 
  group_by(collection, filter) %>% 
  summarise(mse = round(sum((opt_resid^2))/n(), digits = 2),
            rmse = round(sqrt(mse), digits = 2),
            bias = round(sum(opt_resid)/n(), digits = 2),
            mae = round(sum(abs(opt_resid))/n(), digits = 2),
            n_obs = n())  %>% 
  mutate(month = 'All data',
         LSmission = 'All missions') %>% 
  pivot_longer(cols = c(mse:n_obs), names_to = 'variable', values_to = 'values') %>% 
  mutate(variable = factor(variable, levels = c('n_obs', 'mae', 'rmse', 'mse', 'bias'))) %>% 
  arrange(month, variable) %>% 
  pivot_wider(names_from = c(LSmission),
              values_from = values) 

mission_error <- all_surface_temp %>% 
  group_by(collection, LSmission, filter) %>% 
  summarise(mse = round(sum((opt_resid^2))/n(), digits = 2),
            rmse = round(sqrt(mse), digits = 2),
            bias = round(sum(opt_resid)/n(), digits = 2),
            mae = round(sum(abs(opt_resid))/n(), digits = 2),
            n_obs = n())  %>% 
  mutate(month = 'All data') %>% 
  pivot_longer(cols = c(mse:n_obs), names_to = 'variable', values_to = 'values') %>% 
  mutate(variable = factor(variable, levels = c('n_obs', 'mae', 'rmse', 'mse', 'bias'))) %>% 
  arrange(month, variable) %>% 
  pivot_wider(names_from = c(LSmission),
              values_from = values) 

alldata_error <- full_join(alldata_error, mission_error)
alldata_error

month_mission_error <- all_surface_temp %>% 
  group_by(collection, month, LSmission, filter) %>% 
  summarise(mse = round(sum((opt_resid^2))/n(), digits = 2),
            rmse = round(sqrt(mse), digits = 2),
            bias = round(sum(opt_resid)/n(), digits = 2),
            mae = round(sum(abs(opt_resid))/n(), digits = 2),
            n_obs = n())  %>% 
  select(month, collection, LSmission, filter,n_obs, mae, rmse, bias) %>% 
  pivot_longer(!c(month, LSmission, collection, filter), names_to = 'variable', values_to = 'values') %>% 
  mutate(variable = factor(variable, levels = c('n_obs', 'mae', 'rmse', 'mse', 'bias'))) %>% 
  arrange(month, variable) %>% 
  pivot_wider(names_from = c(LSmission),
              values_from = values)

month_error <- all_surface_temp %>% 
  group_by(collection, month, filter) %>% 
  summarise(mse = round(sum((opt_resid^2))/n(), digits = 2),
            rmse = round(sqrt(mse), digits = 2),
            bias = round(sum(opt_resid)/n(), digits = 2),
            mae = round(sum(abs(opt_resid))/n(), digits = 2),
            n_obs = n())  %>% 
  mutate(LSmission = 'All missions') %>% 
  select(month, collection, LSmission, filter, n_obs, mae, rmse, bias) %>% 
  pivot_longer(!c(month, LSmission, filter, collection), names_to = 'variable', values_to = 'values') %>% 
  mutate(variable = factor(variable, levels = c('n_obs', 'mae', 'rmse', 'mse', 'bias'))) %>% 
  arrange(month, variable) %>% 
  pivot_wider(names_from = c(LSmission),
              values_from = values)

month_error <- full_join(month_mission_error, month_error)

alldata_error <- full_join(alldata_error, month_error)

write.csv(alldata_error, file.path(C2_datadir, 'LS_deming_predictionerror_C1C2_stats_v15Oct2021.csv'))





