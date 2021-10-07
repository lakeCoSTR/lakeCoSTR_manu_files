# this script compares the C1 SC algorithm with the C2 surface temp data

#load libraries and functions from R_library.R
source('R_library.R')

#point to directories
C1_datadir <- '~/GitHub/ids-ne-lakes/data/colab-output/2021-06-08/'
C2_datadir <- '~/GitHub/ids-ne-lakes/data/colab-output/C2/'
fig_dir <- '~/GitHub/ids-ne-lakes/figures/'

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
  select(date, LSmission, month, doy, temp_avg:loon_std)

# deming regression for C1
C1_deming = deming::deming(C1SC$surface_temp_median ~ C1SC$temp_med)
C1_deming_forresid = mcreg(x = C1SC$temp_med, y = C1SC$surface_temp_median, method.reg = 'Deming')
C1SC$opt_resid = MCResult.getResiduals(C1_deming_forresid)$optimized


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

#save temp paired data
write_csv(C2ST, file.path(datadir, 'temporary_sunapee_stats_paired_C2.csv'))

# deming regression for C2
C2_deming = deming::deming(C2ST$surface_temp_median ~ C2ST$temp_med)
C2_deming_forresid = mcreg(x = C2ST$temp_med, y = C2ST$surface_temp_median, method.reg = 'Deming')
C2ST$opt_resid = MCResult.getResiduals(C2_deming_forresid)$optimized

#join C1 and C2 data
all_surface_temp <- full_join(C1SC, C2ST) %>% 
  mutate(collection = as.factor(collection))

# Plot comparison
dem_regression_both <- ggplot(all_surface_temp, aes(x = temp_med, y = surface_temp_median, color = collection)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', size = 0.75) +
  geom_point() +
  #add 1:1 line
  #add deming regression and prediction intervals for C2
  geom_abline(intercept = C2_deming$coefficients[1], slope = C2_deming$coefficients[2], color = '#E69F00', size = 0.75) +
  geom_abline(intercept = C2_deming$ci[1,1], slope = C2_deming$ci[2,1], color = '#E69F00', linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_deming$ci[1,2], slope = C2_deming$ci[2,2], color = '#E69F00', linetype = 3, size = 0.75) +
  #add deming regression and prediction intervals for C1
  geom_abline(intercept = C1_deming$coefficients[1], slope = C1_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C1_deming$ci[1,1], slope = C1_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C1_deming$ci[1,2], slope = C1_deming$ci[2,2], linetype = 3, size = 0.75) +
  labs(x = NULL,
       y = 'median Landsat-derived\nsurface temperature (deg C)') +
  final_theme +
  theme(legend.position = 'bottom') +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27)) +
  scale_color_colorblind()
dem_regression_both

#grab legend from this plot
legend = get_legend(dem_regression_both)

#remove legend for viz
dem_regression_both <- dem_regression_both +
  theme(legend.position = 'none')
dem_regression_both

#residuals

# by LS surface temp
resid_by_insitu <- ggplot(all_surface_temp, aes(x = temp_med, y = opt_resid, color = collection)) +
  geom_point() +
  labs(x = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
        y = 'Deming-optimized residual\n(deg C)') +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-6, 6),
                  xlim = c(0, 27)) +
  final_theme +
  scale_color_colorblind() +
  theme(legend.position = 'none')

plots <- plot_grid(dem_regression_both, resid_by_insitu,
          ncol = 1,
          labels = c('a', 'b'),
          rel_heights = c(4.5, 5))
plots_leg <- plot_grid(plots, legend, 
                       ncol = 1,
                       rel_heights = c(7, 0.25)) 
plots_leg

plot_filename <- paste0(fig_dir, 'C1 and C2 deming and residuals v', Sys.Date(), '.jpg')

ggsave(plot_filename, 
       dpi = 300,
       height = 8,
       width = 4,
       units = 'in')

# by doy
ggplot(all_surface_temp, aes(x = as.numeric(doy), y = opt_resid, color = collection)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  labs(x = 'day of year',
       y = 'Deming-optimized residual\n(deg C)') +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-6, 6)) +
  final_theme +
  scale_color_colorblind()

# by pixel count

# by cloud cover
ggplot(all_surface_temp, aes(x = cloud_cover, y = opt_resid, color = collection)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-6, 6)) +
  labs(x = 'cloud cover (percent)',
       y = 'Deming-optimized residual\n(deg C)') +
  final_theme +
  scale_color_colorblind()

# by sun elevation
ggplot(all_surface_temp, aes(x = sunelev, y = opt_resid, color = collection)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-6, 6)) +
  labs(x = 'sun elevation (UNITS)',
       y = 'Deming-optimized residual\n(deg C)') +
  final_theme +
  scale_color_colorblind()

# by earth sun distance
ggplot(all_surface_temp, aes(x = earthsun_d, y = opt_resid, color = collection)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-6, 6)) +
  labs(x = 'earth sun distance (UNITS)',
       y = 'Deming-optimized residual\n(deg C)') +
  final_theme +
  scale_color_colorblind()

# by azimuth
ggplot(all_surface_temp, aes(x = sunazi, y = opt_resid, color = collection)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-6, 6)) +
  labs(x = 'sun azimuth (degrees)',
       y = 'Deming-optimized residual\n(deg C)') +
  final_theme +
  scale_color_colorblind()

# grab residuals and compare against each other
residuals_only <- all_surface_temp %>% 
  select(date, month, LSmission, collection, opt_resid) %>% 
  mutate(collection = case_when(collection == 1 ~ 'C1',
                                collection == 2 ~ 'C2',
                                TRUE ~ NA_character_)) %>% 
  pivot_wider(names_from = collection, values_from = opt_resid)

ggplot(residuals_only, aes(x = C1, y = C2)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian(xlim = c(-6, 3),
                  ylim = c(-6, 3)) +
  final_theme

