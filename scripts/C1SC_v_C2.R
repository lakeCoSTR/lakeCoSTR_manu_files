# this script compares the C1 SC algorhim with the C2 surface temp data

#load libraries and functions from R_library.R
source('R_library.R')

#point to directories
C1_datadir <- '~/GitHub/ids-ne-lakes/data/colab-output/2021-06-08/'
C2_datadir <- '~/GitHub/ids-ne-lakes/data/colab-output/C2/'

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

# deming regression for C2
C2_deming = deming::deming(C2ST$surface_temp_median ~ C2ST$temp_med)
C2_deming_forresid = mcreg(x = C2ST$temp_med, y = C2ST$surface_temp_median, method.reg = 'Deming')
C2ST$opt_resid = MCResult.getResiduals(C2_deming_forresid)$optimized

#join C1 and C2 data
all_surface_temp <- full_join(C1SC, C2ST) %>% 
  mutate(collection = as.factor(collection))

# Plot comparison
ggplot(all_surface_temp, aes(x = temp_med, y = surface_temp_median, color = collection)) +
  geom_point(size = 3) +
  geom_line(aes(group = date)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  final_theme +
  coord_cartesian(xlim = c(0, 30),
                  ylim = c(0, 30)) +
  scale_color_colorblind()

#residuals

# by LS surface temp
ggplot(all_surface_temp, aes(x = surface_temp_median, y = opt_resid, color = collection)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-6, 6)) +
  final_theme +
  scale_color_colorblind()

# by month
ggplot(all_surface_temp, aes(x = month, y = opt_resid, color = collection)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-6, 6)) +
  final_theme +
  scale_color_colorblind()

# by doy
ggplot(all_surface_temp, aes(x = doy, y = opt_resid, color = collection)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-6, 6)) +
  final_theme +
  scale_color_colorblind()

# by cloud cover
ggplot(all_surface_temp, aes(x = cloud_cover, y = opt_resid, color = collection)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-6, 6)) +
  final_theme +
  scale_color_colorblind()

# by sun elevation
ggplot(all_surface_temp, aes(x = sunelev, y = opt_resid, color = collection)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-6, 6)) +
  final_theme +
  scale_color_colorblind()

# by earth sun distance
ggplot(all_surface_temp, aes(x = earthsun_d, y = opt_resid, color = collection)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-6, 6)) +
  final_theme +
  scale_color_colorblind()

# by azimuth
ggplot(all_surface_temp, aes(x = sunazi, y = opt_resid, color = collection)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-6, 6)) +
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

