# Seeing if I can figure out an easy way to automate matching closest time stamps 
# from verified NOAA tides to time stamps for min/max depth measurements

library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(broom)
library(lubridate)
# Matching closest time values is going to rely on data.table package
library(data.table)

################################################
#
# Merging Raw Depth data, GPS lat/long, AND
#
# Tide Corrections for Min/Max Depth Data - For Coding Cafe 5/21/2025
#
################################################

setwd("K:/kelp/bull_kelp_kayak/2024/data_processing/min_max_depth")


####################
# Start by pulling in all the raw GPS data and join together
####################

# Grab all gps txt files, add file name as a column
gpsdata <- list.files(path = "./spatial_data/GPS_raw", pattern = '.txt', all.files = FALSE, full.names = TRUE, 
                      recursive = FALSE, ignore.case = FALSE)

read_gpsdata_filename <- function(filename){
  ret <- read.csv(filename, skip = 1, header=F,
                  col.names = c('type','ident','Latitude','Longitude','x1', 'x2', 
                                'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9', 'x10', 'x11',
                                'time', 'wpt_class', 'x12', 'x13', 'x14', 'x15', 'x16', 
                                'x17', 'x18', 'x19', 'x20', 'x21', 'x22',
                                'x23', 'x24', 'x25', 'x26', 'x27', 'magvar',
                                'x28', 'x29', 'x30', 'x31', 'x32', 'x33','x34',
                                'x35', 'x36', 'x37'))
  ret$source <- filename #EDIT
  ret
}

gps <- plyr::ldply(gpsdata, read_gpsdata_filename)

#clean up gps file, remove unnecessary columns, split file name to get site and gps unit fields 
gps_clean <- gps %>%
  select(-type, -x1, -x2, -x3, -x4, -x5, -x6, -x7, -x8, -x9, -x10, -x11, -x12, -x13,
         -x14, -x15, -x16, -x17, -x18, -x19, -x20, -x21, -x22, -x23, -x24, -x25, -x26, -x27,
         -magvar, -x28, -x29, -x30, -x31, -x32, -x33, -x34, -x35, -x36, -x37)%>%
  separate(source, into = c('y', 'x','x1','site_unit_date_wp_csv'), sep = '/')%>%
  # What we need from the site_unit_date_wp_csv field is the site name, gps unit and date 
  # this can probably be done more streamlined with some stringr nonsense
  # but dealing with strings is a pain in the butt and we just need to see
  # if this whole thing will even work before we streamline the code
  separate(site_unit_date_wp_csv, into = c('site','gpsunit','date', 'wp', 'csv'))%>%
  # For some reason not specifying a 'sep' in above separate drops the '.csv' 
  # automatically. Don't worry about warning message... for now...
  select(-time, -y, -x, -x1, -wp, -csv)

#create date/time field
latlong_2024 <- gps_clean %>%
  mutate(gps_pt = as.factor(ident))%>%
  separate(wpt_class, into = c('cal', 'clockplus'), sep = ' ')%>%
  separate(clockplus, into = c('clock','plus'), sep = '\\+')%>%
  mutate(clock = ymd_hms(paste(cal, clock)) - hours(7), dt_gps=round_date(clock, unit="minute"))%>%  #note that lubridate has functionality for this in the general as.POSIXct where tz= will AUTOMATICALLY do the time change conversions for you, this works hard coded only because data are only from PST
  select(-plus, -cal, -clock, -ident, -date)

write.csv(latlong_2024, './outputs/lat_long_2024.csv')

#################
# Next, load raw min/max depth data entered from field datasheets
#################

#load field data 
minmax_raw_2024_all <- read.csv('./2024_min_max_depth_raw.csv') 

#filter absent transects to create absent data frame to add back in after tide correction 
minmax_absent_2024 <- minmax_raw_2024_all %>%
  filter(minmax == 'absent')

#remove absent data,create date/time field, and assign primary and secondary tide stations to each site
minmax_raw_2024 <- minmax_raw_2024_all %>%
  filter(!minmax == 'absent')%>%
  mutate(gps_pt = as.factor(gps_pt)) %>%
  mutate(timetxt = str_pad(.$timetxt, 4, pad = "0"))%>% #don't forget me when I'm gone nice useful function
  mutate(dt = ymd_hm(paste(datetxt, timetxt, sep = ' ')),
         #gps_pt = as.numeric(gps_pt),
         pr_tide = if_else(site == 'sqx' | site == 'brs' | site == 'dvl' | site == 'fox' 
                           | site == 'day' | site == 'sal' | site == 'owb','tacoma',
                           if_else(site == 'nob' | site == 'bkt', 'porttownsend',
                           if_else(site=='fwb', 'portangeles',
                           if_else(site=='bur' | site == 'cau', 'fridayharbor',
                          if_else(site == 'chp', 'cherrypoint',
                           'seattle'))))),
         sec_tide = if_else(site == 'sqx', 'dofflemeyer',
                            if_else(site == 'sal' | site == 'day' | site=="fox", 'narrows',
                            if_else(site == 'owb', 'tahlequah',
                            if_else(site == 'lip' | site == 'vhe', 'pointvashon',
                            if_else(site == 'han', 'hansville',
                            if_else(site == 'edm', 'edmonds',
                            if_else(site == 'fwb', 'crescentbay',
                            if_else(site == 'bur', 'burrowsbay_allanis',
                            if_else(site == 'bkt', 'gardiner',
                                    pr_tide))))))))))%>%
  select(-timetxt, -datetxt)

#########
#Use min/max depth data to create start and stop survey date/times to target secondary tide info

survey_windows_2024 <- minmax_raw_2024 %>%
  #min/max date-times
  filter(!notes == 'absent')%>%
  select(site,sec_tide, dt)%>%
  separate(dt, into = c('cal','clock'), sep = " ")%>%
  separate(clock, into = c('h','m','s'), sep = ':')%>%
  mutate(cal = ymd(cal),
         timetxt = paste(h,m, sep = ""),
         timetxt = as.numeric(timetxt))%>%
  na.omit()%>%
  group_by(site,sec_tide, cal)%>%
  mutate(survey_start = min(timetxt),
         survey_finish = max(timetxt))%>%
  ungroup()%>%
  select(-h,-m,-s, -timetxt)%>%
  unique()

#write.csv(survey_windows_2024, './outputs/survey_windows_2024.csv')


#########
# Now organize all the tide station info
#########


# Grab all the NOAA verified tide data (primary tides)

verified_tidenames <- list.files(path = "./ver_tides/", pattern = '.csv', all.files = FALSE, full.names = TRUE, 
                                 recursive = FALSE, ignore.case = FALSE)

read_verified_tidenames_filename <- function(filename){
  ret <- read.csv(filename, skip = 1, header=F,
                  col.names = c('cal','clock','actual_pred','junk','actual_ver'))
  ret$source <- filename #EDIT
  ret
}

verified_tides <- plyr::ldply(verified_tidenames, read_verified_tidenames_filename)


# Grab all the secondary tide data 

secondary_tidenames <- list.files(path = "./sec_tides/", pattern = '.csv', all.files = FALSE, full.names = TRUE, 
                                  recursive = FALSE, ignore.case = FALSE)

read_secondary_tidenames_filename <- function(filename){
  ret <- read.csv(filename, header=F,
                  col.names = c('cal','clock', 'tz', 'pred_tide'))
  ret$source <- filename #EDIT
  ret
}

secondary_tides <- plyr::ldply(secondary_tidenames, read_secondary_tidenames_filename)


# Tidy up our primary and secondary tides and min/max data with everything we will need for our join

verified_tides_clean <- verified_tides %>%
  select(-junk)%>%
  separate(source, into = c('x','x1','date_site'), sep = '/')%>%
  # All we need from the date_site field is the tide station name 
  # this can probably be done more streamlined with some stringr nonsense
  # but dealing with strings is a pain in the butt and we just need to see
  # if this whole thing will even work before we streamline the code
  separate(date_site, into = c('x2','x3','tide_site'))%>%
  # For some reason not specifying a 'sep' in above separate drops the '.csv' 
  # automatically. Don't worry about warning message... for now...
  select(-x, -x1, -x2, -x3)%>%
  mutate(dt = mdy_hm(paste(cal,clock, sep = ' ')),
         act_pred_disc_m = actual_ver - actual_pred)


secondary_tides_clean <- secondary_tides %>%
  separate(source, into= c('x','x1','sec_tide'), sep = '/')%>%
  select(-x, -x1, -tz)%>%
  mutate(sec_tide = str_sub(sec_tide,1,nchar(sec_tide)-4),
         dt = mdy_hm(paste(cal, clock, sep = ' ')))

qc <- secondary_tides_clean %>%
  select(sec_tide)%>%
  unique()

############################################
# Now start combining files!
############################################

# First, join minmax_raw together with latlong by site, gpsunit and gps point

minmax_almost <- left_join(minmax_raw_2024, latlong_2024, 
                           by = c('site','gpsunit','gps_pt'))#Max used full join but since I didnt separate out the min/max gps points before joining the files, used left_join to isolate the points references on the field datasheets

# check to see how different the automatically generated GPS waypoint date/times are from the data sheet
dtchck <- minmax_almost %>%
  select(site, gpsunit, gps_pt,dt, dt_gps)%>%
  mutate(match = if_else(dt == dt_gps,
                         'y',
                         'n'))%>%
  group_by(match)%>%
  mutate(n = n())%>%
  ungroup()%>%
  mutate(disc = dt - dt_gps) #calculates the discrepancy between the datasheet time and the gps time

# Now create a list of all observations with a date/time discrepancy greater than 5 minutes
dt_qc <- dtchck %>%
  filter(disc < -300 | disc > 300)

minmax_2024 <- rename(minmax_almost)

#replace date/time with date/time from gps if there is no date/time from datasheet or if it is off
minmax_2024 <- minmax_2024 %>% 
  mutate(dt = if_else(is.na(dt_gps),
                     dt,
                     dt_gps))%>%
  select(-dt_gps)

# Do some quick QA/QC to see what lat/long data is missing from min/max and vice-versa

nogps <- minmax_2024 %>%
  filter(is.na(Latitude))%>%
  filter(!notes == 'absent')

#write.csv(nogps, './QC/no_match_GPS_points.csv')

noll <- minmax_2024 %>%
  filter(is.na(station))

#write.csv(noll, './QC/no_lat_long.csv')

#############################
#Double check of the secondary tide windows/data completeness

#determine the time windows collected for each secondary tide station
secondary_dts <- secondary_tides_clean %>%
  select(sec_tide, cal, clock)%>%
  separate(clock, into = c('h','m'), sep = ':')%>%
  mutate(cal = mdy(cal),
         timetxt = paste(h,m, sep = ""),
         timetxt = as.numeric(timetxt))%>%
  na.omit()%>%
  group_by(sec_tide, cal)%>%
  mutate(sec_start = min(timetxt),
         sec_finish = max(timetxt))%>%
  ungroup()%>%
  select(-timetxt, - h, -m)%>%
  unique()

#Check to see that your secondary tide time windows match the survey windows (sites with only primary tides will get NAs)

secondary_tide_dtcheck <- left_join(survey_windows_2024, secondary_dts, by = c('sec_tide','cal'))%>%
  mutate(start_check = if_else(sec_start<survey_start,
                               ': )',
                               'WRONG!'),
         end_check = if_else(sec_finish>survey_finish,
                             ': )',
                             'WRONG!'))


##################################################################################

# Now match tide station data with min/max depth data


# append tidal discrepancy by matching the closest time from NOAA to actual observation times
# have to parse the whole process out by primary tide station (pr_tide)
# 
# PROCESSING NOTE 2020-03-18 MC: In the future it will be worth automating this process with a loop
#

#subset min max data by primary tide station
minmax_seattle <- minmax_2024 %>% subset(pr_tide == 'seattle')
minmax_tacoma <- minmax_2024 %>% subset(pr_tide == 'tacoma')
minmax_porttownsend <- minmax_2024 %>% subset(pr_tide == 'porttownsend')
minmax_portangeles <- minmax_2024 %>% subset(pr_tide == 'portangeles')
minmax_cherrypoint <- minmax_2024 %>% subset(pr_tide == 'cherrypoint')
minmax_fridayharbor <- minmax_2024 %>% subset(pr_tide == 'fridayharbor')

minmax_seattle <- as.data.table(minmax_seattle)
minmax_tacoma <- as.data.table(minmax_tacoma)
minmax_porttownsend <- as.data.table(minmax_porttownsend)
minmax_portangeles <- as.data.table(minmax_portangeles)
minmax_cherrypoint <- as.data.table(minmax_cherrypoint)
minmax_firdayharbor <- as.data.table(minmax_fridayharbor)

#subset verified tide data by primary tide station
tides_seattle <- verified_tides_clean  %>% subset(tide_site == 'seattle')
tides_tacoma <- verified_tides_clean  %>% subset(tide_site == 'tacoma')
tides_porttownsend <- verified_tides_clean  %>% subset(tide_site == 'porttownsend')
tides_portangeles <- verified_tides_clean  %>% subset(tide_site == 'portangeles')
tides_cherrypoint <- verified_tides_clean  %>% subset(tide_site == 'cherrypoint')
tides_fridayharbor <- verified_tides_clean  %>% subset(tide_site == 'fridayharbor')

tides_seattle <- as.data.table(tides_seattle)
tides_tacoma <- as.data.table(tides_tacoma)
tides_porttownsend <- as.data.table(tides_porttownsend)
tides_portangeles <- as.data.table(tides_portangeles)
tides_cherrypoint <- as.data.table(tides_cherrypoint)
tides_fridayharbor <- as.data.table(tides_fridayharbor)


# These three lines match primary tide times to depth survey times by CLOSEST time

tide_match_seattle <- tides_seattle[minmax_seattle, on='dt', roll='nearest']
tide_match_tacoma <- tides_tacoma[minmax_tacoma, on='dt', roll='nearest']
tide_match_porttownsend <- tides_porttownsend[minmax_porttownsend, on='dt', roll='nearest']
tide_match_portangeles <- tides_portangeles[minmax_portangeles, on='dt', roll='nearest']
tide_match_cherrypoint <- tides_cherrypoint[minmax_cherrypoint, on='dt', roll='nearest']
tide_match_fridayharbor <- tides_fridayharbor[minmax_fridayharbor, on='dt', roll='nearest']

# smoosh em all together
minmax_2024_primary <- bind_rows(tide_match_seattle, tide_match_tacoma, tide_match_porttownsend, tide_match_portangeles, 
                                 tide_match_cherrypoint, tide_match_fridayharbor) %>%
  select(dt, year, site, station, pr_tide, sec_tide, type, obs_type,
         staff, gpsunit, minmax, gps_pt, Latitude, Longitude,
         depth_m_raw, actual_pred, actual_ver, act_pred_disc_m, notes)

# mix in the secondary tide station predictions
minmax_2024_primary_secondary <- left_join(minmax_2024_primary, secondary_tides_clean, by = c('dt','sec_tide')) %>%
  select(-cal, -clock)%>%
  mutate(tide_m = if_else(pr_tide == sec_tide,
                          actual_ver,
                          pred_tide + act_pred_disc_m),
         depth_m = (depth_m_raw * -1) + tide_m)

#clean it all up for saving and appending to the database
minmax_2024_almost_final <- minmax_2024_primary_secondary %>%
  separate(dt, into = c('cal','clock'), sep = ' ', remove = FALSE)%>%
  separate(cal, into = c('y','mth','d'), sep = '-')%>%
  separate(clock, into = c('h','m','s'), sep = ':')%>%
  mutate(depth_m = depth_m * 1,
         latitude = Latitude,
         longitude = Longitude,
         datetxt = as.numeric(paste(y,mth,d, sep = "")),
         timetxt = as.numeric(paste(h, m, sep ="")))%>%
  select(dt, datetxt, timetxt, year, site, type, station, obs_type, gpsunit, gps_pt, minmax, depth_m_raw, 
         depth_m, tide_m, staff, latitude, longitude, notes)

#get absent data set to merge
minmax_absent_2024_final <- minmax_absent_2024 %>%
  mutate(dt = NA, 
         depth_m = NA, 
         tide_m = NA, 
         latitude = NA, 
         longitude = NA) %>%
  select(dt, datetxt, timetxt, year, site, type, station, obs_type, gpsunit, gps_pt, minmax, depth_m_raw, 
         depth_m, tide_m, staff, latitude, longitude, notes)

#merge tide corrected data and absent data together
minmax_2024_final <- rbind(minmax_2024_almost_final, minmax_absent_2024_final)

write.csv(minmax_2024_final, "K:/kelp/bull_kelp_kayak/2024/data_processing/min_max_depth/outputs/2024_minmax_corrected_raw.csv")
write.csv(minmax_2024_final, 'K:/kelp/bull_kelp_kayak/2024/analysis/min_max_depth/data_raw/2024_minmax_corrected_raw.csv')





