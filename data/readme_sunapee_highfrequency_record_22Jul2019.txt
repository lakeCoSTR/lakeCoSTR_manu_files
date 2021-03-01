File created 26March2019 by B. Steele, Weathers Lab, Cary Institue of Ecosystem Stuides (steeleb@caryinstitute.org)

Updated 22Jul2019 by B. Steele

This text file contains information pertaining to the .csv file 'sunapee_highfrequency_record_22Jul2019.csv'

These data are only for use by those working on the NASA IDS grant - DO NOT DISTRIBUTE. 

Only high-frequency data records which have been QAQC'd for obviously errant data are used in this collation.


column_name	description						units			notes

datetime	datetime of observation					YYYY-MM-DD HH:MM:SS	no DST observed -- EST (UTC -5)
lat_dd		latitude of observation in decimal degrees		decimal degrees		WGS84
long_dd		longitude of observation in decimal degrees		decimal degrees		WGS84
depth_m		depth of observation					meters			NA
temp_degC	temperature at depth of observation			degrees Celcius		NA
125m_buff	T/F whether site is within a 125m buffer of shore	1 or 0			1 = site is within 125m of shore, 0 = site is not within 125m of shore
500m_buff	T/F whether site is within a 500m buffer of shore	1 or 0			1 = site is within 500m of shore, 0 = site is not within 500m of shore
source		source of data						NA			see below for definition


data included in this collation ('source' column)
buoy thermistors: 	buoy data 2007-May 2019 ALL DEPTHS
upper do:		buoy upper do sensor 2007-May 2019 - not as precise as temperature from thermistors. It is suggested that, if given data from both thermistors and do sensor, only use the thermistor data. 
lower do:		buoy lower do sensor - not as precise as temperature from thermistors. It is suggested that, if given data from both thermistors and do sensor, only use the thermistor data. 
buoy hobo sensors: 	buoy data in winter at loon location 2014-2019and summer of 2015 at loon location
onset 1hr temp @ gloeo recruitment traps: 	1-hour data 2006-2018 (2018 season does not include data from coffin here), from longterm summer monitoring at gloeo traps
hobo 10min temp@ gloeo rectuitment traps:	10-minute data 2009-2017 from longterm summer monitoring at gloeo traps
2018 mini buoy:		buoy data from Nicole Ward - mini buoys with hobo loggers at 3 locations in the lake

