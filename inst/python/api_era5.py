#!/usr/bin/env python3

#### 
# Import libraries
####

import cdsapi

####
# Retrieving the files from CDS
####

c = cdsapi.Client()
 
for year in range(1965, 1970): # Hyytiälä was planted in 1962
	c.retrieve(
	    'reanalysis-era5-single-levels',
	    {
		'product_type': 'reanalysis',
		'format': 'netcdf',
		'variable': ['2m_temperature', 'relative_humidity', 'leaf_area_index_high_vegetation', 'surface_solar_radiation_downwards', 'volumetric_soil_water_layer_1', 'volumetric_soil_water_layer_2',],
		'month': ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12',],
		'year': year,
		'day': ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31',],
		'time': ['00:00', '01:00', '02:00', '03:00', '04:00', '05:00', '06:00', '07:00', '08:00', '09:00', '10:00', '11:00', '12:00', '13:00', '14:00', '15:00', '16:00', '17:00', '18:00', '19:00', '20:00', '21:00', '22:00', '23:00',],
		'area': [61.9, 24.2, 61.8, 25.3,],
	    },
	    str(year)+'download.nc')


# After big datasets are downloaded need to download the dew point to get RH

for year in range(1965, 1970): # Hyytiälä was planted in 1962
	c.retrieve(
	    'reanalysis-era5-single-levels',
	    {
		'product_type': 'reanalysis',
		'format': 'netcdf',
		'variable': '2m_dewpoint_temperature',
		'month': ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12',],
		'year': year,
		'day': ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31',],
		'time': ['00:00', '01:00', '02:00', '03:00', '04:00', '05:00', '06:00', '07:00', '08:00', '09:00', '10:00', '11:00', '12:00', '13:00', '14:00', '15:00', '16:00', '17:00', '18:00', '19:00', '20:00', '21:00', '22:00', '23:00',],
		'area': [61.9, 24.2, 61.8, 25.3,],
	    },
	    str(year)+'download_td.nc')
   
