# READING NC FILE

###
# 
###

# Path
path_nc <- "./inst/python/"

# Variables
variables <- c('2m_temperature', 
               'leaf_area_index_high_vegetation',
               'surface_solar_radiation_downwards',
               'type_of_high_vegetation',
               'volumetric_soil_water_layer_1',
               'volumetric_soil_water_layer_2')

# Reading nc files
for (variable in variables) {
  nc_files <- list.files(path = path_nc)
}

