#Purpose of ths script is to determine if there is a relationship between height and biomass for grass samples
#collected from Carnation Farms (Test Flight 2) as part of the drone demonstration project (AMFAV-Grass) and
#to test for differences in fuel moisture among different color categories of grass seen in the site
#imagery collected from the UAV (DJI Phantom 3)

#Reset functions
rm(list=ls())
dev.off()

#Libraries
library(dplyr)
library(plyr)
library(data.table)

#Set working drive to University of Washington/FERA server
#IP Address: mfav-grass (\\172.16.0.145)
setwd("Z:/testFlight02/vegetation/")

##############################################################################################################
##############################################################################################################
##############################################################################################################
#BIOMASS AND HEIGHT RELATIONSHIP TEST

#Open file with tree species metadata:
clip <- read.table("20170725_vegetation_biomass_height.csv", header=TRUE, 
                          sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Calculate net weights
net_weight <- clip$gross_dry_weight_grams - clip$bag_weight_grams

#There are 4000 clip plot areas (0.25 square meters per hectare)
loading_multiplier <- 4000

#Calculate loading in grams per hectare
loading_perHectare_grams <- net_weight * loading_multiplier

#There are 1,000,000 grams per metric ton
grams_to_tons <- 1000000

#Calculate loading in metric tons per hectare
loading_perHectare_tons <- loading_perHectare_grams / grams_to_tons

#Create a data frame with new loading information
clip_processed <- data.frame(clip, net_weight_grams = net_weight, 
                             loading_tonsPerHectare = loading_perHectare_tons)

#Show average height and biomass by color type
#Sum loading for each category
summary_statistics <- ddply(clip_processed, "image_color_category", summarise, 
                         average_loading = mean(net_weight_grams, na.rm = T),
                         SD_loading = sd(net_weight_grams, na.rm = T),
                         min_moading = min(net_weight_grams, na.rm = T),
                         max_loading = max(net_weight_grams, na.rm = T), 
                         average_height = mean(vegetation_height_cm, na.rm = T),
                         SD_height = sd(vegetation_height_cm, na.rm = T),
                         min_height = min(vegetation_height_cm, na.rm = T),
                         max_height = max(vegetation_height_cm, na.rm = T))

##########----------GREEN
##########
###########Test for relationship between height and biomass for green color type.
height_green <- clip_processed$vegetation_height_cm[clip_processed$image_color_category == "green"]
weight_green <- clip_processed$net_weight_grams[clip_processed$image_color_category == "green"]

#Linear model
green_lm <- lm(weight_green ~ height_green)
summary(green_lm)

#Plot data
plot(height_green, weight_green)

#Relationship is not significant, but will be if I had collected more samples.

##########----------GREY
##########
###########Test for relationship between height and biomass for grey color type.
height_grey <- clip_processed$vegetation_height_cm[clip_processed$image_color_category == "grey"]
weight_grey <- clip_processed$net_weight_grams[clip_processed$image_color_category == "grey"]

#Linear model
grey_lm <- lm(weight_grey ~ height_grey)
summary(grey_lm)

#Plot data
plot(height_grey, weight_grey)

#Relationship is not significant, but should be if I had collected more samples.

##########----------BROWN
##########
###########Test for relationship between height and biomass for brown color type.
height_brown <- clip_processed$vegetation_height_cm[clip_processed$image_color_category == "brown"]
weight_brown <- clip_processed$net_weight_grams[clip_processed$image_color_category == "brown"]

#Linear model
brown_lm <- lm(weight_brown ~ height_brown)
summary(brown_lm)

#Plot data
plot(height_brown, weight_brown)

#Relationship is significant.

##########
##########
##########NOTES
#For all color types, it appears there may be a max response to height (i.e. boundary layer regression
#may be more appropriate). It would be useful to have more (10-30) samples to test the relationship.
#You also need a more objective system for measuring height. This should be consistent with heights 
#that image processing programs (Pix4D or AgiSoft) are calculating.

##########
##########
##########------------------------------------------------------------------------------END


##############################################################################################################
##############################################################################################################
##############################################################################################################
#FUEL MOISTURE

#Open file with fuel moisture data:
fm <- read.table("20170725_vegetation_fuelMoisture.csv", header=TRUE, 
                   sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Calculate net wet weight
net_wet <- fm$gross_wet_weight_grams - fm$wet_container_weight_grams

#Calculate net wet weight
net_dry <- fm$gross_dry_weight_grams - fm$dry_container_weight_grams

#Calculate difference between net wet and dry weights
moisture_weight <- net_wet - net_dry

#Calculate fuel moisture
fuel_moisture <- round((moisture_weight/net_dry)*100,2)

#Create a new data frame with processed data
fm_processed <- data.frame(fm, net_wet_weight = net_wet, net_dry_weight = net_dry, 
                           fuel_moisture = fuel_moisture)

#Fuel moisture summary
fm_summary_statistics <- ddply(fm_processed, "image_color_category", summarise, 
                            average_loading = mean(fuel_moisture, na.rm = T),
                            SD_loading = sd(fuel_moisture, na.rm = T),
                            min_moading = min(fuel_moisture, na.rm = T),
                            max_loading = max(fuel_moisture, na.rm = T))







