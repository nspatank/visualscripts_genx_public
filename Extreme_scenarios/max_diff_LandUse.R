setwd('/D/Collab/Xiili/Extreme_scenarios/plots')
getwd()
#tools, install packages, type in the name of the packages 
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(RSQLite)
library(gridExtra)
library(dplyr)
library(reshape)
library(MASS)
library(reshape2)
library(GGally)
library(lattice)
library(data.table)
library(splitstackshape)
library(ggvis)
rm(list = setdiff ( ls() , "") )

#define sets
ZONE_MAP <- c(
  "CA_N",
  "CA_S",
  "WECC_N",
  "WECC_NMAZ",
  "WECC_WYCO",
  "WECC_PNW"
)

POLICY_ORDER <- c(
  "p2_2045_high_mid_REF_high",
  "p7_2045_high_low_REF_high"
)

# read the CSV file for all MGA iterations
Gen_data <- read.csv(file="../data/Generators_data.csv", header=TRUE, sep=",")
Raw_Capacity <- read.csv(file="../data/Raw_Capacity.csv", header=TRUE, sep=",")

#REGION SPECIFIC LAND USE CORRELATION FOR MODEL REGIONS
#calculate solar PV area and spur line capacity for each iteration and normalize it between 0-1
Solar_area <- Raw_Capacity %>%                                                              # read the raw capacity CSV file
  dplyr::select(iter, Region, Resource.Name, EndCap, Policy, C) %>%                         # select a subset of columns
  group_by(iter,Policy) %>%                                                                 # want to add R_ID column for each iter and policy so that we can merge raw capacity with generators_data file
  mutate(R_ID = 1:n()) %>%                                                                  # add the R_ID column
  left_join(dplyr::select(Gen_data, R_ID, Resource, Max_Cap_MW, spur_miles)) %>%            # add the spur miles and Max_Cap_MW columns from generators data file to raw capacity file
  filter(Resource.Name == "solar" & Max_Cap_MW > 0) %>%                                     # select data corresponding to new build solar resources
  mutate(spur_MW_mile = spur_miles * EndCap) %>%                                            # calculate capacity of spur line built
  mutate(Built_area = EndCap / (45 * 0.2)) %>%                                              # calculate area for the new capacity
  ungroup() %>%                                                                             # ungroup because we want to create new grouping with region
  group_by(Policy,Region, iter) %>%                                                         # add region to the grouping
  summarise(Built_area_z=sum(Built_area), Built_spur_z = sum(spur_MW_mile)) %>%             # sum up the spur line capacity and area for each iter, policy and region
  ungroup() %>%                                                                             # ungroup 
  group_by(Policy) %>%                                                                      # gruop by policy for normalization
  mutate(Resource = "Solar") %>%
  mutate(AreaNorm = Built_area_z/max(Built_area_z)) %>%                                     # normalize area
  mutate(SpurNorm = Built_spur_z/max(Built_spur_z))                                         # normalize spur line capacity
  
  
#calculate Wind PV area for each iteration and normalize it between 0-1
Wind_area <- Raw_Capacity %>%                                                               # read the raw capacity CSV file
  dplyr::select(iter, Region, Resource.Name, EndCap, Policy, C) %>%                         # select a subset of columns
  group_by(iter,Policy) %>%                                                                 # want to add R_ID column for each iter and policy so that we can merge raw capacity with generators_data file
  mutate(R_ID = 1:n()) %>%                                                                  # add the R_ID column
  left_join(dplyr::select(Gen_data, R_ID, Resource, Max_Cap_MW, spur_miles)) %>%            # add the spur miles and Max_Cap_MW columns from generators data file to raw capacity file
  filter(Resource.Name == "onshore_wind" & Max_Cap_MW > 0) %>%                              # select data corresponding to new build Wind resources
  mutate(spur_MW_mile = spur_miles * EndCap) %>%                                            # calculate capacity of spur line built
  mutate(Built_area = EndCap / (45 * 0.2)) %>%                                              # calculate area for the new capacity
  ungroup() %>%                                                                             # ungroup because we want to create new grouping with region
  group_by(Policy,Region, iter) %>%                                                         # add region to the grouping
  summarise(Built_area_z=sum(Built_area), Built_spur_z = sum(spur_MW_mile)) %>%             # sum up the spur line capacity and area for each iter, policy and region
  ungroup() %>%                                                                             # ungroup 
  group_by(Policy) %>%                                                                      # gruop by policy for normalization
  mutate(Resource = "Wind") %>%
  mutate(AreaNorm = Built_area_z/max(Built_area_z)) %>%                                     # normalize area
  mutate(SpurNorm = Built_spur_z/max(Built_spur_z))                                         # normalize spur line capacity
  
  
#join both the dataframes for solar and wind and cast them for the pair plot
solar_wind_area <- dplyr::bind_rows(Solar_area, Wind_area) %>%
  dplyr::select(iter, Region, Policy, Resource, AreaNorm) %>%
  cast(Policy+iter+Resource~Region) %>%
  mutate(quantity = "Area")
#join both the dataframes for solar and wind and cast them for the pair plot
solar_wind_SpurMile <- dplyr::bind_rows(Solar_area, Wind_area) %>%
  dplyr::select(iter, Region, Policy, Resource, SpurNorm) %>%
  cast(Policy+iter+Resource~Region) %>%
  mutate(quantity = "Trans")

#plotting wind and solar area for high electrification scenario
solar_spurline_area_ <- dplyr::bind_rows(solar_wind_area, solar_wind_SpurMile) %>%
  filter(Policy == POLICY_ORDER[2] & Resource == "Solar")

lowerfun <- function(data,mapping){
  ggplot("smooth",data = data, mapping = mapping, aes(colour=quantity))+
    geom_point(alpha = 0.3, size=0.1, aes(colour=quantity))+
    geom_smooth(method="lm",alpha = 0.3, size=0.5, aes(colour=quantity), se=F)+
    scale_x_continuous(limits = c(0,1))+
    scale_y_continuous(limits = c(0,1))
}

diagonal <- function(data,mapping){
  ggplot(data = data, mapping = mapping, aes(colour=quantity))+
    geom_density(alpha = 0.3, aes(y=..scaled..,fill=quantity,colour=quantity))+
    scale_x_continuous(limits = c(0,1))+
    scale_y_continuous(limits = c(0,1))
}

ggpairs(solar_spurline_area_, 
        columns = 4:9, 
        legend=1,
        title="Solar Land-use and Spur Line Capacity Correlation",
        diag = list(continuous = wrap(diagonal)),
        upper = list(continuous = wrap(ggally_cor, alignPercent = 0.8, size=2), 
                     mapping = ggplot2::aes(color = quantity)),
        lower = list(continuous = wrap(lowerfun)),
        ) +
  theme_bw()+
  theme(text = element_text(size=6), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggsave("Solar_LandUse_SpurLine_correlatin.png", width=10, height=5, dpi=500)





#plotting wind and Wind area for high electrification scenario
Wind_spurline_area_ <- dplyr::bind_rows(solar_wind_area, solar_wind_SpurMile) %>%
  filter(Policy == POLICY_ORDER[2] & Resource == "Wind")

lowerfun <- function(data,mapping){
  ggplot("smooth",data = data, mapping = mapping, aes(colour=quantity))+
    geom_point(alpha = 0.3, size=0.1, aes(colour=quantity))+
    geom_smooth(method="lm",alpha = 0.3, size=0.5, aes(colour=quantity), se=F)+
    scale_x_continuous(limits = c(0,1))+
    scale_y_continuous(limits = c(0,1))
}

diagonal <- function(data,mapping){
  ggplot(data = data, mapping = mapping, aes(colour=quantity))+
    geom_density(alpha = 0.3, aes(y=..scaled..,fill=quantity,colour=quantity))+
    scale_x_continuous(limits = c(0,1))+
    scale_y_continuous(limits = c(0,1))
}

ggpairs(Wind_spurline_area_, 
        columns = 4:9, 
        legend=1,
        title="Wind Land-use and Spur Line Capacity Correlation",
        diag = list(continuous = wrap(diagonal)),
        upper = list(continuous = wrap(ggally_cor, alignPercent = 0.8, size=2), 
                     mapping = ggplot2::aes(color = quantity)),
        lower = list(continuous = wrap(lowerfun)),
) +
  theme_bw()+
  theme(text = element_text(size=6), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggsave("Wind_LandUse_SpurLine_correlatin.png", width=10, height=5, dpi=500)
