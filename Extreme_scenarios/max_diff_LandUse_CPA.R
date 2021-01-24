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
Raw_data <- read.csv(file="../data/Selected_CPAs_Rawp7_2045_high_low_REF_high.csv", header=TRUE, sep=",")

#REGION SPECIFIC LAND USE CORRELATION FOR MODEL REGIONS
#calculate solar PV area and spur line capacity for each iteration and normalize it between 0-1
summarized_Raw_data <- Raw_data %>%                                                         # read the raw capacity CSV file
  dplyr::select(iter, region, tech, lcoe, Area, interconnect_annuity,Capacity) %>%          # select a subset of columns
  group_by(iter,region, tech) %>%                                                           # add region to the grouping
  mutate(lcoe_ = lcoe*Capacity) %>%
  summarise(Built_area_z=sum(Area), Trans_cost = sum(interconnect_annuity), 
            LCOE = sum(lcoe_)/sum(Capacity)) %>%                                            # sum up the spur line capacity and area for each iter, policy and region
  ungroup() %>%                                                                             # ungroup 
  group_by(tech) %>%                                                                        # gruop by policy for normalization
  mutate(AreaNorm = Built_area_z/max(Built_area_z)) %>%                                     # normalize area
  mutate(SpurNorm = Trans_cost/max(Trans_cost)) %>%                                         # normalize spur line capacity
  mutate(LcoeNorm = LCOE/max(LCOE))
  
#join both the dataframes for solar and wind and cast them for the pair plot
area <- summarized_Raw_data %>%
  dplyr::select(iter, region,tech,AreaNorm) %>%
  cast(iter+tech~region) %>%
  mutate(quantity = "Area")
lcoe <- summarized_Raw_data %>%
  dplyr::select(iter, region,tech,LcoeNorm) %>%
  cast(iter+tech~region) %>%
  mutate(quantity = "LCOE")
Trans <- summarized_Raw_data %>%
  dplyr::select(iter, region,tech,SpurNorm) %>%
  cast(iter+tech~region) %>%
  mutate(quantity = "Inter. Annu.")

#plotting wind and solar area for high electrification scenario
solar_ <- dplyr::bind_rows(area, lcoe, Trans) %>%
  filter(tech == "UtilityPV")

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
#plotting correlation in solar area, lacoe and interconnect annuity for high electrification scenario
ggpairs(solar_, 
        columns = 3:8, 
        legend=1,
        title="Solar Correlation: Area, LCOE, Interconnect Annuity",
        diag = list(continuous = wrap(diagonal)),
        upper = list(continuous = wrap(ggally_cor, alignPercent = 0.8, size=2), 
                     mapping = ggplot2::aes(color = quantity)),
        lower = list(continuous = wrap(lowerfun)),
        ) +
  theme_bw()+
  theme(text = element_text(size=6), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggsave("Solar_correlatin_p7.png", width=10, height=5, dpi=500)


#plotting correlation in wind area, lacoe and interconnect annuity for high electrification scenario
Wind_ <- dplyr::bind_rows(area, lcoe, Trans) %>%
  filter(tech == "OnshoreWind")

ggpairs(Wind_, 
        columns = 3:8, 
        legend=1,
        title="Wind Correlation: Area, LCOE, Interconnect Annuity",
        diag = list(continuous = wrap(diagonal)),
        upper = list(continuous = wrap(ggally_cor, alignPercent = 0.8, size=2), 
                     mapping = ggplot2::aes(color = quantity)),
        lower = list(continuous = wrap(lowerfun)),
) +
  theme_bw()+
  theme(text = element_text(size=6), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggsave("Wind_correlatin_p7.png", width=10, height=5, dpi=500)

