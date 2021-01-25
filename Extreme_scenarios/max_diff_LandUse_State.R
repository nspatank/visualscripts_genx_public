setwd('/D/Collab/Xiili/Extreme_scenarios/plots')
getwd()
#tools, install packages, type in the name of the packages 
library(ggplot2)
library(ggpubr)
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
library(tidyr)
library(viridis)
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
MGA <- read.csv(file="../data/Capacity_MGA.csv", header=TRUE, sep=",") %>%
  distinct(Policy, folder, iteration) %>%
  filter(iteration == "MGA_1" | iteration =="MGA_2"| iteration =="MGA_3") %>%
  separate(folder, c('d','iter'), sep="_0.05_") %>%
  dplyr::rename(Type = iteration) %>%
  mutate(condition="",case="MGA",Region="WECC") %>%
  dplyr::select(-d)


#gather selected iterations and combine them
iter_cap <- dplyr::bind_rows(
  read.csv(file="../result_data/max_min_cap_agg.csv", header=TRUE, sep=",") %>%
    mutate(case = "cap_agg", Region = "WECC"),
  read.csv(file="../result_data/max_min_cap_z.csv", header=TRUE, sep=",") %>%
    mutate(case = "cap_z"),
  read.csv(file="../result_data/max_min_cap_VRE.csv", header=TRUE, sep=",") %>%
    mutate(case = "cap_vre", Region = "WECC") %>%
    mutate(iter=as.factor(iter)),
  read.csv(file="../result_data/max_min_trans.csv", header=TRUE, sep=",") %>%
    mutate(case = "trans_agg", Region = "WECC", Type = "Transmission") %>%
    mutate(iter=as.factor(iter)),
  MGA,
  data.frame(iter="Base",Type="",Policy=POLICY_ORDER[2],condition="Least Cost",case=NA,Region="WECC")) %>%
  filter(Policy == POLICY_ORDER[2])

#REGION SPECIFIC LAND USE CORRELATION FOR MODEL REGIONS
#calculate solar PV area and spur line capacity for each iteration and normalize it between 0-1
summarized_Raw_data <- Raw_data %>%                                                         # read the raw capacity CSV file
  dplyr::select(iter, tech, Area, state) %>%                                                # select a subset of columns
  group_by(iter,state, tech) %>%                                                            # add region to the grouping
  summarise(Built_area=sum(Area)) %>%                                                       # sum up the spur line capacity and area for each iter, policy and region
  right_join(iter_cap) %>%
  ungroup() %>%                                                                             # ungroup 
  filter(!Type %in% c("LDS","ZCF","offshore") &
           !state %in% c("Texas","North Dakota","Nebraska") & Region=="WECC") %>%
  group_by(tech) %>%                                                                        # gruop by policy for normalization
  mutate(AreaNorm = Built_area/max(Built_area)) %>%                                             # normalize area
  mutate(unique = paste(Type,condition))

# min/max land use - sum AreaNorm over tech and state, find case with max AreaNorm - highest land use, 
A <- summarized_Raw_data %>% 
  group_by(unique) %>% 
  summarize(sum = sum(AreaNorm)) %>%
  arrange(sum)

summarized_Raw_data$unique <- factor(summarized_Raw_data$unique,levels = unique(A$unique))

p1 <- ggplot(summarized_Raw_data,aes(x=state, y=unique, fill=AreaNorm))+
  geom_tile(color="white", size=.5)+
  facet_grid(~tech, scales="free_y")+
  theme_bw()+
  ggtitle("Land use impact for states in WECC under different criteria")+
  scale_fill_gradientn(colours = brewer.pal(8, "OrRd")) +
  labs(x=str_wrap("States in WECC", width=40), y="WECC-wide Criteria")+
  theme(text = element_text(size=10), legend.key.size = unit(1, "cm"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none")+
  annotate("rect", xmin = 0, xmax = 12, ymin = 9.5, ymax = 10.5, color="black", alpha=0, size=0.1)+
  annotate("text",x = 10, y = 10,label="Highest footprint", size=2)+
  annotate("rect", xmin = 0, xmax = 12, ymin = 10.5, ymax = 11.5, color="black", alpha=0, size=0.1)+
  annotate("text",x = 10, y = 11,label="Lowest footprint", size=2)

#total land use in WECC for solar and wind
sum_land_use <- dplyr::select(summarized_Raw_data, tech, Policy, unique, Built_area) %>%
  group_by(tech, unique, Policy) %>%
  summarize(Area = sum(Built_area))

p2 <- ggplot(sum_land_use,aes(x=tech, y=unique, fill=Area))+
  geom_tile(color="white", size=.5)+
  theme_bw()+
  ggtitle(str_wrap("Built Area (thous. km^2)", width=15))+
  geom_text(aes(label = round(sum_land_use$Area/1000,1)), size=3)+
  scale_fill_gradientn(colours = brewer.pal(8, "OrRd")) +
  labs(x=str_wrap("WECC", width=40), y="")+
  theme(text = element_text(size=10), legend.key.size = unit(1, "cm"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

# computing footprint for wind and solar
summarized_Raw_data$footprint = 0
summarized_Raw_data$footprint[summarized_Raw_data$tech=="UtilityPV"] <- summarized_Raw_data$Built_area[summarized_Raw_data$tech=="UtilityPV"] * 0.91
summarized_Raw_data$footprint[summarized_Raw_data$tech=="OnshoreWind"] <- summarized_Raw_data$Built_area[summarized_Raw_data$tech=="OnshoreWind"] * 0.01
sum_footprint <- dplyr::select(summarized_Raw_data, tech, Policy, unique, footprint) %>%
  group_by(tech, unique, Policy) %>%
  summarize(Footprint = sum(footprint))

p3 <- ggplot(sum_footprint,aes(x=tech, y=unique, fill=Footprint))+
  geom_tile(color="white", size=.5)+
  theme_bw()+
  ggtitle(str_wrap("Footprint Area (thous. km^2)", width=20))+
  geom_text(aes(label = round(sum_footprint$Footprint/1000,1)), size=3)+
  scale_fill_gradientn(colours = brewer.pal(8, "OrRd")) +
  labs(x=str_wrap("WECC", width=40), y="")+
  theme(text = element_text(size=10), legend.key.size = unit(1, "cm"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

g <- grid.arrange(p1, p2, p3, nrow = 1,widths = c(4, 1,1))
ggsave(file="LandUse.png", g) 


