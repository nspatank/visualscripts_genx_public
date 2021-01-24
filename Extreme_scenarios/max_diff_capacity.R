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
Resource_Name <- c("pumped_hydro", "battery","solar","offshore_wind", "onshore_wind", "hydro", "hydrogen_storage", "other_renewables","nuclear", "NGCCS", "ZCF")
color_list <- c("#66CCCC","#A6CEE3","#fccb6e", "#92CF72", "#33A02C","#408DBF", "#ff7700", "#FFDAB9", "#E94330","#DDA0DD", "#cd51e0")
Resource_color <- data.frame(Resource_Name, color_list)
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
Capacity <- read.csv(file="../data/Capacity.csv", header=TRUE, sep=",")
Capacity <- subset(Capacity, Capacity$Resource.Name != "flexible_load")

names(Capacity)[names(Capacity) == "Resource.Name"] <- "Resource_Name"
Capacity$EndCap <- Capacity$EndCap * 0.001
Capacity$Resource_Name <- factor(Capacity$Resource_Name,levels = Resource_Name)

#allocate resource type for each Resource_Name
Capacity$Type <- 'NA'
Capacity$Type[Capacity$Resource_Name=="pumped_hydro"] <- "Storage"
Capacity$Type[Capacity$Resource_Name=="battery"] <- "Storage"
Capacity$Type[Capacity$Resource_Name=="solar"] <- "solar"
Capacity$Type[Capacity$Resource_Name=="offshore_wind"] <- "offshore"
Capacity$Type[Capacity$Resource_Name=="onshore_wind"] <- "onshore"
Capacity$Type[Capacity$Resource_Name=="hydro"] <- "Clean Firm"
Capacity$Type[Capacity$Resource_Name=="hydrogen_storage"] <- "LDS"
Capacity$Type[Capacity$Resource_Name=="other_renewables"] <- "Clean Firm"
Capacity$Type[Capacity$Resource_Name=="nuclear"] <- "Clean Firm"
Capacity$Type[Capacity$Resource_Name=="NGCCS"] <- "Clean Firm"
Capacity$Type[Capacity$Resource_Name=="ZCF"] <- "ZCF"

#WECC-WIDE MAX/MIN - AGGREGATED PLOT
#Select iteration with maximum capacity for each Resource type defined above
iter_max_cap <- dplyr::select(Capacity, iter, Type, EndCap, Policy) %>%
  group_by(iter,Type,Policy) %>%
  summarise(sum=sum(EndCap)) %>%
  ungroup() %>%
  group_by(Type,Policy) %>%
  slice(which.max(sum)) %>%
  mutate(condition = "Max Cap") %>%
  ungroup() %>%
  left_join(dplyr::select(Capacity, Region, iter, Resource_Name, EndCap, Policy))

#Select iteration with minimum capacity for each Resource type defined above
iter_min_cap <- dplyr::select(Capacity, iter, Type, EndCap, Policy) %>%
  group_by(iter,Type,Policy) %>%
  summarise(sum=sum(EndCap)) %>%
  ungroup() %>%
  group_by(Type,Policy) %>%
  slice(which.min(sum)) %>%
  mutate(condition = "Min Cap") %>%
  left_join(dplyr::select(Capacity, Region, iter, Resource_Name, EndCap, Policy))

#join both the dataframes with iterations corresponding to max/min capacity of a give resource type
max_min_iter <- dplyr::bind_rows(iter_max_cap, iter_min_cap)

#aggregate the End Capacity for entire WECC for selected iterations
max_min_aggre <- dplyr::select(max_min_iter, iter, Resource_Name, EndCap, Policy, condition, Type) %>%
  group_by(iter,Type,Policy, condition, Resource_Name) %>%
  summarise(sum = sum(EndCap))

write.csv(distinct (max_min_aggre,iter), "../result_data/max_min_cap_agg.csv",row.names = FALSE)

ggplot(max_min_aggre, aes(x=Type, y=sum, fill=Resource_Name))+
  geom_bar(position="stack", stat="identity") + 
  theme_bw()+
  facet_grid(col = vars(condition), row= vars(Policy))+
  theme(text = element_text(size=10), legend.key.size = unit(1, "cm"),axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x=str_wrap("Resource Type", width=40), y="Capacity (GW)")+
  scale_fill_manual(name = "Resource",values = as.character(Resource_color$color_list))+
  ggsave("Max_min_Capacity_agg.png", width=10, height=5, dpi=500)

#REGION SPECIFIC MAX/MIN 
#Select iteration with maximum capacity for each Resource type defined above by Region
iter_max_cap_z <- dplyr::select(Capacity, iter, Type, EndCap, Policy, Region) %>%
  group_by(iter,Type,Policy, Region) %>%
  summarise(sum=sum(EndCap)) %>%
  ungroup() %>%
  group_by(Type,Policy,Region) %>%
  slice(which.max(sum)) %>%
  mutate(condition = "Max Cap") %>%
  ungroup() %>%
  left_join(dplyr::select(Capacity, Region, iter, Resource_Name, EndCap, Policy))

#Select iteration with minimum capacity for each Resource type defined above
iter_min_cap_z <- dplyr::select(Capacity, iter, Type, EndCap, Policy,Region) %>%
  group_by(iter,Type,Policy,Region) %>%
  summarise(sum=sum(EndCap)) %>%
  ungroup() %>%
  group_by(Type,Policy,Region) %>%
  slice(which.min(sum)) %>%
  mutate(condition = "Min Cap") %>%
  left_join(dplyr::select(Capacity, Region, iter, Resource_Name, EndCap, Policy))

#join both the dataframes with iterations corresponding to max/min capacity of a give resource type for each region
max_min_iter_z <- dplyr::bind_rows(iter_max_cap_z, iter_min_cap_z)
write.csv(distinct (max_min_iter_z,iter,Type, Policy, Region,condition), "../result_data/max_min_cap_z.csv", row.names = FALSE)

for (z in ZONE_MAP){
  max_min_iter_ <- subset(max_min_iter_z, max_min_iter_z$Region==z)
  Resource_color_ <- subset(Resource_color, Resource_color$Resource_Name %in% unique(max_min_iter_$Resource_Name))
  ggplot(max_min_iter_, aes(x=Type, y=EndCap, fill=Resource_Name))+
    geom_bar(position="stack", stat="identity") + 
    theme_bw()+
    facet_grid(col = vars(condition), row= vars(Policy))+
    theme(text = element_text(size=10), legend.key.size = unit(1, "cm"),axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(x=str_wrap("Resource Type", width=40), y="Capacity (GW)")+
    scale_fill_manual(name = "Resource",values = as.character(Resource_color_$color_list))+
    ggsave(paste("Max_min_Capacity_",z,".png"), width=10, height=5, dpi=500)
}


#SPECIFICALLY MAX/MIN VRE
#allocate resource type for each Resource_Name
Capacity$Type <- 'NA'
Capacity$Type[Capacity$Resource_Name=="pumped_hydro"] <- "Storage"
Capacity$Type[Capacity$Resource_Name=="battery"] <- "Storage"
Capacity$Type[Capacity$Resource_Name=="solar"] <- "VRE"
Capacity$Type[Capacity$Resource_Name=="offshore_wind"] <- "VRE"
Capacity$Type[Capacity$Resource_Name=="onshore_wind"] <- "VRE"
Capacity$Type[Capacity$Resource_Name=="hydro"] <- "Clean Firm"
Capacity$Type[Capacity$Resource_Name=="hydrogen_storage"] <- "LDS"
Capacity$Type[Capacity$Resource_Name=="other_renewables"] <- "Clean Firm"
Capacity$Type[Capacity$Resource_Name=="nuclear"] <- "Clean Firm"
Capacity$Type[Capacity$Resource_Name=="NGCCS"] <- "Clean Firm"
Capacity$Type[Capacity$Resource_Name=="ZCF"] <- "ZCF"

#WECC-WIDE MAX/MIN - AGGREGATED PLOT
#Select iteration with maximum capacity for each Resource type defined above
iter_max_cap <- dplyr::select(Capacity, iter, Type, EndCap, Policy) %>%
  group_by(iter,Type,Policy) %>%
  filter(Type=="VRE") %>%
  summarise(sum=sum(EndCap)) %>%
  ungroup() %>%
  group_by(Type,Policy) %>%
  slice(which.max(sum)) %>%
  mutate(condition = "Max Cap") %>%
  ungroup() %>%
  left_join(dplyr::select(Capacity, Region, iter, Resource_Name, EndCap, Policy))

#Select iteration with minimum capacity for each Resource type defined above
iter_min_cap <- dplyr::select(Capacity, iter, Type, EndCap, Policy) %>%
  group_by(iter,Type,Policy) %>%
  filter(Type=="VRE") %>%
  summarise(sum=sum(EndCap)) %>%
  ungroup() %>%
  group_by(Type,Policy) %>%
  slice(which.min(sum)) %>%
  mutate(condition = "Min Cap") %>%
  left_join(dplyr::select(Capacity, Region, iter, Resource_Name, EndCap, Policy))

#join both the dataframes with iterations corresponding to max/min capacity of a give resource type
max_min_iter <- dplyr::bind_rows(iter_max_cap, iter_min_cap)

#aggregate the End Capacity for entire WECC for selected iterations
max_min_aggre <- dplyr::select(max_min_iter, iter, Resource_Name, EndCap, Policy, condition, Type) %>%
  group_by(iter,Type,Policy, condition, Resource_Name) %>%
  summarise(sum = sum(EndCap))

write.csv(distinct (max_min_aggre,iter), "../result_data/max_min_cap_VRE.csv",row.names = FALSE)

