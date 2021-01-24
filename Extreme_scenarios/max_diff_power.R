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

#power generation plots
Resource_Name_P <- c("solar","offshore_wind", "onshore_wind", "hydro", "other_renewables","nuclear", "NGCCS", "ZCF")
color_list_P <- c("#fccb6e", "#92CF72", "#33A02C","#408DBF", "#FFDAB9", "#E94330","#DDA0DD", "#cd51e0")
Resource_color_P <- data.frame(Resource_Name_P, color_list_P)

# read the CSV file for all MGA iterations
Power <- read.csv(file="../data/Power.csv", header=TRUE, sep=",")
Power <- subset(Power, Power$Resource.Name != "flexible_load" & Power$Resource.Name != "battery" & Power$Resource.Name != "hydrogen_storage" & Power$Resource.Name != "pumped_hydro")

names(Power)[names(Power) == "Resource.Name"] <- "Resource_Name"
Power$EndPower <- Power$Sum * 1#0.001
Power$Resource_Name <- factor(Power$Resource_Name,levels = Resource_Name_P)

#allocate resource type for each Resource_Name
Power$Type <- 'NA'
Power$Type[Power$Resource_Name=="pumped_hydro"] <- "Storage"
Power$Type[Power$Resource_Name=="battery"] <- "Storage"
Power$Type[Power$Resource_Name=="solar"] <- "VRE"
Power$Type[Power$Resource_Name=="offshore_wind"] <- "VRE"
Power$Type[Power$Resource_Name=="onshore_wind"] <- "VRE"
Power$Type[Power$Resource_Name=="hydro"] <- "Clean Firm"
Power$Type[Power$Resource_Name=="hydrogen_storage"] <- "LDS"
Power$Type[Power$Resource_Name=="other_renewables"] <- "Clean Firm"
Power$Type[Power$Resource_Name=="nuclear"] <- "Clean Firm"
Power$Type[Power$Resource_Name=="NGCCS"] <- "Clean Firm"
Power$Type[Power$Resource_Name=="ZCF"] <- "ZCF"

#Select iteration with maximum Power for each Resource type defined above
iter_max_gen <- dplyr::select(Power, iter, Type, EndPower, Policy) %>%
  group_by(iter,Type,Policy) %>%
  summarise(sum=sum(EndPower)) %>%
  ungroup() %>%
  group_by(Type,Policy) %>%
  slice(which.max(sum)) %>%
  mutate(condition = "Max Gen") %>%
  ungroup() %>%
  left_join(dplyr::select(Power, Region, iter, Resource_Name, EndPower, Policy))

#Select iteration with minimum Power for each Resource type defined above
iter_min_gen <- dplyr::select(Power, iter, Type, EndPower, Policy) %>%
  group_by(iter,Type,Policy) %>%
  summarise(sum=sum(EndPower)) %>%
  ungroup() %>%
  group_by(Type,Policy) %>%
  slice(which.min(sum)) %>%
  mutate(condition = "Min Gen") %>%
  left_join(dplyr::select(Power, Region, iter, Resource_Name, EndPower, Policy))

#join both the dataframes with iterations corresponding to max/min Power of a give resource type
max_min_iter_gen <- dplyr::bind_rows(iter_max_gen, iter_min_gen)

#aggregate the End Power for entire WECC for selected iterations
max_min_aggre <- dplyr::select(max_min_iter_gen, iter, Resource_Name, EndPower, Policy, condition, Type) %>%
  group_by(iter,Type,Policy, condition, Resource_Name) %>%
  summarise(sum = sum(EndPower))
write.csv(distinct (max_min_aggre,iter,Type, Policy, condition), "../result_data/max_min_gen.csv",row.names = FALSE)

#plot aggregated Power
ggplot(max_min_aggre, aes(x=Type, y=sum, fill=Resource_Name))+
  geom_bar(position="stack", stat="identity") + 
  theme_bw()+
  facet_grid(col = vars(condition), row= vars(Policy))+
  theme(text = element_text(size=10), legend.key.size = unit(1, "cm"),axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x=str_wrap("Resource Type", width=40), y="Power (GWh)")+
  scale_fill_manual(name = "Resource",values = as.character(Resource_color_P$color_list))+
  ggsave("Max_min_Power_agg.png", width=10, height=5, dpi=500)

#REGION SPECIFIC MAX/MIN 
#Select iteration with maximum Power for each Resource type defined above by Region
iter_max_gen_z <- dplyr::select(Power, iter, Type, EndPower, Policy, Region) %>%
  group_by(iter,Type,Policy, Region) %>%
  summarise(sum=sum(EndPower)) %>%
  ungroup() %>%
  group_by(Type,Policy,Region) %>%
  slice(which.max(sum)) %>%
  mutate(condition = "Max gen") %>%
  ungroup() %>%
  left_join(dplyr::select(Power, Region, iter, Resource_Name, EndPower, Policy))

#Select iteration with minimum Power for each Resource type defined above
iter_min_gen_z <- dplyr::select(Power, iter, Type, EndPower, Policy,Region) %>%
  group_by(iter,Type,Policy,Region) %>%
  summarise(sum=sum(EndPower)) %>%
  ungroup() %>%
  group_by(Type,Policy,Region) %>%
  slice(which.min(sum)) %>%
  mutate(condition = "Min gen") %>%
  left_join(dplyr::select(Power, Region, iter, Resource_Name, EndPower, Policy))

#join both the dataframes with iterations corresponding to max/min Power of a give resource type for each region
max_min_iter_z <- dplyr::bind_rows(iter_max_gen_z, iter_min_gen_z)
write.csv(distinct (max_min_iter_z,iter,Type, Policy, Region, condition), "../result_data/max_min_gen_z.csv",row.names = FALSE)

#plot selected iteration for each region
for (z in ZONE_MAP){
  max_min_iter_gen_z <- subset(max_min_iter_z, max_min_iter_z$Region==z)
  Resource_color_P_ <- subset(Resource_color_P, Resource_color_P$Resource_Name %in% unique(max_min_iter_gen_z$Resource_Name))
  ggplot(max_min_iter_gen_z, aes(x=Type, y=EndPower, fill=Resource_Name))+
    geom_bar(position="stack", stat="identity") + 
    theme_bw()+
    facet_grid(col = vars(condition), row= vars(Policy))+
    theme(text = element_text(size=10), legend.key.size = unit(1, "cm"),axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(x=str_wrap("Resource Type", width=40), y="Power (GWh)")+
    scale_fill_manual(name = "Resource",values = as.character(Resource_color_P_$color_list))+
    ggsave(paste("Max_min_Power_",z,".png"), width=10, height=5, dpi=500)
}
