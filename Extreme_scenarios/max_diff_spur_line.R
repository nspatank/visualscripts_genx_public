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
Resource_Name <- c("pumped_hydro", "battery","solar","offshore_wind", "onshore_wind", "hydro", "hydrogen_storage", "other_renewables","nuclear", "NGCCS", "ZCF")
color_list <- c("#66CCCC","#A6CEE3","#fccb6e", "#92CF72", "#33A02C","#408DBF", "#ff7700", "#FFDAB9", "#E94330","#DDA0DD", "#cd51e0")
Resource_color <- data.frame(Resource_Name, color_list)
# read the CSV file for all MGA iterations
Spur_line <- read.csv(file="../data/Generators_data.csv", header=TRUE, sep=",")
Capacity <- read.csv(file="../data/Capacity.csv", header=TRUE, sep=",")
Capacity <- subset(Capacity, Capacity$Resource.Name != "flexible_load")
Capacity$Resource.Name <- factor(Capacity$Resource.Name,levels = Resource_Name)

#WECC-WIDE MAX/MIN spur line cost- AGGREGATED
#Select iteration with maximum spur line cost for each Resource type and policy
Spur_cost_max <- read.csv(file="../data/Raw_Capacity.csv", header=TRUE, sep=",") %>%        # read the raw capacity CSV file
  dplyr::select(iter, Region, Resource.Name, EndCap, Policy, C) %>%                         # select a subset of columns
  group_by(iter,Policy) %>%                                                                 # want to add R_ID column for each iter and policy so that we can merge raw capacity with generators_data file
  mutate(R_ID = 1:n()) %>%                                                                  # add the R_ID column
  left_join(dplyr::select(Spur_line, R_ID,Resource, spur_miles, interconnect_annuity)) %>%  # add the spur miles and interconnect annuity columns from generators data file to raw capacity file
  filter(spur_miles>0) %>%                                                                  # keep only new resources with positive spur miles
  mutate(spur_cost = interconnect_annuity * EndCap * 10^-3) %>%                             # calculate cost of spur line in thousands of $
  ungroup() %>%                                                                             # ungroup because we want to create new grouping with resource type
  group_by(iter, Policy, Resource.Name) %>%                                                 # add resource name to the grouping
  summarise(sum=sum(spur_cost)) %>%                                                         # sum up the spur line cost for each iter, policy and resource type
  ungroup() %>%                                                                             # ungroup 
  group_by(Policy, Resource.Name) %>%                                                       # gruop by policy and resource type because now we want to find the iterations with highest spur line cost
  slice(which.max(sum)) %>%                                                                 # find the iteratin with highest spur line cost
  mutate(condition = "Max Spur cost") %>%                                                   # add a column named 'condition' with entries - 'Max Spur Cost'
  left_join(dplyr::select(Capacity, iter,Resource.Name, Policy, EndCap)                     # now extract the resource capacity from capacity.csv for selected iterations
            %>% dplyr::rename(Resource_Name = Resource.Name))
  
#Select iteration with minimum spur line cost for each Resource type and policy
Spur_cost_min <- read.csv(file="../data/Raw_Capacity.csv", header=TRUE, sep=",") %>%        # read the raw capacity CSV file
  dplyr::select(iter, Region, Resource.Name, EndCap, Policy, C) %>%                         # select a subset of columns
  group_by(iter,Policy) %>%                                                                 # want to add R_ID column for each iter and policy so that we can merge raw capacity with generators_data file
  mutate(R_ID = 1:n()) %>%                                                                  # add the R_ID column
  left_join(dplyr::select(Spur_line, R_ID,Resource, spur_miles, interconnect_annuity)) %>%  # add the spur miles and interconnect annuity columns from generators data file to raw capacity file
  filter(spur_miles>0) %>%                                                                  # keep only new resources with positive spur miles
  mutate(spur_cost = interconnect_annuity * EndCap * 10^-3) %>%                             # calculate cost of spur line in thuosand of $
  ungroup() %>%                                                                             # ungroup because we want to create new grouping with resource type
  group_by(iter, Policy, Resource.Name) %>%                                                 # add resource name to the grouping
  summarise(sum=sum(spur_cost)) %>%                                                         # sum up the spur line cost for each iter, policy and resource type
  ungroup() %>%                                                                             # ungroup 
  group_by(Policy, Resource.Name) %>%                                                       # gruop by policy and resource type because now we want to find the iterations with highest spur line cost
  slice(which.min(sum)) %>%                                                                 # find the iteratin with lowest spur line cost
  mutate(condition = "Min Spur cost") %>%                                                   # add a column named 'condition' with entries - 'min Spur Cost'
  left_join(dplyr::select(Capacity, iter,Resource.Name, Policy, EndCap)                     # now extract the resource capacity from capacity.csv for selected iterations
            %>% dplyr::rename(Resource_Name = Resource.Name))

#join both the dataframes with iterations corresponding to max/min capacity of a give resource type
max_min_iter <- dplyr::bind_rows(Spur_cost_min,Spur_cost_max)

ggplot(max_min_iter, aes(x=condition, y=EndCap*0.001, fill = Resource_Name))+
  geom_bar(position="stack", stat="identity") + 
  geom_point(aes(y=sum*10^-4) )+ 
  theme_bw()+
  facet_grid(col = vars(Resource.Name), row= vars(Policy))+
  theme(text = element_text(size=8), legend.key.size = unit(1, "cm"),axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(name = "Capacity (GW)", sec.axis = sec_axis( trans=~.*.01, name="Spur line cost ($ million)"))+
  scale_fill_manual(name = "Resource",values = as.character(Resource_color$color_list))+
  ggsave("Max_min_SpurCost.png", width=8, height=6, dpi=500)






#REGION SPECIFIC MAX/MIN 
#Select iteration with maximum spur line cost for each Resource type, policy and region
Spur_cost_max_z <- read.csv(file="../data/Raw_Capacity.csv", header=TRUE, sep=",") %>%      # read the raw capacity CSV file
  dplyr::select(iter, Region, Resource.Name, EndCap, Policy, C) %>%                         # select a subset of columns
  group_by(iter,Policy) %>%                                                                 # want to add R_ID column for each iter and policy so that we can merge raw capacity with generators_data file
  mutate(R_ID = 1:n()) %>%                                                                  # add the R_ID column
  left_join(dplyr::select(Spur_line, R_ID,Resource, spur_miles, interconnect_annuity)) %>%  # add the spur miles and interconnect annuity columns from generators data file to raw capacity file
  filter(spur_miles>0) %>%                                                                  # keep only new resources with positive spur miles
  mutate(spur_cost = interconnect_annuity * EndCap * 10^-3) %>%                             # calculate cost of spur line in thousands of $
  ungroup() %>%                                                                             # ungroup because we want to create new grouping with resource type
  group_by(iter, Policy, Resource.Name, Region) %>%                                         # add resource name to the grouping
  summarise(sum=sum(spur_cost)) %>%                                                         # sum up the spur line cost for each iter, policy and resource type
  ungroup() %>%                                                                             # ungroup 
  group_by(Policy, Resource.Name, Region) %>%                                               # gruop by policy and resource type because now we want to find the iterations with highest spur line cost
  slice(which.max(sum)) %>%                                                                 # find the iteratin with highest spur line cost
  mutate(condition = "Max Spur cost") %>%                                                   # add a column named 'condition' with entries - 'Max Spur Cost'
  dplyr::rename(Region_sel = Region) %>%                                                    # rename the 'Region' column as Regin selected
  left_join(dplyr::select(Capacity, iter,Resource.Name, Policy, EndCap, Region)             # now extract the resource capacity from capacity.csv for selected iterations
            %>% dplyr::rename(Resource_Name = Resource.Name))

#Select iteration with minimum spur line cost for each Resource type and policy
Spur_cost_min_z <- read.csv(file="../data/Raw_Capacity.csv", header=TRUE, sep=",") %>%      # read the raw capacity CSV file
  dplyr::select(iter, Region, Resource.Name, EndCap, Policy, C) %>%                         # select a subset of columns
  group_by(iter,Policy) %>%                                                                 # want to add R_ID column for each iter and policy so that we can merge raw capacity with generators_data file
  mutate(R_ID = 1:n()) %>%                                                                  # add the R_ID column
  left_join(dplyr::select(Spur_line, R_ID,Resource, spur_miles, interconnect_annuity)) %>%  # add the spur miles and interconnect annuity columns from generators data file to raw capacity file
  filter(spur_miles>0) %>%                                                                  # keep only new resources with positive spur miles
  mutate(spur_cost = interconnect_annuity * EndCap * 10^-3) %>%                             # calculate cost of spur line in thousands of $
  ungroup() %>%                                                                             # ungroup because we want to create new grouping with resource type
  group_by(iter, Policy, Resource.Name, Region) %>%                                         # add resource name to the grouping
  summarise(sum=sum(spur_cost)) %>%                                                         # sum up the spur line cost for each iter, policy and resource type
  ungroup() %>%                                                                             # ungroup 
  group_by(Policy, Resource.Name, Region) %>%                                               # gruop by policy and resource type because now we want to find the iterations with highest spur line cost
  slice(which.min(sum)) %>%                                                                 # find the iteratin with lowest spur line cost
  mutate(condition = "min Spur cost") %>%                                                   # add a column named 'condition' with entries - 'min Spur Cost'
  dplyr::rename(Region_sel = Region) %>%                                                    # rename the 'Region' column as Regin selected
  left_join(dplyr::select(Capacity, iter,Resource.Name, Policy, EndCap, Region)             # now extract the resource capacity from capacity.csv for selected iterations
            %>% dplyr::rename(Resource_Name = Resource.Name))

#join both the dataframes with iterations corresponding to max/min capacity of a give resource type for each region
max_min_iter_z <- dplyr::bind_rows(Spur_cost_min_z, Spur_cost_max_z)

for (R in as.character(unique(max_min_iter_z$Resource.Name))){
  max_min_iter_z_ <- subset(max_min_iter_z, max_min_iter_z$Resource.Name == R & max_min_iter_z$Resource_Name == R)
  ggplot(max_min_iter_z_, aes(x=factor(Region_sel), y=EndCap*0.001, fill=Region))+
    geom_bar(position="stack", stat="identity") + 
    theme_bw()+
    facet_grid(col = vars(condition), row= vars(Policy))+
    theme(text = element_text(size=10), legend.key.size = unit(1, "cm"),axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(x=str_wrap("Selected Region for Min/Max spur line cost", width=60), y="Capacity built in a region (GW)")+
    scale_fill_manual(name = "Region",values = color_list)+
    ggsave(paste("Max_min_SpurLineCost_Capacity_",R,".png"), width=10, height=5, dpi=500)
}





