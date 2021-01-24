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
color_list <- c("#66CCCC","#A6CEE3","#fccb6e", "#92CF72", "#33A02C","#408DBF", "#ff7700", "#FFDAB9", "#E94330","#DDA0DD", "#cd51e0")

# read the CSV file for all MGA iterations
Network <- read.csv(file="../data/Network_results.csv", header=TRUE, sep=",")
Network$New_Trans_Capacity <- Network$New_Trans_Capacity * 0.001
Network_data <- read.csv(file="../data/Network.csv", header=TRUE, sep=",") %>%
  dplyr::select(Network_lines, Transmission.Path.Name, Line_Max_Reinforcement_MW) %>%
  dplyr::rename(Line = Network_lines, Max_Cap = Line_Max_Reinforcement_MW) %>%
  mutate(Max_Cap = Max_Cap*0.001)

#WECC-WIDE MAX/MIN transmission capacity- AGGREGATED PLOT
#Select iteration with maximum capacity for each Resource type defined above
iter_max_cap <- dplyr::select(Network, iter, New_Trans_Capacity, Policy) %>%
  group_by(iter,Policy) %>%
  summarise(sum=sum(New_Trans_Capacity)) %>%
  ungroup() %>%
  group_by(Policy) %>%
  slice(which.max(sum)) %>%
  mutate(condition = "Max Cap") %>%
  ungroup() %>%
  left_join(dplyr::select(Network, Line, iter, New_Trans_Capacity, Policy)) %>%
  right_join(Network_data)

#Select iteration with minimum capacity for each Resource type defined above
iter_min_cap <- dplyr::select(Network, iter, New_Trans_Capacity, Policy) %>%
  group_by(iter,Policy) %>%
  summarise(sum=sum(New_Trans_Capacity)) %>%
  ungroup() %>%
  group_by(Policy) %>%
  slice(which.min(sum)) %>%
  mutate(condition = "Min Cap") %>%
  ungroup() %>%
  left_join(dplyr::select(Network, Line, iter, New_Trans_Capacity, Policy)) %>%
  right_join(Network_data)

#join both the dataframes with iterations corresponding to max/min capacity of a give resource type
max_min_iter <- dplyr::bind_rows(iter_max_cap, iter_min_cap)
write.csv(distinct(max_min_iter,iter, Policy,condition), "../result_data/max_min_trans.csv",row.names = FALSE)

ggplot(max_min_iter, aes(x=Transmission.Path.Name, y=New_Trans_Capacity))+
  geom_bar(position="stack", stat="identity", fill="grey") + 
  geom_point(aes(y=Max_Cap), size=0.5)+
  theme_bw()+
  facet_grid(col = vars(condition), row= vars(Policy))+
  theme(text = element_text(size=8), legend.key.size = unit(1, "cm"),axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x=str_wrap("Line number", width=40), y="Capacity (GW)")+
  scale_fill_manual(values = "light grey")+
  ggsave("Max_min_Trans_Capacity.png", width=8, height=6, dpi=500)

#LINE SPECIFIC MAX/MIN 
#Select iteration with maximum capacity for each Resource type defined above by Region
iter_max_cap_z <- dplyr::select(Network, iter, Line, New_Trans_Capacity, Policy) %>%
  group_by(Line, Policy) %>%
  slice(which.max(New_Trans_Capacity)) %>%
  mutate(condition = "Max Line Cap") %>%
  ungroup() %>%
  dplyr::rename(Line_sel = Line, New_Cap = New_Trans_Capacity) %>%
  left_join(dplyr::select(Network, Line, iter, New_Trans_Capacity, Policy, Cost_Trans_Capacity)) %>%
  right_join(Network_data)

#Select iteration with minimum capacity for each Resource type defined above
iter_min_cap_z <- dplyr::select(Network, iter, Line, New_Trans_Capacity, Policy) %>%
  group_by(Line, Policy) %>%
  slice(which.min(New_Trans_Capacity)) %>%
  mutate(condition = "Min Line Cap") %>%
  ungroup() %>%
  dplyr::rename(Line_sel = Line, New_Cap = New_Trans_Capacity) %>%
  left_join(dplyr::select(Network, Line, iter, New_Trans_Capacity, Policy, Cost_Trans_Capacity)) %>%
  right_join(Network_data)

#join both the dataframes with iterations corresponding to max/min capacity of a give resource type for each region
max_min_iter_z <- dplyr::bind_rows(iter_max_cap_z, iter_min_cap_z)
write.csv(distinct (max_min_iter_z,iter, Policy, Line_sel,condition), "../result_data/max_min_trans_z.csv",row.names = FALSE)

ggplot(max_min_iter_z, aes(x=factor(Line_sel), y=New_Trans_Capacity, fill=Transmission.Path.Name))+
  geom_bar(position="stack", stat="identity") + 
  theme_bw()+
  facet_grid(col = vars(condition), row= vars(Policy))+
  theme(text = element_text(size=10), legend.key.size = unit(1, "cm"))+
  labs(x=str_wrap("Max/Min capacity of Line N", width=40), y="Capacity (GW)")+
  scale_fill_manual(name = "Lines",values = color_list)+
  ggsave("Max_min_Line_Capacity.png", width=10, height=5, dpi=500)

ggplot(max_min_iter_z, aes(x=Transmission.Path.Name, y=Cost_Trans_Capacity*10^-6))+
  geom_point(color="blue", size=0.5)+
  theme_bw()+
  facet_grid(col = vars(condition), row= vars(Policy))+
  theme(text = element_text(size=8), legend.key.size = unit(1, "cm"),axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x=str_wrap("Line", width=40), y="Cost ($ Million)")+
  ggsave("Max_min_Line_Cost.png", width=10, height=5, dpi=500)




