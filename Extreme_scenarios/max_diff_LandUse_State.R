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

A <- summarized_Raw_data %>% 
  group_by(unique,state) %>% 
  summarize(sum = sum(AreaNorm)) %>%
  ungroup() %>%
  group_by(unique) %>%
  summarize(max = max(sum)) %>% arrange(max)

summarized_Raw_data$unique <- factor(summarized_Raw_data$unique,levels = unique(A$unique))

ggplot(summarized_Raw_data,aes(x=state, y=unique, fill=AreaNorm))+
  geom_tile(color="white", size=.5)+
  facet_grid(~tech, scales="free_y")+
  theme_bw()+
  ggtitle("Land use impact for states in WECC under different criteria")+
  scale_fill_gradient2(high = "maroon")+
  labs(x=str_wrap("States in WECC", width=40), y="WECC-wide Criteria")+
  theme(text = element_text(size=10), legend.key.size = unit(1, "cm"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("rect", xmin = 0, xmax = 12, ymin = 0.5, ymax = 1.5, color="black", alpha=0, size=0.1)+
  annotate("text",x = 10, y = 1,label="Least impact", size=2)+
  annotate("rect", xmin = 0, xmax = 12, ymin = 12.5, ymax = 13.5, color="black", alpha=0, size=0.1)+
  annotate("text",x = 10, y = 13,label="Most impact", size=2)+
  ggsave("LandArea_States.png", width=6, height=5, dpi=500)
  



