library(tidyverse)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(RSQLite)
library(gridExtra)
library(dplyr)
library(reshape)
setwd("/Users/anikamaskara/Desktop/P2X/genx_viz/PJM_results_Sep1")
all_inputs = read.csv(file="sample_inputs_pjm.csv", header=TRUE, sep=",", stringsAsFactors = FALSE, na.strings="")

#read in capacity csv and inputs, extract relevant settings
Capacity_1 <- read_csv("capacity.csv")

resource_mapping <- select(all_inputs, Resource, Fuel)

capacity_resource_levels <- as.character(unique(all_inputs$Fuel))
power_resource_levels <- as.character(all_inputs$Power_Fuel)

colors <- select(all_inputs, Fuel, Color) %>%
  distinct()

power_colors <- filter(colors, Fuel %in% power_resource_levels)


capacity_resource_colors <- as.character(colors$Color)
power_resource_colors<- as.character(power_colors$Color)

Interested_Regions <- as.character(na.omit(all_inputs$Interested_Regions))
Deep_Dive <- as.character(na.omit(all_inputs$Deep_Dive))
Total <- as.character(na.omit(all_inputs$Total))
Total_title <- as.character(na.omit(all_inputs$Total_title))
directory <- na.omit(all_inputs$directory_name)

# create relevant folders 

dir.create(directory, showWarnings = FALSE)
dir.create((paste0(directory, "/capacity_bar")), showWarnings = FALSE)




All_Set <- na.omit(all_inputs$All_Set[all_inputs$All_Set!= ""])
Interested_Set <- na.omit(all_inputs$Interested_Set[all_inputs$Interested_Set!= ""])

ordered_set <- Interested_Set
years_all <- as.character(na.omit(all_inputs$list_years))

## CAPACITY

Capacity_1$EndCap <- Capacity_1$EndCap * 0.001
Capacity_1 <- head(Capacity_1,-1)
Capacity_1 = Capacity_1[!Capacity_1$Resource =="Total",]

Capacity_1 <- left_join(Capacity_1, resource_mapping )


zone_mapping <- na.omit(select(all_inputs, zone, region)) 
zone_mapping$zone = as.factor(zone_mapping$zone)
Capacity_1 <- left_join(Capacity_1, zone_mapping, by = c("Zone" = "zone")) %>%
  plyr::rename(c("region" = "Agg_region"))


Capacity_res_z <- aggregate(EndCap~Agg_region+Fuel+case+year, Capacity_1, sum, na.rm = na.omit) %>%
  filter(year %in% years_all)

# one set of bar charts for each case across all years
for (k in 1:length(Interested_Set)){
  Capacity_reg <- subset(Capacity_res_z, as.character(Capacity_res_z$case)==Interested_Set[k]) %>%
    filter(Agg_region %in% Interested_Regions)
  
 
  color_a <- capacity_resource_colors
  Capacity_reg$year <- as.factor(Capacity_reg$year)
  
  ggplot(Capacity_reg , aes(x=year, y=EndCap, fill=factor(Fuel, levels = capacity_resource_levels)))+
    geom_col() + 
    theme_bw()+
    facet_wrap(~Agg_region, nrow = 4, scales = "free")+
    theme(text = element_text(size=6), axis.text.x = element_text(size=6),legend.key.size = unit(0.5, "cm"))+
    labs(x=str_wrap(Interested_Set[k], width=40), y="Capacity (GW)")+
    scale_fill_manual(name = "Resource",values = color_a)+
    scale_linetype_manual(name = "", values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("black")))) + 
    scale_x_discrete(limits = unique(Capacity_reg$year), labels = years_all) +
    ggsave(paste0(directory, "/capacity_bar/", All_Set[k], "_Capacity_interested_region_case.png"), width=5, height=6, dpi=300)
}





## shows one region across all cases and years
for(region in Deep_Dive) {
  
Capacity_reg <- filter(Capacity_res_z,Agg_region == region)%>%
  filter(case %in% Interested_Set)

color_a <- capacity_resource_colors

Capacity_reg$case = factor(Capacity_reg$case, levels = ordered_set)
Capacity_reg$year <- as.factor(Capacity_reg$year)
ggplot(Capacity_reg , aes(x=year, y=EndCap, fill=factor(Fuel, levels = capacity_resource_levels)))+
  geom_col() + 
  theme_bw()+
  facet_wrap(~case)+
  theme(text = element_text(size=6), axis.text.x = element_text(size=6),legend.key.size = unit(0.5, "cm"))+
  labs(y="Capacity (GW)")+
  scale_fill_manual(name = "Resource",values = color_a, drop = FALSE)+
  scale_linetype_manual(name = "", values = c(2, 2), 
                        guide = guide_legend(override.aes = list(color = c("black")))) + 
  scale_x_discrete(limits = unique(Capacity_reg$year), labels = years_all) +
  ggtitle(region) +
  ggsave(paste0(directory, "/capacity_bar/", region, "_Capacity_all_case.png"), width=5, height=6, dpi=300)

  

}


## aggregate total of relevant regions across all cases and years
  Capacity_reg <- filter(Capacity_res_z, Agg_region %in% Total)%>%
    filter(case %in% Interested_Set)
  Capacity_reg <- aggregate(EndCap~Fuel+case+year, Capacity_reg, sum)
  resource_a <- unique(Capacity_reg$Fuel)
  color_a <- capacity_resource_colors
  
  
  Capacity_reg$case = factor(Capacity_reg$case, levels = ordered_set)
  Capacity_reg$year <- as.factor(Capacity_reg$year)
  ggplot(Capacity_reg , aes(x=year, y=EndCap, fill=factor(Fuel, levels = capacity_resource_levels)))+
    geom_col() + 
    theme_bw()+
    facet_wrap(~case)+
    theme(text = element_text(size=6), axis.text.x = element_text(size=6),legend.key.size = unit(0.5, "cm"))+
    labs(y="Capacity (GW)")+
    scale_fill_manual(name = "Resource",values = color_a, drop = FALSE)+
    scale_linetype_manual(name = "", values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("black")))) + 
    scale_x_discrete(limits = unique(Capacity_reg$year), labels = years_all) +
    ggtitle(Total_title)+
    ggsave(paste0(directory, "/capacity_bar/", Total_title, "_aggregate_all_case.png"), width=5, height=6, dpi=300)
  
  
