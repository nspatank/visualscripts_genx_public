
all_inputs = read.csv(file="sample_inputs_pjm.csv", header=TRUE, sep=",", stringsAsFactors = FALSE, na.strings="")
resource_mapping <- select(all_inputs, Resource, Fuel)
capacity_resource_levels <- as.character(unique(all_inputs$Fuel))
power_resource_levels <- as.character(all_inputs$Power_Fuel)

colors <- select(all_inputs, Fuel, Color) %>% distinct()
power_colors <- filter(colors, Fuel %in% power_resource_levels)
capacity_resource_colors <- as.character(colors$Color)
power_resource_colors<- as.character(power_colors$Color)

zone_mapping <- na.omit(select(all_inputs, zone, region)) 
zone_mapping$zone = as.factor(zone_mapping$zone)

All_Set <- na.omit(all_inputs$All_Set[all_inputs$All_Set!= ""])
Interested_Set <- na.omit(all_inputs$Interested_Set[all_inputs$Interested_Set!= ""])

ordered_set <- Interested_Set
years_all <- c("2020",as.character(na.omit(all_inputs$list_years)))

Total <- as.character(na.omit(all_inputs$Total))
Total_title <- as.character(na.omit(all_inputs$Total_title))
Total_2 <- as.character(na.omit(all_inputs$Total_2))
Total_2_title <- as.character(na.omit(all_inputs$Total_2_title))

# create relevant folders (no power bar charts for now)
dir.create("Graphics", showWarnings = FALSE)
dir.create("Graphics/capacity_bar/", showWarnings = FALSE)

#read in capacity csv and inputs, extract relevant settings
Capacity_1 <- read.csv("capacity.csv")
#modify data for ZCF
# Capacity_1_temp1 <- subset(Capacity_1,Fuel == "ZCF") %>%
#   mutate(Resource = paste(Resource,"_ZCF",sep = ""));
# Capacity_1 <- rbind(Capacity_1_temp1, subset(Capacity_1,Fuel != "ZCF"));
# Capacity_1 <- subset(Capacity_1,select = -c(Fuel));
Capacity_1$EndCap <- Capacity_1$EndCap * 0.001
Capacity_1$Zone <- as.factor(Capacity_1$Zone);
## CAPACITY
Capacity_1 <- left_join(Capacity_1, resource_mapping )
Capacity_1 <- left_join(Capacity_1, zone_mapping, by = c("Zone" = "zone")) %>%
  plyr::rename(c("region" = "Agg_region"))


Capacity_res_z <- aggregate(EndCap~Agg_region+Fuel+case+year, Capacity_1, sum, na.rm = na.omit)
Interested_Regions <- as.character(na.omit(all_inputs$Interested_Regions))
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
    labs(x=str_wrap(All_Set[k], width=40), y="Capacity (GW)")+
    scale_fill_manual(name = "Resource",values = color_a)+
    scale_linetype_manual(name = "", values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("black")))) + 
    scale_x_discrete(limits = unique(Capacity_reg$year), labels = years_all) +
    ggsave(paste0("Graphics/capacity_bar/", All_Set[k], "_Capacity_interested_region_case.png"), width=6, height=6, dpi=300)
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
  ggsave(paste0("Graphics/capacity_bar/", region, "_Capacity_all_case.png"), width=5, height=4, dpi=300)




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
    # geom_hline(data= Peak_data_reg, aes(yintercept=max_val, linetype = "Peak demand"), color = "black")+
    scale_linetype_manual(name = "", values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("black")))) + 
    scale_x_discrete(limits = unique(Capacity_reg$year), labels = years_all) +
    ggtitle(Total_title)+
    ggsave(paste0("Graphics/capacity_bar/", Total_title, "_aggregate_all_case.png"), width=5, height=6, dpi=300)
  
  Capacity_reg <- filter(Capacity_res_z, Agg_region %in% Total_2)%>%
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
    # geom_hline(data= Peak_data_reg, aes(yintercept=max_val, linetype = "Peak demand"), color = "black")+
    scale_linetype_manual(name = "", values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("black")))) + 
    scale_x_discrete(limits = unique(Capacity_reg$year), labels = years_all) +
    ggtitle(Total_2_title)+
    ggsave(paste0("Graphics/capacity_bar/", Total_2_title, "_aggregate_all_case.png"), width=5, height=6, dpi=300)
  
}
  
