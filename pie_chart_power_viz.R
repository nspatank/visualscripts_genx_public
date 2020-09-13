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


resource_mapping <- select(all_inputs, Resource, Fuel)
resource_list <- unique(all_inputs$Fuel)

directory <- na.omit(all_inputs$directory_name)


colors <- select(all_inputs, Fuel, Color) %>%
  distinct()
fuel_list <- as.character(na.omit(all_inputs$Power_Fuel))
power_colors <- filter(colors, Fuel %in% fuel_list)

color_list <- as.character(power_colors$Color)
Interested_Regions <- as.character(na.omit(all_inputs$Interested_Regions))
Deep_Dive <- as.character(na.omit(all_inputs$Deep_Dive))
Total <- as.character(na.omit(all_inputs$Total))
Total_title <- as.character(na.omit(all_inputs$Total_title))

cases <- na.omit(all_inputs$Interested_Set[all_inputs$Interested_Set!= ""])
years <- as.character(na.omit(all_inputs$list_years))


  

dir.create(directory, showWarnings = FALSE)
dir.create((paste0(directory, "/power_pie_charts")), showWarnings = FALSE)

library(reshape)
#read csv files
Power_csv <- read.csv(file="power.csv", sep=",")

#keep only resource, zone and total power generation from power_csv file
Power_Sum <- subset(Power_csv , Resource=="Sum" | Resource == "Zone")
rownames(Power_Sum) <- seq(length=nrow(Power_Sum))


# #reshape the Power_sum so that we can plot graphs easiy
Power <- melt(Power_Sum, id=c("Resource","case","year"))
zones <- filter(Power, Resource == "Zone")
Zone <- zones$value
Power <- filter(Power, Resource == "Sum")
Power[is.na(Power)] <- 0
Power <- cbind(Power, Zone)


Power$value <- Power$value * 10^-6
Power$case <- factor(Power$case)

Power <- Power %>%
  separate(variable, into = c("fuel", "index"), "[.]")

Power <- left_join(Power, resource_mapping, by = c("fuel" = "Resource"))


Power_1 <- subset(Power, select=c(Fuel,value,case, year,Zone))
Power_1 <- aggregate(. ~ Fuel+case+year+Zone, Power_1, sum)
Power_1 <- mutate(Power_1, value = ifelse(value < 0, 0, value))


zone_mapping <- na.omit(select(all_inputs, zone, region)) 
zone_mapping$zone = as.numeric(zone_mapping$zone)
Power_1 <- left_join(Power_1, zone_mapping, by = c("Zone" = "zone")) %>%
  plyr::rename(c("region" = "Agg_region"))

Power_1 <- subset(Power_1, select=c(Agg_region,Fuel,case, year,value)) %>%
  filter(Fuel %in% fuel_list) %>%
  filter(case %in% cases) %>%
  filter(year %in% years)


# plots one plot for each case and year including intereste regions
for (i in 1:length(cases)) {
  for (j in 1:length(years)) {
    
    power_working <- Power_1 %>%
      filter(case == as.character(cases[i])) %>%
      filter(year == as.character(years[j]))
    
    power_primary <- power_working %>%
      group_by(Agg_region) %>%
      mutate(total = sum(value)) %>%
      ungroup() %>%
      filter(Agg_region %in% Interested_Regions)
    
    
    ggplot(power_primary, aes(x=total/2, y = value, fill = factor(Fuel, levels = fuel_list), width = total)) +
      geom_bar(position="fill", stat="identity") + 
      facet_wrap(~Agg_region) + 
      coord_polar("y") + 
      scale_fill_manual(name = "Resource",values = color_list, drop = FALSE)+
      theme(axis.text.x = element_blank()) +
      labs(y= paste0(years[j], "_", cases[i]), x="Annual Energy (TWh)")+
      ggsave(paste0(directory, "/power_pie_charts/", cases[i], years[j], "_power_interested_set_bubble.png"), width=10, height=10, dpi=300)
    
    ## set of regions next to a total
    
    individual_regions <- filter(power_working, Agg_region %in% Deep_Dive)%>%
      group_by(Agg_region) %>%
      mutate(total = sum(value)) %>%
      ungroup()
    individual_regions <- select(individual_regions, (order(colnames(individual_regions))))
    
    total_region <- filter(power_working, Agg_region %in% Total)
    total_region <- aggregate(value~Fuel+case+year, total_region, sum)%>%
      mutate(Agg_region = Total_title) %>%
      group_by(Agg_region) %>%
      mutate(total = sum(value)) %>%
      ungroup()
    total_region <- select(total_region, (order(colnames(total_region))))
    
    combined_total <- rbind(individual_regions, total_region)
    
    ggplot(combined_total, aes(x=total/2, y = value, fill = factor(Fuel, levels = fuel_list), width = total)) +
      geom_bar(position="fill", stat="identity") + 
      facet_wrap(~Agg_region) + 
      coord_polar("y") + 
      scale_fill_manual(name = "Resource",values = color_list, drop = FALSE)+
      theme(axis.text.x = element_blank()) +
      labs(y= paste0(years[j], "_", cases[i]), x="Annual Energy (TWh)")+
      ggsave(paste0(directory, "/power_pie_charts/", cases[i], years[j], Total_title,"_with_deep_dive_bubble.png"), width=10, height=2.5, dpi=300)
    
    
    ####
    
  
    
  }
}

# plots one region across all cases and years
for(region in Deep_Dive) {
  temp <- filter(Power_1, Agg_region == region)%>%
    group_by(case, year) %>%
    mutate(total = sum(value)) %>%
    ungroup()
 
  
  temp$case = factor(temp$case, levels = cases)
  ggplot(temp, aes(x=total/2, y = value, fill = factor(Fuel, levels = fuel_list), width = total)) +
    geom_bar(position="fill", stat="identity") + 
    facet_grid(rows = vars(case), cols = vars(year)) + 
    coord_polar("y") + 
    scale_fill_manual(name = "Resource",values = color_list, drop = FALSE)+
    labs(y= region , x="Annual Energy (TWh)")+
    theme(axis.text.x = element_blank()) +
    ggsave(paste0(directory, "/power_pie_charts/",region,  "_all_cases.png"), width=10, height=10, dpi=300)
  
}




## plots total of chosen regions across all cases and years
total_all <- filter(Power_1, Agg_region %in% Total)

total_all <- aggregate(value~Fuel+case+year, total_all, sum) %>%
  group_by(case, year) %>%
  mutate(total = sum(value)) %>%
  ungroup()

total_all$case = 
  factor(total_all$case, levels = cases)
ggplot(total_all, aes(x=total/2, y = value, fill = factor(Fuel, levels = fuel_list), width = total)) +
  geom_bar(position="fill", stat="identity") + 
  facet_grid(rows = vars(case), cols = vars(year)) + 
  coord_polar("y") + 
  scale_fill_manual(name = "Resource",values = color_list, drop = FALSE)+
  labs(y= "total", x="Annual Energy (TWh)")+
  theme(axis.text.x = element_blank()) +
  ggtitle(Total_title) +
  ggsave(paste0(directory, "/power_pie_charts/", Total_title,  "_total_all_cases.png"), width=10, height=10, dpi=300)



