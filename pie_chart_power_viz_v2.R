
all_inputs = read.csv(file="sample_inputs_pjm.csv", header=TRUE, sep=",", stringsAsFactors = FALSE, na.strings="")
colors <- select(all_inputs,Fuel,Color) %>% distinct()
fuel_list <- as.character(na.omit(all_inputs$Power_Fuel));
power_colors <- filter(colors, Fuel %in% fuel_list);
color_list <- as.character(power_colors$Color);
cases <- na.omit(all_inputs$Interested_Set[all_inputs$Interested_Set!= ""]);
years <- as.character(na.omit(all_inputs$list_years));
resource_mapping <- select(all_inputs, Resource, Fuel);
resource_list <- unique(all_inputs$Fuel);
zone_mapping <- na.omit(select(all_inputs, zone, region)); 
zone_mapping$zone = as.numeric(zone_mapping$zone);
dir.create("Graphics", showWarnings = FALSE)
dir.create("Graphics/power_pie_charts", showWarnings = FALSE)


Power_csv <- read.csv(file="power.csv", sep=",")
Power_csv$Sum <- (Power_csv$Sum) * 10^-6
Power_csv$case <- factor(Power_csv$case,levels = cases)
# Power_csv_temp1 <- subset(Power_csv,Fuel == "ZCF") %>%
#   mutate(Resource = paste(Resource,"_ZCF",sep = ""));
# Power_csv <- rbind(Power_csv_temp1, subset(Power_csv,Fuel != "ZCF"));
# Power_csv <- subset(Power_csv,select = -c(Fuel));
Power_csv <- left_join(Power_csv, resource_mapping)
Power_1 <- aggregate(Sum ~ Fuel+case+year+region, Power_csv, sum)
Power_1 <- mutate(Power_1, Sum = ifelse(Sum < 0, 0, Sum))
Power_1 <- Power_1 %>% plyr::rename(c("region" = "Agg_region","Sum"="value"));
Power_1 <- subset(Power_1, select=c(Agg_region,Fuel,case, year,value)) %>%
  filter(Fuel %in% fuel_list) %>%
  filter(case %in% cases)


# plots one plot for each case and year including intereste regions

  for (j in 1:length(years)) {
    
    power_working <- Power_1 %>%
      filter(year == years[j])
    
    PJM_West_power <- filter(power_working, Agg_region == "PJM_WEST")%>%
      group_by(Agg_region) %>%
      mutate(total = sum(value)) %>%
      ungroup()
    PJM_West_power <- select(PJM_West_power, (order(colnames(PJM_West_power))))
    
    
    PJM_power <- filter(power_working, grepl("PJM", Agg_region))
    PJM_power <- aggregate(value~Fuel+case+year, PJM_power, sum) %>%
      mutate(Agg_region = "Total PJM") %>%
      group_by(Agg_region) %>%
      mutate(total = sum(value)) %>%
      ungroup()
    PJM_power <- select(PJM_power, (order(colnames(PJM_power))))
    
    PJM_total_and_West <- rbind(PJM_West_power, PJM_power)
    PJM_total_and_West$case = 
      factor(PJM_total_and_West$case, levels = cases)
    
     ggplot(PJM_total_and_West, aes(x=total/2, y = value, fill = factor(Fuel, levels = fuel_list), width = total)) +
      geom_bar(position="fill", stat="identity") + 
      facet_grid(case~Agg_region) + 
      coord_polar("y") + 
      #geom_text(aes(label = paste0(round(value), "TWh")))+
      theme_bw()+
      scale_fill_manual(name = "Resource",values = color_list, drop = FALSE)+
      theme(axis.text.x = element_blank()) +
      labs(y= paste0(years[j], "_", cases), x="Annual Energy (TWh)")+
      ggsave(paste0("Graphics/power_pie_charts/", years[j], "_power_PJM_bubble.png"), width=10, height=10, dpi=300)
    
    
    PJM_NJLand_power <- filter(power_working, Agg_region == "PJM_NJLand")%>%
      group_by(Agg_region) %>%
      mutate(total = sum(value)) %>%
      ungroup()
    PJM_NJLand_power <- select(PJM_NJLand_power, (order(colnames(PJM_NJLand_power))))
    
    PJM_NJCoast_power <- filter(power_working, Agg_region == "PJM_NJCoast")%>%
      group_by(Agg_region) %>%
      mutate(total = sum(value)) %>%
      ungroup()
    PJM_NJCoast_power <- select(PJM_NJCoast_power, (order(colnames(PJM_NJCoast_power))))
    
    NJ_power <- filter(power_working, grepl("NJ", Agg_region))
    NJ_power <- aggregate(value~Fuel+case+year, NJ_power, sum) %>%
      mutate(Agg_region = "Total NJ") %>%
      group_by(Agg_region) %>%
      mutate(total = sum(value)) %>%
      ungroup()
    NJ_power <- select(NJ_power, (order(colnames(NJ_power))))
    
    all_NJ <- rbind(PJM_NJLand_power, PJM_NJCoast_power, NJ_power)
    all_NJ$case = 
      factor(all_NJ$case, levels = cases)
    ggplot(all_NJ, aes(x=total/2, y = value, fill = factor(Fuel, levels = fuel_list), width = total)) +
      geom_bar(position="fill", stat="identity") + 
      facet_grid(case~Agg_region) + 
      coord_polar("y") + 
      #geom_text(aes(label = paste0(round(value), "TWh")), position = position_stack(vjust = 0.5))+
      theme_bw()+
      scale_fill_manual(name = "Resource",values = color_list, drop = FALSE)+
      labs(y= paste0(years[j], "_", cases), x="Annual Energy (TWh)")+
      theme(axis.text.x = element_blank()) +
      ggsave(paste0("Graphics/power_pie_charts/", years[j], "_power_NJ_bubble.png"), width=10, height=10, dpi=300)
    
    
  }

PJM_power <- filter(Power_1, grepl("PJM", Agg_region))
PJM_power <- aggregate(value~Fuel+case+year, PJM_power, sum) %>%
  mutate(Agg_region = "Total PJM") %>%
  group_by(Agg_region,case,year) %>%
  mutate(total = sum(value)) %>%
  ungroup()
PJM_power <- select(PJM_power, (order(colnames(PJM_power))))
PJM_power$case <- factor(PJM_power$case,levels = cases);
ggplot(PJM_power, aes(x=total/2, y = value, fill = factor(Fuel, levels = fuel_list), width = total)) +
  geom_bar(position="fill", stat="identity") + 
  facet_grid(case~year) + 
  coord_polar("y") + 
  #geom_text(aes(label = paste0(round(value), "TWh")))+
  theme_bw()+
  scale_fill_manual(name = "Resource",values = color_list, drop = FALSE)+
  theme(axis.text.x = element_blank()) +
  labs(x="Annual Energy (TWh)",y= paste0(" "))+
  ggsave(paste0("Graphics/power_pie_charts/", "total_power_PJM_bubble.png"), width=10, height=10, dpi=300)







