#----0. Set up library-------
library(dplyr) 
library(ggplot2) # plots
library(stringr)
library(RColorBrewer) # color pallettes
library(RSQLite) #Database
library(maps)
library(gridExtra)
library(dplyr)
library(reshape)
library(rgdal)
library(ggrepel)
library(ggmap)
library(tidyverse)
library(viridis)

#---1. ADMIN - libraries and other important-----------
setwd("/Users/qingyuxu/Documents/PJM_QX")
codepath <- "/Users/qingyuxu/Documents/Graphic Rscripts/";
path <- "/Users/qingyuxu/Documents/PJM_QX"
outpath <- path

#---2. Prepare: Read Inupt Setting----
all_inputs = read.csv(file="sample_inputs_pjm.csv", header=TRUE,na.strings="")
resource_mapping <- select(all_inputs, Resource, Fuel)
resource_list <- unique(all_inputs$Fuel);
fuel_list <- as.character(na.omit(all_inputs$Power_Fuel));
capacity_resource_levels <- as.character(unique(all_inputs$Fuel))
power_resource_levels <- as.character(all_inputs$Power_Fuel)
colors <- select(all_inputs, Fuel, Color) %>% distinct()
color_list <- as.character(power_colors$Color);
power_colors <- filter(colors, Fuel %in% power_resource_levels)
capacity_resource_colors <- as.character(colors$Color)
power_resource_colors<- as.character(power_colors$Color)
zone_mapping <- na.omit(select(all_inputs, zone, region)) 
zone_mapping$zone = as.factor(zone_mapping$zone)
All_Set <- na.omit(all_inputs$All_Set[all_inputs$All_Set!= ""])
Interested_Set <- na.omit(all_inputs$Interested_Set[all_inputs$Interested_Set!= ""])
Interested_Regions <- as.character(na.omit(all_inputs$Interested_Regions));
ordered_set <- Interested_Set
years_all <- c(na.omit(all_inputs$start_year),as.character(na.omit(all_inputs$list_years)))
start_year <- na.omit(all_inputs$start_year);
Total <- as.character(na.omit(all_inputs$Total))
Total_title <- as.character(na.omit(all_inputs$Total_title))
Total_2 <- as.character(na.omit(all_inputs$Total_2))
Total_2_title <- as.character(na.omit(all_inputs$Total_2_title))

Deep_Dive <- as.character(na.omit(all_inputs$Deep_Dive))
cases <- na.omit(all_inputs$Interested_Set[all_inputs$Interested_Set!= ""])
years <- as.character(na.omit(all_inputs$list_years))





sep = "/"
# Lists all files
allfiles = list.files(recursive = TRUE)
#Readfilepath
all_generators_data <- allfiles[grep("/Generators_data.csv", allfiles)];
all_power = allfiles[grep("/power.csv", allfiles)]; 
#This is to find where the "case" is in the string
find_case = which(strsplit(allfiles[[1]], "")[[1]]=="/") 
#This is to find where the "year" is in the string
find_year = which(strsplit(allfiles[[1]], "")[[1]]=="_")


#--------------------------------#
#     Combining Power results    #
#--------------------------------#

power = t(read.csv(paste0(path,"/",all_power[[1]]),header = F)[1:3,]);
powercolnames <- power[1,];
power <- power[-c(1,length(power[,1])),];
colnames(power) <- powercolnames;
power <- as.data.frame(power);
t_generators <- read.csv(paste0(path,"/",all_generators_data[[1]]));
power <- cbind(power,t_generators$Fuel);
colnames(power)[dim(power)[2]] <- "Fuel";
power$case = substr(all_power[[1]], find_year[2] + 1, find_case[2] - 1)
power$year = substr(all_power[[1]],1, 4)

if (length(all_power)>1)
{
  for (f in 2:length(all_power)){
    temp_power = t(read.csv(paste0(path,"/",all_power[[f]]),header = F)[1:3,]);
    powercolnames <- temp_power[1,];
    temp_power <- temp_power[-c(1,length(temp_power[,1])),];
    colnames(temp_power) <- powercolnames;
    temp_power <- as.data.frame(temp_power);
    t_generators <- read.csv(paste0(path,"/",all_generators_data[[f]]));
    temp_power <- cbind(temp_power,t_generators$Fuel);
    colnames(temp_power)[dim(temp_power)[2]] <- "Fuel";
    temp_power$case = substr(all_power[[f]], find_year[2] + 1, which(strsplit(all_power[[f]], "")[[1]]=="/")[2] - 1)
    temp_power$year = substr(all_power[[f]],1, 4)
    power <- bind_rows(power, temp_power)
  }
}
power$Sum <- as.numeric(power$Sum);
power <- aggregate(Sum~case+year+Zone+Resource+Fuel,power,sum);

for (i in 1:length(power[,1]))
{
  power$region[i] <- zone_mapping$region[as.numeric(power$Zone[i])];
}
power_for_settlement <- power;

power_temp1 <- subset(power,Fuel == "ZCF") %>%
  mutate(Resource = paste(Resource,"_ZCF",sep = ""));
power <- rbind(power_temp1, subset(power,Fuel != "ZCF"));
power <- subset(power,select = -c(Fuel));

write.csv(power, paste0(outpath, sep, "power.csv"), row.names = FALSE);

#--------------------------------#
#-------Plotting-----------------#
#--------------------------------#


Power_csv <- read.csv(file="power.csv", sep=",")
Power_csv$Sum <- (Power_csv$Sum) * 10^-6
Power_csv$case <- factor(Power_csv$case,levels = cases)
Power_csv <- left_join(Power_csv, resource_mapping)
Power_1 <- aggregate(Sum ~ Fuel+case+year+region, Power_csv, sum)
Power_1 <- mutate(Power_1, Sum = ifelse(Sum < 0, 0, Sum))
Power_1 <- Power_1 %>% plyr::rename(c("region" = "Agg_region","Sum"="value"));
Power_1 <- subset(Power_1, select=c(Agg_region,Fuel,case, year,value)) %>%
  filter(Fuel %in% fuel_list) %>%
  filter(case %in% cases)


# plots one plot for each case and year including intereste regions

for (j in 1:length(cases)) {
    power_working <- Power_1 %>%
      filter(case == cases[j]) %>%
      group_by(Agg_region) %>%
      mutate(total = sum(value)) %>%
      ungroup();
     ggplot(power_working, aes(x=year, y = value, fill = factor(Fuel, levels = fuel_list))) +
      geom_col() + 
      facet_wrap(Agg_region~.,scales = "free_y") + 
      #geom_text(aes(label = paste0(round(value), "TWh")))+
      theme_bw()+
      scale_fill_manual(name = "Resource",values = color_list, drop = FALSE)+
      theme(axis.text.x = element_blank()) +
      labs(y="Annual Energy (TWh)")+
      ggsave(paste0("Graphics/power_pie_charts/", years[j], "_power_bubble.png"), width=10, height=10, dpi=300)
}







