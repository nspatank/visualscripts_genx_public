#---0. Pkg loading---------------------------------------------
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
library(RColorBrewer)
library(hrbrthemes)
#---1. ADMIN - Include libraries and other important-----------
setwd("/Users/qingyuxu/Documents/PJM_QX_2030")
codepath <- "/Users/qingyuxu/Documents/Graphic Rscripts/";
path <- "/Users/qingyuxu/Documents/PJM_QX_2030"
outpath <- path

all_inputs = read.csv(file="sample_inputs_pjm.csv", header=TRUE,na.strings="")
ordered_set <- na.omit(all_inputs$Interested_Set[all_inputs$Interested_Set!= ""])

resource_mapping <- select(all_inputs, Resource, Fuel)
resource_list <- unique(all_inputs$Fuel)


Interested_Regions <- as.character(na.omit(all_inputs$Interested_Regions))
Deep_Dive <- as.character(na.omit(all_inputs$Deep_Dive))
Total <- as.character(na.omit(all_inputs$Total))
Total_title <- as.character(na.omit(all_inputs$Total_title))
cases <- na.omit(all_inputs$Interested_Set[all_inputs$Interested_Set!= ""])
years <- as.character(na.omit(all_inputs$list_years))
#---2. Combining Result files----------------------------------
source(paste0(codepath,"Post_process_resultsvPJM.R"));
#note that this depends on the naming of the cases.
#if each period has 4 days, then the factor should be 96 = 24*4.
#don't divide if your load sums are already weighed correctly!
#---4. Pie-Charts-------------------
source(paste0(codepath,"pie_chart_power_viz_v2.R"));
#---5. Capacity Bar Chart-------------------
source(paste0(codepath,"yearbyyearcomp.R"));
#---6. Total Cost and Emission----------------
source(paste0(codepath,"cost_emission.R"));
#---7. Map-----------------
source(paste0(codepath,"map_plot_transmission_pjm.R"));
#---8. Prices--------------
source(paste0(codepath,"RPS_ResMar_prices.R"));
#---9. Settlement----------
source(paste0(codepath,"Settlement.R"));
