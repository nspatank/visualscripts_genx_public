setwd("/Users/anikamaskara/Desktop/P2X/genx_viz/PJM_results_Sep1/2030/p3_2030_deepdecarbonization/Inputs")

load_csv <- read_csv("Load_data.csv") 
load <- select(load_csv, contains("Load_MW_"))
names <- colnames(load) 

sub_weights <- load_csv$Sub_Weights[1:20]
sub_weights_rep <- rep(sub_weights, each = 96)
  
load <- mutate(load, row_total = rowSums(load[,names]))
load$Sub_Weights_pre <- sub_weights_rep
load <- mutate(load, Total = Sub_Weights_pre * row_total)


total <- sum(load$Total)
total

