


# PJM+ regions
pjmplus <- c("MISO_Central", "MISO_East", "NY_East", "NY_West", "PJM_Dom","PJM_COMD", 
             "PJM_Delaware", "PJM_NJCoast", "PJM_NJLand", "PJM_PECO", "PJM_SMAC", "PJM_West", 
             "TVA", "VACA", "PJM_WMAC")

dir.create("Graphics", showWarnings = FALSE)
dir.create("Graphics/transmission", showWarnings = FALSE)

#read in IPM to PJM mapping and filter to relevant rows and columns
pjm_zones <- read.csv("PJM_zones_final.csv")
pjm_zones <- filter(pjm_zones, GenX.Region %in% pjmplus)
pjm_zones <- select(pjm_zones, -c("State", "County", "IPM.Region"))

# filter to PJM and neighboring regions
EERDWN <- read.csv("EERDWN_withIPM.csv")


EERDWN.pjm <- subset(EERDWN, region %in% c("wisconsin", "iowa", "missouri", "illinois", "mississippi",
                                     "tennessee", "alabama", "georgia", "north carolina", 
                                     "south carolina", "michigan", "indiana", "kentucky", "ohio",
                                     "virginia", "west virginia", "maryland", "delaware",
                                     "pennsylvania", "new jersey", "new york", "vermont", 
                                     "connecticut", "massachusetts", "rhode island", "new hampshire", 
                                     "maine", "arkansas", "iowa", "minnesota"))

# # Define new columns 

# join county mapping data with EERDWN to assign GenX Regions
EERDWN.pjm <- left_join(EERDWN.pjm, pjm_zones, by = c("fips"= "County.FIPS.5"))

# # Define new columns 
EERDWN.pjm$center_lat <- NA
EERDWN.pjm$center_long <- NA

#rename PJM_Delaware, TVA, VACA, and PJM_West to match load data
EERDWN.pjm$GenX.Region <- as.character(EERDWN.pjm$GenX.Region)

EERDWN.pjm[!is.na(EERDWN.pjm$GenX.Region) & EERDWN.pjm$GenX.Region == "TVA",]$GenX.Region <- "SC_TVA"
EERDWN.pjm[!is.na(EERDWN.pjm$GenX.Region) & EERDWN.pjm$GenX.Region == "VACA",]$GenX.Region <- "SC_VACA"
EERDWN.pjm[!is.na(EERDWN.pjm$GenX.Region) & EERDWN.pjm$GenX.Region == "MISO_Central",]$GenX.Region <- "MIS_Central"
EERDWN.pjm[!is.na(EERDWN.pjm$GenX.Region) & EERDWN.pjm$GenX.Region == "MISO_East",]$GenX.Region <- "MIS_East"
EERDWN.pjm[!is.na(EERDWN.pjm$GenX.Region) & EERDWN.pjm$GenX.Region == "PJM_WMAC",]$GenX.Region <- "PJM_WestMAC"
EERDWN.pjm[!is.na(EERDWN.pjm$GenX.Region) & EERDWN.pjm$GenX.Region == "PJM_West",]$GenX.Region <- "PJM_WEST"


#convert GenX.Region to factor
EERDWN.pjm$GenX.Region <- as.factor(EERDWN.pjm$GenX.Region)

#redefine GenX regions vector to match updated names
pjmplus <- c("MIS_Central", "MIS_East", "NY_East", "NY_West", "PJM_Dom","PJM_COMD", 
             "PJM_Delaware", "PJM_NJCoast", "PJM_NJLand", "PJM_PECO", "PJM_SMAC", "PJM_WEST", 
             "SC_TVA", "SC_VACA", "PJM_WestMAC")


#define new color pallete function
getPalette = colorRampPalette(brewer.pal(12, "Paired"))


# create base map
pjm_base <- ggplot(data = EERDWN.pjm, mapping = aes(x = long, y = lat, fill=GenX.Region, group = group)) + 
  coord_quickmap() +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank()) +
  scale_fill_manual(values = getPalette(15),na.value = "gray88") +
  scale_color_manual(values = getPalette(15),na.value = "gray88") +
  theme(legend.position="left") +
  geom_polygon(aes(color = GenX.Region))



#calculate centroid of each region
for(i in pjmplus){
  # print(i)
  long.lad <- filter(EERDWN.pjm, GenX.Region == i)
  long.lad <- select(long.lad, long, lat)
  x <- mean(long.lad$lon)
  y <- mean(long.lad$lat)
  EERDWN.pjm[!is.na(EERDWN.pjm$GenX.Region) & EERDWN.pjm$GenX.Region == i,]$center_long <- x
  EERDWN.pjm[!is.na(EERDWN.pjm$GenX.Region) & EERDWN.pjm$GenX.Region == i,]$center_lat <- y
  
}
pjm.centroids <- data.frame(zone <- unique(EERDWN.pjm$GenX.Region), 
                            lat <- unique(EERDWN.pjm$center_lat), 
                            long <- unique(EERDWN.pjm$center_long))
pjm.centroids <- na.omit(pjm.centroids)
colnames(pjm.centroids) = c("zone", "center_lat", "center_long")

##transmission_mapping


#extract 2030 maximum flow for each line
network <- read_csv("network.csv")%>%
  select(`Transmission Path Name`, `Line_Max_Reinforcement_MW`) %>%
  mutate(Line = 1:length(`Transmission Path Name`)) %>%
  separate(`Transmission Path Name`, c("start", "end"), sep = "_to_")


transmission <- read_csv("trans.csv") %>%
  select(-Cost_Trans_Capacity) %>%
  left_join(network) %>%
  mutate(New_Trans_Capacity = abs(New_Trans_Capacity))


cases <- unique(transmission$case)
years <- unique(transmission$year)

#find cumulative addition to each line
transmission <- group_by(transmission, Line, case) %>%
  arrange(Line, case) %>%
  mutate(csum = cumsum(New_Trans_Capacity)) %>%
  mutate(capacity = csum + `Line_Max_Reinforcement_MW`)
  
  #this section allows for the creation of two color scales on the same plot, found publically available
  #on github, not entirely sure how it works
  new_scale <- function(new_aes) {
    structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
  }
  
  ggplot_add.new_aes <- function(object, plot, object_name) {
    plot$layers <- lapply(plot$layers, bump_aes, new_aes = object)
    plot$scales$scales <- lapply(plot$scales$scales, bump_aes, new_aes = object)
    plot$labels <- bump_aes(plot$labels, new_aes = object)
    plot
  }
  
  bump_aes <- function(layer, new_aes) {
    UseMethod("bump_aes")
  }
  
  bump_aes.Scale <- function(layer, new_aes) {
    old_aes <- layer$aesthetics[remove_new(layer$aesthetics) %in% new_aes]
    new_aes <- paste0(old_aes, "_new")
    
    layer$aesthetics[layer$aesthetics %in% old_aes] <- new_aes
    
    if (is.character(layer$guide)) {
      layer$guide <- match.fun(paste("guide_", layer$guide, sep = ""))()
    }
    layer$guide$available_aes[layer$guide$available_aes %in% old_aes] <- new_aes
    layer
  }
  
  bump_aes.Layer <- function(layer, new_aes) {
    original_aes <- new_aes
    
    old_aes <- names(layer$mapping)[remove_new(names(layer$mapping)) %in% new_aes]
    new_aes <- paste0(old_aes, "_new")
    
    old_geom <- layer$geom
    
    old_setup <- old_geom$handle_na
    new_setup <- function(self, data, params) {
      colnames(data)[colnames(data) %in% new_aes] <- original_aes
      old_setup(data, params)
    }
    
    new_geom <- ggplot2::ggproto(paste0("New", class(old_geom)[1]), old_geom,
                                 handle_na = new_setup)
    
    new_geom$default_aes <- change_name(new_geom$default_aes, old_aes, new_aes)
    new_geom$non_missing_aes <- change_name(new_geom$non_missing_aes, old_aes, new_aes)
    new_geom$required_aes <- change_name(new_geom$required_aes, old_aes, new_aes)
    new_geom$optional_aes <- change_name(new_geom$optional_aes, old_aes, new_aes)
    
    layer$geom <- new_geom
    
    old_stat <- layer$stat
    
    old_setup2 <- old_stat$handle_na
    new_setup <- function(self, data, params) {
      colnames(data)[colnames(data) %in% new_aes] <- original_aes
      old_setup2(data, params)
    }
    
    new_stat <- ggplot2::ggproto(paste0("New", class(old_stat)[1]), old_stat,
                                 handle_na = new_setup)
    
    new_stat$default_aes <- change_name(new_stat$default_aes, old_aes, new_aes)
    new_stat$non_missing_aes <- change_name(new_stat$non_missing_aes, old_aes, new_aes)
    new_stat$required_aes <- change_name(new_stat$required_aes, old_aes, new_aes)
    new_stat$optional_aes <- change_name(new_stat$optional_aes, old_aes, new_aes)
    
    layer$stat <- new_stat
    
    layer$mapping <- change_name(layer$mapping, old_aes, new_aes)
    layer
  }
  
  bump_aes.list <- function(layer, new_aes) {
    old_aes <-  names(layer)[remove_new(names(layer)) %in% new_aes]
    new_aes <- paste0(old_aes, "_new")
    
    names(layer)[names(layer) %in% old_aes] <- new_aes
    layer
  }
  
  change_name <- function(list, old, new) {
    UseMethod("change_name")
  }
  
  change_name.character <- function(list, old, new) {
    list[list %in% old] <- new
    list
  }
  
  change_name.default <- function(list, old, new) {
    nam <- names(list)
    nam[nam %in% old] <- new
    names(list) <- nam
    list
  }
  
  change_name.NULL <- function(list, old, new) {
    NULL
  }
  
  remove_new <- function(aes) {
    stringi::stri_replace_all(aes, "", regex = "(_new)*")
  }

  #/ends color scale hacking
 
# makes map for each case and year 
for(l in 1:length(cases)){
  print(cases[l])
  for(j in 1:length(years)) {
    print(years[j])
    
    
    EERDWN.pjm.transmission <- transmission %>%
      filter(case == cases[l]) %>%
      filter(year == years[j])  %>%
      filter(capacity > 10) %>%
      mutate(old_capacity = capacity - New_Trans_Capacity)
      
    # colnames(EERDWN.pjm.transmission)[8] <- "capacity"
    
    
    EERDWN.pjm.transmission['x'] <- NA
    EERDWN.pjm.transmission['xend'] <- NA
    EERDWN.pjm.transmission['y'] <- NA
    EERDWN.pjm.transmission['yend'] <- NA
    
    
    # add centroid values to transmission data
    for (i in unique(EERDWN.pjm.transmission$start)) 
    {
      EERDWN.pjm.transmission <- within(EERDWN.pjm.transmission , x[start == i ] <- pjm.centroids$center_long[pjm.centroids$zone == i])
      EERDWN.pjm.transmission <- within(EERDWN.pjm.transmission , y[start == i ] <- pjm.centroids$center_lat[pjm.centroids$zone == i])
    }
    
    for (i in unique(EERDWN.pjm.transmission$end)) 
    {
      EERDWN.pjm.transmission <- within(EERDWN.pjm.transmission , xend[end == i ] <- pjm.centroids$center_long[pjm.centroids$zone == i])
      EERDWN.pjm.transmission <- within(EERDWN.pjm.transmission , yend[end == i ] <- pjm.centroids$center_lat[pjm.centroids$zone == i])
    }
    
    
    EERDWN.pjm.transmission <- arrange(EERDWN.pjm.transmission, desc(capacity))
    EERDWN.pjm.transmission$capacity <- as.numeric(as.character(EERDWN.pjm.transmission$capacity))
    
    
    
  
    pjm_base <- ggplot(data = EERDWN.pjm, mapping = aes(x = long, y = lat, fill=GenX.Region, group = group)) + 
      coord_quickmap() +
      theme(panel.background = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),) +
      scale_fill_manual(values = getPalette(15),na.value = "gray88", name = "Region") +
      scale_color_manual(values = getPalette(15),na.value = "gray88", name = "Region") +
      theme(legend.position="left") +
      geom_polygon(aes(color = GenX.Region))
    
    pjm_base + new_scale("color") + geom_segment(data=EERDWN.pjm.transmission, 
                                                 aes(x=x, y=y, xend=xend, yend=yend, size = capacity/1000, 
                                                     color = capacity/old_capacity), inherit.aes = F)+ 
      geom_label_repel(inherit.aes = F, data=EERDWN.pjm.transmission, 
                       aes(x=(x+xend)/2, y=(y+yend)/2 , label = round(capacity/1000, digits = 1)))+
      scale_color_gradient(low = "light gray", high = "black", labels = scales::percent, name="% New Build") +
      #scale_color_gradient(low = "light gray", high = "black", name="% New Build") +
      ggtitle(paste0(cases[l], "_", years[j])) +
      labs(size = "Capacity (GW)")+
      ggsave(paste0("Graphics/transmission/", cases[l], years[j], "_transmission_map.png"), width=10, height=10, dpi=300)
    
  }

}



