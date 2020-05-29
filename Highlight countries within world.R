

https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/   # Compute the centroid as the mean longitude and lattitude

https://socviz.co/maps.html   # 2012 US election results maps of different kinds.

https://stackoverflow.com/questions/42481783/how-to-fill-colors-correctly-in-geom-polygon-in-ggplot

Google search of: ggplot highlight select coutries different colors discrete

# ======================================================================================================



lib("rnaturalearth")
lib("rnaturalearthdata")
lib(ggplot2)
lib(purrr)
 
world <- ne_countries(scale = "medium", type = 'map_units', returnclass = "sf")
dim(world)
# class(world)

world <- world[!world$geounit %in% 'Antarctica', ]

# geoUnit <- unique(world$geounit)


world <- ne_countries(scale = "medium", returnclass = "sf")
# class(world)



world$PEP <- NA
world$PEP[world$geounit %in% 'United States of America'] <- 1
world$PEP[world$geounit %in% 'Canada'] <- 2
world$PEP[world$geounit %in% c('Argentina', 'India', 'South Africa','Italy')] <- 3
world$PEP[world$geounit %in% c("Indonesia", "Colombia", "Brazil", "Suriname", "Guyana", "French Guiana", "Trinidad and Tobago", 
             "Norway", "Kenya", "Seychelles", "China", "Australia", "Spain", "Iceland", "Philippines", "Thailand", "Mexico", "East Timor")] <- 4
world$PEP[world$geounit %in% 'Chile'] <- 5
world$PEP[world$geounit %in% c('Denmark', 'France', 'Sweden', 'England')] <- 6
world$PEP[world$geounit %in% 'New Zealand'] <- 7
world$PEP[world$geounit %in% 'Australia'] <- 8
world$PEP[world$geounit %in% c('Algeria', 'Egypt', 'Libya', 'Morocco', 'Sudan', 'Tunisia', 'Western Sahara','Czech Republic', 'Poland', 'Croatia',
             'Slovakia', 'Hungary', 'Romania', 'Moldova', 'Republic of Serbia')] <- 9


world$PEP_Name <- "No Collaboration within the last 6 Years" #0
world$PEP_Name[world$geounit %in% 'United States of America'] <- "PEP US" #1
world$PEP_Name[world$geounit %in% 'Canada'] <- "Collaborations, Stock Assessment Reviews, Spatial Assessments & Data Limited" #2
world$PEP_Name[world$geounit %in% c('Argentina', 'India', 'South Africa','Italy')] <- "Teaching Stock Assessment" #3
world$PEP_Name[world$geounit %in% c("Indonesia", "Colombia", "Brazil", "Suriname", "Guyana", "French Guiana", "Trinidad and Tobago", 
  "Norway", "Kenya", "Seychelles", "China", "Australia", "Spain", "Iceland", "Philippines", "Thailand", "Mexico", "East Timor")] <- "Data Limited Group" #4
world$PEP_Name[world$geounit %in% 'Chile'] <- "Teaching stock assessment & Stock assessment reivew" #5
world$PEP_Name[world$geounit %in% c('Denmark', 'France', 'Sweden', 'England')] <- "ICES Stock Assessment Review" #6
world$PEP_Name[world$geounit %in% 'New Zealand'] <- "Workshop Participation & Spatial Assessment Research" #7
world$PEP_Name[world$geounit %in% 'Australia'] <- "Spatial Assessment Research" #8
world$PEP_Name[world$geounit %in% c('Algeria', 'Egypt', 'Libya', 'Morocco', 'Sudan', 'Tunisia', 'Western Sahara','Czech Republic', 'Poland', 'Croatia',
  'Slovakia', 'Hungary', 'Romania', 'Moldova', 'Republic of Serbia')] <- "Teaching Stock Assessement, FAO Participants from multiple countries" #9


# =========================== ggplot2 ================================================================= 
   
 

dev.new(width = 1250/2, height = 600/2)
ggplot(data = world) +
    geom_sf(aes(fill = PEP)) +
    scale_fill_viridis_c(option = "plasma") +
    theme(legend.position = "none") 
 

dev.new(width = 1250/2, height = 600/2)
d <- ggplot(data = world) +
    geom_sf(aes(fill = PEP)) +
    theme(legend.position = "none") 
d + scale_colour_viridis_d(direction = -1, option = 'B')


rm(d)
dev.new(width = 1250/2, height = 600/2)
d <- ggplot(data = world) +
    geom_sf(aes(fill = PEP), color = world$PEP) +
    theme(legend.position = "none") 
d + scale_colour_gradient()



png(width = 1250, height = 600, file = 'PEP_Collaboration.png')
# dev.new(width = 1250/2, height = 600/2)
d <- ggplot(data = world) +
    geom_sf(aes(fill = PEP)) +
    theme(legend.position = "none") 
d + scale_fill_gradientn(colors = rainbow(10))
dev.off()


# ================ gvisGeoChart ============================================================================== 
   
library(JRWToolBox)   
lib(googleVis)
lib("rnaturalearth")
lib("rnaturalearthdata")
lib(rgeos)
lib(farver)
lib(colorspace)


 

# PEP <- gvisGeoChart(world, locationvar = 'geounit', colorvar ='PEP', hovervar = 'PEP_Name',
#                   options = list(projection="kavrayskiy-vii", width = 1200, height = 800))

# plot(PEP)


# plot(gvisGeoChart(world, locationvar = 'geounit', colorvar ='PEP', hovervar = 'PEP_Name',
#                    options = list(projection="kavrayskiy-vii", width = 1200, height = 800,
#                    colorAxis="{colors:['purple', 'red', 'orange', 'grey']}",
#                    backgroundColor="lightblue"), chartid = "PEP"))
# 
# 




PEP_Data <- world[!is.na(world$PEP), c("geounit", "PEP", "PEP_Name")]
PEP_Data$geounit[PEP_Data$geounit %in% 'United States of America'] <- 'United States'


PEP_Chart <- gvisGeoChart(PEP_Data, locationvar = 'geounit', colorvar ='PEP', hovervar = 'PEP_Name',
           options = list(projection="kavrayskiy-vii", width = 1200, height = 800,
                   colorAxis="{colors:['cyan', '#CC00FF', 'orange', 'grey']}",
                   backgroundColor="lightblue", legend = 'none'), chartid = "PEP")



PEP_Chart <- gvisGeoChart(PEP_Data, locationvar = 'geounit', colorvar ='PEP', hovervar = 'PEP_Name',
           options = list(projection="kavrayskiy-vii", width = 1200, height = 800,
                   colorAxis= paste0("{colors:['", paste(c('cyan', '#CC00FF', 'orange', 'grey'), collapse = "', '"), "']}"),
                   backgroundColor="lightblue", legend = 'none'), chartid = "PEP")




PEP_Chart <- gvisGeoChart(PEP_Data, locationvar = 'geounit', colorvar ='PEP', hovervar = 'PEP_Name',
           options = list(projection="kavrayskiy-vii", width = 1200, height = 800,
                   colorAxis= paste0("{colors:['", paste(hcl.colors(12, "Set 3"), collapse = "', '"), "']}"),
                   backgroundColor="lightblue", legend = 'none'), chartid = "PEP")


PEP_Chart <- gvisGeoChart(PEP_Data, locationvar = 'geounit', colorvar ='PEP', hovervar = 'PEP_Name',
           options = list(projection="kavrayskiy-vii", width = 1200, height = 800,
                   colorAxis= paste0("{colors:['", paste(rev(colorspace::rainbow_hcl(12)), collapse = "', '"), "']}"),
                   backgroundColor="lightblue", vis.editor="Editor"), chartid = "PEP")



plot(PEP_Chart)

# print(PEP_Chart, file = 'PEP_Collaboration.htm')





# ============================================================================================================ 

par(mfrow = c(2, 2))
pie(rep(1, 12), col = rainbow(12), main = "RGB/HSV")
pie(rep(1, 12), col = hcl.colors(12, "Set 2"), main = "HCL")
pie(rep(1, 12), col = hcl.colors(12, "Set 3"), main = "HCL")
pie(rep(1, 12), col = hcl.colors(12, "Set 4"), main = "HCL")
par(mfrow = c(1, 1))





# farver::encode_colour(matrix(c(181, 156, 160), ncol=3))
# [1] "#B59CA0"
# 
# pie(rep(1,1), col = "#B59CA0")







   
    
    +
    coord_map(projection = "albers", lat0 = 0,  lat1 = 45, xlim=c(-180,180), ylim=c(-60, 90))




dev.new(width = 1250, height = 660)
ggplot(data = world) +
    geom_sf(aes(fill = PEP)) +
    theme(legend.position = "none", col = c("cyan", "blue", "red"))
    
    
    
    







p <- ggplot(data = world,
           mapping = aes(fill = geounit))



p + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 0,  xlim=c(-180,180), ylim=c(-60, 90)) +
    guides(fill = FALSE)





library(maps)
us_states <- map_data("state")
head(us_states)



p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat, group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    guides(fill = FALSE)




p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                         fill = region))

p + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    guides(fill = FALSE)

























