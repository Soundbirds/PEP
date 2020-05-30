

https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/   # Compute the centroid as the mean longitude and lattitude

https://socviz.co/maps.html   # 2012 US election results maps of different kinds.

https://stackoverflow.com/questions/42481783/how-to-fill-colors-correctly-in-geom-polygon-in-ggplot

Google search of: ggplot highlight select coutries different colors discrete

# ======================================================================================================



lib('rnaturalearth')
lib('rnaturalearthdata')
lib(ggplot2)
lib(purrr)
 
world <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')
dim(world)
# class(world)

world <- world[!world$geounit %in% 'Antarctica', ]
# geoUnit <- unique(world$geounit)


Grp1 <- 'United States of America'
Grp2 <- 'Canada'
Grp3 <- c('Argentina', 'India', 'South Africa','Italy', 'Uruguay', 'Peru', 'Eritrea', 'Iran', 'Madagascar', 'Maldives', 'Mauritius', 'Mozambique', 
             'Oman', 'Sierra Leone', 'Somalia', 'Sri Lanka', 'Tanzania', 'Italy', 'Spain', 'Bulgaria', 'Cyprus', 'Greece', 'Morocco', 'France', 
             'Montenegro', 'Tunisia', 'Turkey', 'Ukraine')  
Grp4 <- c('Suriname', 'Guyana', 'French Guiana', 'Trinidad and Tobago', 
             'Norway', 'China', 'Australia', 'Spain', 'Iceland', 'Philippines', 'Thailand', 'Mexico', 'East Timor')
Grp5 <- 'Chile'
Grp6 <- c('Denmark', 'France', 'Sweden', 'England')
Grp7 <- 'New Zealand'
Grp8 <- 'Australia'
Grp9 <- c('Colombia', 'Brazil', 'Indonesia', 'Kenya', 'Seychelles')
Grp10 <- 'Slovenia'
Grp11 <- 'Netherlands'

# # Example of testing if geo unit is kosher
# all(Grp9 %in%  world$geounit)
# Grp3[!Grp3 %in%  world$geounit]


world$PEP <- NA
world$PEP[world$geounit %in% Grp1] <- 1
world$PEP[world$geounit %in% Grp2] <- 2
world$PEP[world$geounit %in% Grp3] <- 3
world$PEP[world$geounit %in% Grp4] <- 4
world$PEP[world$geounit %in% Grp5] <- 5
world$PEP[world$geounit %in% Grp6] <- 6
world$PEP[world$geounit %in% Grp7] <- 7
world$PEP[world$geounit %in% Grp8] <- 8
world$PEP[world$geounit %in% Grp9] <- 9
world$PEP[world$geounit %in% Grp10] <- 10
world$PEP[world$geounit %in% Grp11] <- 11

world$PEP_Name <- "No Collaboration within the last 6 Years" #0
world$PEP_Name[world$geounit %in% Grp1] <- "PEP USA" #1
world$PEP_Name[world$geounit %in% Grp2] <- "Hake and other Collaborations, Stock Assessment Reviews, Spatial Assessments, and Data Limited" #2
world$PEP_Name[world$geounit %in% Grp3] <- "Teaching Stock Assessment" #3
world$PEP_Name[world$geounit %in% Grp4] <- "Data Limited Group" #4
world$PEP_Name[world$geounit %in% Grp5] <- "Teaching Stock Assessment & Stock Assessment Review" #5
world$PEP_Name[world$geounit %in% Grp6] <- "ICES Stock Assessment Review" #6
world$PEP_Name[world$geounit %in% Grp7] <- "Workshop Participation & Spatial Assessment Research" #7
world$PEP_Name[world$geounit %in% Grp8] <- "Spatial Assessment Research" #8
world$PEP_Name[world$geounit %in% Grp9] <- "Data Limited & Teaching Stock Assessement" #9
world$PEP_Name[world$geounit %in% Grp10] <- "International Whaling Commission" #10
world$PEP_Name[world$geounit %in% Grp11] <- "Random Effects in Stock Assessments" #11

Col <- rainbow(14)[-c(5, 6, 11)]

# =========================== ggplot2 ================================================================= 
   
 

# dev.new(width = 1250/2, height = 600/2)
# ggplot(data = world) +
#     geom_sf(aes(fill = PEP)) +
#     scale_fill_viridis_c(option = "plasma") +
#     theme(legend.position = "none") 
#  
# 
# dev.new(width = 1250/2, height = 600/2)
# d <- ggplot(data = world) +
#     geom_sf(aes(fill = PEP)) +
#     theme(legend.position = "none") 
# d + scale_colour_viridis_d(direction = -1, option = 'B')
# 
# 
# rm(d)
# dev.new(width = 1250/2, height = 600/2)
# d <- ggplot(data = world) +
#     geom_sf(aes(fill = PEP), color = world$PEP) +
#     theme(legend.position = "none") 
# d + scale_colour_gradient()

# png(width = 1250, height = 600, file = 'PEP_Collaboration, No Legend.png')
# # dev.new(width = 1250/2, height = 600/2)
# d <- ggplot(data = world) +
#     geom_sf(aes(fill = PEP)) +
#     theme(legend.position = "none") 
# d + scale_fill_gradientn(colors = rainbow(10))
# dev.off()


# Use imap() and locator() to find locations for the numerals
Imap::imap(list(world.h.land, world.h.borders), poly = c("grey40", NA), zoom = FALSE)


LS <- 7  # Start legend lat
dev.new(width = 1250/2, height = 600/2)
# png(width = 1250, height = 600, file = 'PEP_Collaboration, with Legend.png')
d <- ggplot(data = world) +
     geom_sf(aes(fill = PEP)) +
     theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
     scale_fill_gradientn(colors = rainbow(14)[-c(5, 6, 11)]) +
     annotate("text", x = -100, y = 40, label = "1") +
     annotate("text", x = -116, y = 60, label = "2") + 
     annotate("text", x = -65, y = -34, label = "3") + 
     annotate("text", x = 103.5, y = 34.6, label = "4") + 
     annotate("text", x = -102.7, y = 25.5, label = "4") + 
     annotate("text", x = -73, y = -28, label = "5") + 
     annotate("text", x = 2.2, y = 47.3, label = "6") + 
     annotate("text", x = 178.8, y = -41.72, label = "7") + 
     annotate("text", x = 132.4, y = -23.3, label = "8") + 
     annotate("text", x = -50.1, y = -9, label = "9") +  
     annotate("text", x = 18, y = 47, label = "10", color = 'white') +  
     annotate("text", x = 9, y = 52, label = "11", , color = 'white') + 
     
     annotate("text", x = -182, y = LS,      hjust = 0, label = "  1. PEP USA") +
     annotate("text", x = -182, y = LS -  5, hjust = 0, label = "  2. Hake and other Collaborations, Stock Assessment") +
     annotate("text", x = -182, y = LS - 10, hjust = 0, label = "       Reviews, Spatial Assessments, and Data Limited") +
     annotate("text", x = -182, y = LS - 15, hjust = 0, label = "  3. Teaching Stock Assessment") +
     annotate("text", x = -182, y = LS - 20, hjust = 0, label = "  4. Data Limited Group") +
     annotate("text", x = -182, y = LS - 25, hjust = 0, label = "  5. Teaching Stock Assessment & Stock Assessment Review") +
     annotate("text", x = -182, y = LS - 30, hjust = 0, label = "  6. ICES Stock Assessment Review") +
     annotate("text", x = -182, y = LS - 35, hjust = 0, label = "  7. Workshop Participation & Spatial Assessment Research") +
     annotate("text", x = -182, y = LS - 40, hjust = 0, label = "  8. Spatial Assessment Research") +
     annotate("text", x = -182, y = LS - 45, hjust = 0, label = "  9. Data Limited & Teaching Stock Assessement") +
     annotate("text", x = -182, y = LS - 50, hjust = 0, label = "10. International Whaling Commission")
 d + annotate("text", x = -182, y = LS - 55, hjust = 0, label = "11. Random Effects in Stock Assessments")
# dev.off()


# With colored text -cant read some on the grey background

LS <- 5  # Start legend lat
# dev.new(width = 1250/2, height = 600/2)
png(width = 1250, height = 600, file = 'PEP_Collaboration, with Legend.png')
d <- ggplot(data = world) +
     geom_sf(aes(fill = PEP)) +
     theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
     scale_fill_gradientn(colors = Col) +
     annotate("text", x = -100, y = 40, label = "1") +
     annotate("text", x = -116, y = 60, label = "2") + 
     annotate("text", x = -65, y = -34, label = "3") + 
     annotate("text", x = 103.5, y = 34.6, label = "4") + 
     annotate("text", x = -102.7, y = 25.5, label = "4") + 
     annotate("text", x = -73, y = -28, label = "5") + 
     annotate("text", x = 2.2, y = 47.3, label = "6") + 
     annotate("text", x = 178.8, y = -41.72, label = "7") + 
     annotate("text", x = 132.4, y = -23.3, label = "8") + 
     annotate("text", x = -50.1, y = -9, label = "9") +  
     annotate("text", x = 18, y = 47, label = "10", color = 'white') +  
     annotate("text", x = 9, y = 52, label = "11", , color = 'white') + 
     
     annotate("text", x = -180, y = LS,      hjust = 0, label = "  1. PEP USA", color = Col[1]) +
     annotate("text", x = -180, y = LS -  5, hjust = 0, label = "  2. Hake and other Collaborations, Stock Assessment", color = Col[2]) +
     annotate("text", x = -180, y = LS - 10, hjust = 0, label = "       Reviews, Spatial Assessments, and Data Limited", color = Col[2]) +
     annotate("text", x = -180, y = LS - 15, hjust = 0, label = "  3. Teaching Stock Assessment", color = Col[3]) +
     annotate("text", x = -180, y = LS - 20, hjust = 0, label = "  4. Data Limited Group", color = Col[4]) +
     annotate("text", x = -180, y = LS - 25, hjust = 0, label = "  5. Teaching stock assessment & Stock Assessment Review", color = Col[5]) +
     annotate("text", x = -180, y = LS - 30, hjust = 0, label = "  6. ICES Stock Assessment Review", color = Col[6]) +
     annotate("text", x = -180, y = LS - 35, hjust = 0, label = "  7. Workshop Participation & Spatial Assessment Research", color = Col[7]) +
     annotate("text", x = -180, y = LS - 40, hjust = 0, label = "  8. Spatial Assessment Research", color = Col[8]) +
     annotate("text", x = -180, y = LS - 45, hjust = 0, label = "  9. Data Limited & Teaching Stock Assessement", color = Col[9]) +
     annotate("text", x = -180, y = LS - 50, hjust = 0, label = "10. International Whaling Commission", color = Col[10])
 d + annotate("text", x = -180, y = LS - 55, hjust = 0, label = "11. Random Effects in Stock Assessments", color = Col[11])
dev.off()






# ================ gvisGeoChart ============================================================================== 
   
library(JRWToolBox)   
lib(googleVis)
lib("rnaturalearth")
lib("rnaturalearthdata")
lib(rgeos)
lib(farver)
lib(colorspace)


PEP_Data <- world[!is.na(world$PEP), c("geounit", "PEP", "PEP_Name")]
PEP_Data$geounit[PEP_Data$geounit %in% 'United States of America'] <- 'United States'


# PEP_Chart <- gvisGeoChart(PEP_Data, locationvar = 'geounit', colorvar ='PEP', hovervar = 'PEP_Name',
#            options = list(projection="kavrayskiy-vii", width = 1200, height = 800,
#                    colorAxis="{colors:['cyan', '#CC00FF', 'orange', 'grey']}",
#                    backgroundColor="lightblue", legend = 'none'), chartid = "PEP")
# 
# 
# 
# PEP_Chart <- gvisGeoChart(PEP_Data, locationvar = 'geounit', colorvar ='PEP', hovervar = 'PEP_Name',
#            options = list(projection="kavrayskiy-vii", width = 1200, height = 800,
#                    colorAxis= paste0("{colors:['", paste(c('cyan', '#CC00FF', 'orange', 'grey'), collapse = "', '"), "']}"),
#                    backgroundColor="lightblue", legend = 'none'), chartid = "PEP")
# 
# 
# 
# 
# PEP_Chart <- gvisGeoChart(PEP_Data, locationvar = 'geounit', colorvar ='PEP', hovervar = 'PEP_Name',
#            options = list(projection="kavrayskiy-vii", width = 1200, height = 800,
#                    colorAxis= paste0("{colors:['", paste(hcl.colors(12, "Set 3"), collapse = "', '"), "']}"),
#                    backgroundColor="lightblue", legend = 'none'), chartid = "PEP")

# PEP_Chart <- gvisGeoChart(PEP_Data, locationvar = 'geounit', colorvar ='PEP', hovervar = 'PEP_Name',
#            options = list(projection="kavrayskiy-vii", width = 1200, height = 800,
#                    colorAxis= paste0("{colors:['", paste(rev(colorspace::rainbow_hcl(14)[-c(5, 6, 11)]), collapse = "', '"), "']}"),
#                    backgroundColor="lightblue", legend = 'none'), chartid = "PEP")
# 

PEP_Chart <- gvisGeoChart(PEP_Data, locationvar = 'geounit', colorvar ='PEP', hovervar = 'PEP_Name',
           options = list(projection="kavrayskiy-vii", width = 1200, height = 800,
                   colorAxis= paste0("{colors:['", paste(farver::encode_colour(farver::decode_colour(Col)), collapse = "', '"), "']}"),
                   backgroundColor="lightblue", legend = 'none'), chartid = "PEP")




(PEP_RGB <- rbind(
c(255, 0, 0), #1 Red
c(255, 144, 0), #2 Orange
c(218, 236, 0), #3 Gold
c(118, 255, 0), #4 Green
c(35, 255, 76), #5 Light Green
c(52, 255, 181), #6 Aqua Green
c(61, 193, 255), #7 Dodger Blue
c(34, 82, 255), #8 Blue
c(99, 0, 255), #9 Dark Blue
c(211, 0, 245), #10 Magenta
c(255, 0, 153) #11 Pink
))

(PEP_Colors <- farver::encode_colour(PEP_RGB))

PEP_Chart <- gvisGeoChart(PEP_Data, locationvar = 'geounit', colorvar ='PEP', hovervar = 'PEP_Name',
           options = list(projection="kavrayskiy-vii", width = 1200, height = 800,
                   colorAxis= paste0("{colors:['", paste(PEP_Colors, collapse = "', '"), "']}"),
                   backgroundColor="lightblue", legend = 'none'), chartid = "PEP")



plot(PEP_Chart)

# print(PEP_Chart, file = 'PEP_Collaboration.htm')





# ================================= Colors =========================================================================== 

par(mfrow = c(2, 2))
pie(rep(1, 12), col = rainbow(12), main = "RGB/HSV")
pie(rep(1, 12), col = hcl.colors(12, "Set 2"), main = "HCL")
pie(rep(1, 12), col = hcl.colors(12, "Set 3"), main = "HCL")
pie(rep(1, 12), col = hcl.colors(12, "Set 4"), main = "HCL")
par(mfrow = c(1, 1))


# rgb2hsv(r = 181, g = 156, b = 160, maxColorValue = 255)



farver::encode_colour(matrix(c(181, 156, 160), ncol=3))
[1] "#B59CA0" 

pie(rep(1,1), col = "#B59CA0")



PEP_Colors <- c(



   
    
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

























