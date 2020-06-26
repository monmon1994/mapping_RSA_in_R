library(maptools)
library(raster)
library(plyr)
library(ggplot2)
library(rgdal)
library(ggmap)
library(scales)
library(extrafont)
library(ggthemes)
library(ggrepel)
loadfonts(device = "win", quiet = T)


RSA2<-getData("GADM", country="ZA", level=2)


plot(RSA2)

#RSA2_UTM <-spTransform(RSA2, CRS("+init=EPSG:32735"))  

RSA2@data$NAME_1

easterncape <- RSA2[RSA2@data$NAME_1 == "Eastern Cape",]
easterncape_df<-fortify(easterncape)

kzn <- RSA2[RSA2@data$NAME_1 == "KwaZulu-Natal", ]
kzn_df <- fortify(kzn)

ethekwini <- RSA2[RSA2@data$NAME_2 == "eThekwini",]
ethekwini_df <- fortify(ethekwini)

alfrednzo <- RSA2[RSA2@data$NAME_2 == "Alfred Nzo",]
alfrednzo_df <- fortify(alfrednzo)

# adding count data

NAME_1<-RSA1_UTM@data$NAME_1
count<-sample(1:1000,9)     #or any other data you can associate with admin level here

count_df<-data.frame(NAME_1, count)

RSA1_UTM@data$id <- rownames(RSA1_UTM@data)
RSA1_UTM@data <- merge(RSA1_UTM@data, count_df, by="NAME_1")
RSA1_df <- fortify(RSA1_UTM)
RSA1_df <- merge(RSA1_df, RSA1_UTM@data, by="id")

# set theme

theme_opts<-list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_blank(),
                       plot.background = element_blank(),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       text = element_text(family = "Helvetica"),
                       plot.caption = element_text(family = "Helvetica", size = 12, vjust = 0.5),
                       plot.caption.position = "plot"))

points <- data.frame(long = c(31.004370, 29.724720), 
                     lat = c(-29.703070, -30.761669), 
                     name = c("Phoenix", "Nkantolo"))

pdf("map_RSA_final.pdf", width = 9, height = 7)
ggplot() + 
  geom_polygon(data = RSA2, aes(long,lat,group=group), 
               fill="whitesmoke", colour = "#1d1d1b")+
  geom_path(data = RSA2, aes(long,lat, group=group), 
            color="#1d1d1b", size=0.1) +
  geom_path(data = easterncape_df, aes(long, lat, group = group), 
            colour = "#495da7") +
  geom_path(data = kzn_df, aes(long, lat, group = group), 
            colour = "#495da7") +
  geom_polygon(data= ethekwini_df, aes(long, lat, group = group), 
               fill="#00965b")+
  geom_polygon(data = alfrednzo_df, aes(long, lat, group = group), 
               fill = "#00965b")+
  geom_point(data = points, aes(x = long, y = lat), size = 2, col = "red") +
  #geom_label_repel(data = points, aes(long, lat, label = name), 
   #               size = 4, label.size = 0.4, label.padding = 0.3, segment.size = 0.5, 
   #               segment.color = "black", hjust = -12, vjust = 1, force = 2) +
  geom_point(data = SA_Cities, aes(x = long, y = lat), size = 0.5) +
  geom_label_repel(data = SA_Cities, aes(long, lat, label = name), 
                   size = 4, label.size = 0.3, label.padding = 0.3, segment.size = 0.5, 
                   segment.color = "black", force = 2
                  ) +
  theme(aspect.ratio=1)+
  labs(caption = "Source: www.gadm.org, June 2020
                  Graphic: Monique Bennett at Good Governance Africa",
       title = "Location of Phoenix and Nkantolo within the municipality boundaries of South Africa") +
  theme_opts
dev.off()

ggsave("map_of_RSA_erica.png", dpi = 600, width = 9, height = 7)


### 

SA_Cities <- data.frame(name = c("Cape Town", "Durban", "Johannesburg","Pretoria", 
                                 "Port Elizabeth", "Pietermaritzburg",
                                 "East London", "Bloemfontein", "Phoenix", "Nkantolo"),
                        
                        lat = c(-33.926, -29.858, -26.202, -25.745, -33.961, 
                                -29.617, -33.015, -29.121, -29.703070, -30.761669),
                        
                        long = c(18.423, 31.029, 28.044, 28.188, 25.615, 
                                30.393, 27.912, 26.214, 31.004370, 29.724720))
