###############################
### CODE CREATED BY ELIZABETH WEBB TO CREATE MAP FIGURE FOR FLARE PLOTS
### NEAR CHERSKIY AND YAKUTSK. LAST UPDATED OCTOBER 2023
###
### lots of help from: https://r-craft.org/r-news/zooming-in-on-maps-with-sf-and-ggplot2/
##############################

#library(raster)
library(sf)
library(RColorBrewer)
library(stars)
library(ggplot2)
library(rnaturalearth)
library(tidyr)
library(dplyr)
library(ggnewscale)
library(raster)
library(cowplot)


###############################
### set working drive
##############################

setwd(" ")

###############################
### load in data
##############################

### note: this file is derived from the ESA CCI landcover 2015 raster. It is too large
### to upload to github, but can be shared upon request. Please email Elizabeth Webb for access

    russia<-read_stars("larchonly.tif") %>%
         setNames("larch") %>%
         mutate(larch= case_when (larch==80 ~ 1)) 
    
    
    worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                             returnclass = 'sf')
    
    sites<-read.csv("FLARE_site_info.csv")


###############################
### define CRS and mapping paramaters
##############################
      zoom_to <- c(100,56) 
      zoom_level <- 2
      lon_span <- 360 / 2^zoom_level
      lat_span <- 180 / 2^zoom_level
      lon_bounds <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
      lat_bounds <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)
      
      target_crs <- sprintf('+proj=laea +lon_0=%f +lat_0=%f +x_0=0 +y_0=0 +ellps=WGS84 
                   +datum=WGS84 +units=m +no_defs',
                            zoom_to[1], zoom_to[2])
      
      C <- 40075016.686   # ~ circumference of Earth in meters
      x_span <- C / 2^zoom_level
      y_span <- C / 2^(zoom_level+1)
      
      zoom_to_xy <- st_transform(st_sfc(st_point(zoom_to), crs = 4326),
                                 crs = target_crs)

      disp_window <- st_sfc(
        st_point(st_coordinates(zoom_to_xy - c(x_span / 6, y_span / 8))), ## south y, east x
        st_point(st_coordinates(zoom_to_xy + c(x_span / 3, y_span /1.8))), ## north y, west x
        crs = target_crs)

###############################
### prepare data
##############################
      
    # Don't include Duck fire
    sites<-subset(sites,site != 'Duck')
    
    # get only one point per site
    points<- sites %>% distinct(site, .keep_all = TRUE)
    
    # change regions to Arctic/subarctic
    points$region <- recode(points$region, 
                                   Cherskiy = 'Arctic',
                                  Yakutsk = 'Subarctic' )
    
    
    points$longitude<-as.numeric(points$longitude)
    points$latitude<-as.numeric(points$latitude)
    
    ### transform df into spdf
    points<-st_as_sf(points,coords=c("longitude", "latitude"), remove=FALSE, crs=4326) 
    
    ### transform points into target crs
    points <- st_transform(points, crs = target_crs)

    ## create city labels
    labelpoints <- data.frame(long = c(129.74161137818044,166.82504352927407),
                            lat = c(63.53467302931193,67.04558794702443 ),
                            citynames = c('Yakutsk', 'Cherskiy'),
                              stringsAsFactors = FALSE    )
    
     citypoints <- data.frame(long = c(129.74161137818044,161.32504352927407),
                             lat = c(62.03467302931193,68.74558794702443 ),
                            citynames = c('Yakutsk', 'Cherskiy'),
                               stringsAsFactors = FALSE    ) 
    ### transform df into spdf
    citypoints<-st_as_sf(citypoints,coords=c("long", "lat"), remove=FALSE, crs=4326) 
    labelpoints<-st_as_sf(labelpoints,coords=c("long", "lat"), remove=FALSE, crs=4326) 
    
    ### transform points into target crs
    citypoints <- st_transform(citypoints, crs = target_crs)
    labelpoints <- st_transform(labelpoints, crs = target_crs)


###############################
### plot main plot
##############################
    
plot<- ggplot() +
      ####### background 
      geom_sf(data = worldmap, color='#E5E5E5', fill='#E5E5E5') +
      
      ####### aboveground biomass
     geom_stars(data = russia ,downsample=20)+
   scale_fill_gradientn(name= "Larch dominance", na.value="transparent",
                        guide = guide_colourbar(barheight = 1),
                        labels = NULL,
                        colors = alpha(c(brewer.pal(9, "BuGn"))))+
   guides(fill = guide_legend(title.position = "right"))+

      ####### graticules
      scale_x_continuous(breaks = seq(60,140,20)) +
      scale_y_continuous(breaks = seq(40,70,10)) +
      
      ####### city points
      geom_sf(data=citypoints, colour="black", size=2.5, shape = 17) +
      
      ####### study sites
      new_scale_color() +
      new_scale_fill() +
      geom_sf(data=points, aes(fill=region),size=1.5, shape =21, 
              color='gray15') +
      scale_fill_manual(values=c("#FEC287FF", "#8b7bc5"), guide = NULL)+
      
      ####### city labels
      geom_sf_text(data=labelpoints,aes(label = citynames), size=3.5) +
      
      ####### theme, etc.  
      coord_sf(xlim = st_coordinates(disp_window)[,'X'],
               ylim = st_coordinates(disp_window)[,'Y'],
               crs = target_crs) +
      xlab("") +
      ylab("") +
      theme_bw() +
      theme( panel.ontop = T,
             panel.background = element_blank(),
             panel.border = element_rect(colour = "gray30", fill=NA, size=1),
             panel.grid.major=element_line(colour="#D3D3D3"),
             legend.direction = "horizontal", 
             legend.title.align=0.5,
             legend.background = element_rect(fill="transparent", color ="transparent"),
             legend.box = "horizontal",
             legend.margin = margin(0,0,0,0, unit="cm"),
             legend.box.just = "bottom",
             legend.position = c(0.87, 0.03),
             legend.key=element_rect(fill='transparent'))
    
    
###############################
### plot inset
##############################    
    zoom_level2 <- 2
    zoom_to2 <- c(140,90) 
    x_span2 <- C / 2^zoom_level2
    y_span2 <- C / 2^(zoom_level2+1)
    
    zoom_to_xy2 <- st_transform(st_sfc(st_point(zoom_to2), crs = 4326),
                                crs = target_crs)
    disp_window2 <- st_sfc(
      st_point(st_coordinates(zoom_to_xy2 - c(x_span2/2, y_span2))), ## south y, east x
      st_point(st_coordinates(zoom_to_xy2 + c(x_span2/2, y_span2))), ## north y, west x
      crs = target_crs)
    
    
inset<-ggplot() +
      geom_sf(data = worldmap, color='darkgrey', fill='darkgrey') + 
      coord_sf(xlim = st_coordinates(disp_window2)[,'X'],
               ylim = st_coordinates(disp_window2)[,'Y'],
               crs = target_crs) +
      geom_rect(aes(xmin = st_coordinates(disp_window)[,'X'][1],
                    xmax = st_coordinates(disp_window)[,'X'][2],
                    ymin = st_coordinates(disp_window)[,'Y'][1],
                    ymax = st_coordinates(disp_window)[,'Y'][2]), 
                color='black', fill=NA, linewidth=1) +
      theme(axis.text.x=element_blank(), 
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.x= element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "gray30", fill=NA, size=1),
            panel.grid.major=element_line(colour="#D3D3D3"),
            plot.margin=grid::unit(c(0,0,-1,-1), "mm"))
    
    

###############################
### combine and save plot
##############################    

finalplot<-ggdraw(plot) + 
  draw_plot(inset,  x=0.05, y=0.14, width=0.3, height=0.3, scale=1)



ggsave("Studysite.png", finalplot,
       width=8, height=6,units="in", scale=1, dpi=500)

        


