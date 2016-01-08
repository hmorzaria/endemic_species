#'Hem Nalini Morzaria Luna
#'September 29, 2015
#'Calculation of centroid of raster 
#'hmorzarialuna@gmail.com
#'
#'## @knitr setpreferences

x = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
      "PBSmapping", "rgdal", "fields","data.table","rgbif","raster", "rasterVis",
      "sp","sperich","spocc","dplyr","SDMTools","ggplot2","ggmap", "httr",
      "wesanderson","tidyr","cowplot","stringr")
lapply(x, require, character.only = TRUE)
cleanbib()
options("citation_format" = "pandoc")

# read chunk (does not run code)

rm(list=ls())

## @knitr getcentroid

datafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Temperature_endemic species distributions/Analysis/resultados_asc"
analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Temperature_endemic species distributions/Analysis"

crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

setwd(analysispath)

#approximate radius of the earth
r.earth = 6378.2

species.table <- fread("species_list.txt",header=TRUE) %>% 
  tbl_df %>% 
  mutate (species_name = str_c(Species,name,sep="_"))

species.list = unique(species.table$species_name)

table.results = matrix(0,0,5) %>% 
  data.frame 


for(eachspecies in 1:length(species.list))
{
  this.species = species.list[eachspecies]
  
  setwd(datafiles)
  raster.files <- list.files(path=datafiles,pattern =paste("^",this.species,sep=""))
  
for(eachfile in 1:length(raster.files))

  {
  
  this.file.data= subset(species.table, species_name%in% this.species)
  
  projection = raster.files[eachfile] %>% 
    strsplit("[.]") %>% 
    unlist %>% 
    .[1] %>% 
    strsplit("_") %>% 
    unlist %>% 
    .[4]
    
  this.file.data$projection = projection
      
  raster.data = raster.files[eachfile] %>% raster
  proj4string(raster.data) <- crs.geo.wgs
  
  # Convert raster to SpatialPointsDataFrame
  r.pts = rasterToPoints(raster.data, spatial=TRUE)
  
  r.pts@data = data.frame(r.pts@data, long=coordinates(r.pts)[,1],
                           lat=coordinates(r.pts)[,2])
  
  data.centroid = r.pts %>% 
    data.frame %>% 
    tbl_df %>% 
    select(1:3) %>% 
    setnames(c("data","long","lat")) %>% 
    mutate(LatAb = lat * data) %>% 
    summarise(sum(LatAb)/sum(data)) %>% 
    as.numeric
  
  this.file.data$centroid = data.centroid

  table.results = rbind(table.results,this.file.data)
  
}

  }


table.results2 = matrix(0,0,6) %>% 
  data.frame 


for(eachspecies in 1:length(species.list))
{

  this.species = species.list[eachspecies]
  
  centroid.observed = table.results %>% 
    filter(species_name == this.species) %>% 
    filter(projection == "obs" ) %>% 
    .$centroid
    
 this.species.range = table.results %>% 
    filter(species_name == this.species) %>% 
    filter(projection == "26"| projection == "45"| projection == "85") %>% 
   mutate(observed_centroid = centroid.observed) %>% 
    mutate(range_shift = ((centroid - observed_centroid) * (pi/180)) * r.earth)

 table.results2 = rbind(table.results2,this.species.range)
   }

  #shift is in km
  
 
write.csv(table.results2, file="species_shift.csv")

table.results2 = table.results2 %>% 
  mutate(species_full=paste(Species,name,sep=" "))

range.plot = ggplot(data=table.results2, aes(y=range_shift, x=species_full,colour=projection)) +
  geom_point(size=3, shape = 16) +
  scale_colour_manual(values = c("cadetblue","indianred4","darkorange"),
                      name="RCP:",
                      labels=c("2.6","4.5","8.5"))+
  theme_bw() +
  geom_hline(yintercept=0,linetype = "dashed")+
  labs(x= ("Species"), y =("Range shift (Observed - RCP scenario) km")) + 
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12, angle=90),
        axis.text.x = element_text(colour = "black", angle = 90,size=10,hjust = 1, vjust = 0), 
        axis.text.y = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))
X11()


ggsave("range_shift.png",dpi=300,width=4, height=4)


