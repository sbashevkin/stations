sensors <- read_html("https://cdec.water.ca.gov/reportapp/javareports?name=SensList")%>%
  html_elements('table')%>%
  html_table(header=T)%>%
  .[[1]]%>%
  .[,1:5]

names(sensors)<-tolower(as.character(sensors[1,]))

sensors<-sensors[-1,]%>%
  select(`sensor num`, description)

CDEC<-read_csv("data/CDEC_Stations.csv",
               col_types = cols_only(STA="c", `Station Name`="c",
                                     Latitude="d", Longitude="d",
                                     Sensors="c"))%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)%>%
  st_transform(crs=st_crs(deltamapr::R_EDSM_Subregions_Mahardja))%>%
  st_filter(deltamapr::R_EDSM_Subregions_Mahardja)%>%
  st_drop_geometry()%>%
  mutate(Station=paste0(`Station Name`, " (", STA, ")"))%>%
  select(Station, Latitude, Longitude, Sensors)%>%
  mutate(Sensors=str_split(Sensors, ","))%>%
  unnest(Sensors)%>%
  left_join(sensors, by=c("Sensors"="sensor num"))

# Trying to scrape info from each station's page
CDEC_info_retrieve<-function(station){
  inf<-read_html(glue("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id={station}"))%>%
    html_elements("table")
  if(class(inf)=="xml_missing"){
    return(NULL)
  }else{
    dat<-html_table(inf, convert=F)
    correct_table<-which(map_lgl(dat, ~ncol(.x)==6))
    if(length(correct_table)==1){
      dat<-dat[[correct_table]]
      names(dat)<-c("Sensor Description", "Sensor Number", "Duration", "Plot",	"Data Collection", "Data Available")
      return(dat)
    }else
      return(NULL)
  }
}

Stations<-set_names(CDEC$STA)

CDEC_info<-map_dfr(Stations, CDEC_info_retrieve, .id = "Station")

CDEC_info<-list()
for(i in 1:nrow(CDEC)){
  CDEC_info[[i]]<-read_html(glue("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id={CDEC$STA[i]}"))%>%
    html_element(xpath='//*[@id="main_content"]/div/div[1]/table[2]')%>%
    html_table(header=F)
}

read_html("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=ANC")%>%
  html_element(xpath='//*[@id="main_content"]/div/div[1]/table[2]')
