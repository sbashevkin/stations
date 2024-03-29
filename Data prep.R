require(dplyr)
require(deltareportr)
require(lubridate)
require(stringr)
require(discretewq)
require(readr)
require(tidyr)
require(sf)
require(readxl)


# Zooplankton -------------------------------------------------------------


## YBFMP -------------------------------------------------------------------

download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.494.1&entityid=9190cd46d697e59aca2de678f4ca1c95", file.path(tempdir(), "YBFMP_zoop.csv"), mode="wb")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.494.1&entityid=f2332a9a2aad594f61fea525584694da", file.path(tempdir(), "YBFMP_zoop_stations.csv"), mode="wb")

YBFMP_zoop_stations<-read_csv(file.path(tempdir(), "YBFMP_zoop_stations.csv"),
                              col_types=cols_only(StationCode="c", Latitude="d", Longitude="d"))%>%
  rename(Station=StationCode)

YBFMP_zoop<-read_csv(file.path(tempdir(), "YBFMP_zoop.csv"),
                     col_types=cols_only(Date="c", Time="c", StationCode="c"))%>%
  rename(Station=StationCode)%>%
  mutate(Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
         Source="YBFMP",
         Year=year(Date))%>%
  select(-Time)%>%
  distinct(Station, Date, Datetime, .keep_all = TRUE)%>% # Select unique samples
  group_by(Station, Year, Source)%>%
  summarise(N=n(), .groups="drop")%>%
  left_join(YBFMP_zoop_stations, by="Station")%>%
  filter(!is.na(Latitude) & !is.na(Longitude))%>%
  mutate(Station2=Station)


# FRP ---------------------------------------------------------------------

#FRP_stations<-st_read("FRPstations")

#download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.2&entityid=41d077ac60682ce93c57c1ae1957fdcb", file.path(tempdir(), "FRP_Fish.csv"), mode="wb")
#("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.2&entityid=630f16b33a9cbf75f1989fc18690a6b3", file.path(tempdir(), "FRP_bugs.csv"), mode="wb")
#download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.2&entityid=d4c76f209a0653aa86bab1ff93ab9853", file.path(tempdir(), "FRP_zoops.csv"), mode="wb")

#FRP_zoop<-read_csv(file.path(tempdir(), "FRP_zoops.csv"),
#                   col_types=cols_only(Date="c", Time="c", StationCode="c"))

#FRP_bugs<-read_csv(file.path(tempdir(), "FRP_bugs.csv"))
##Use st_nearest_feature, not st_join to assign FRP stations

## All zoops ---------------------------------------------------------------

Zoop<-zooper::zoopEnvComb%>%
  filter(!is.na(Latitude) & !is.na(Longitude))%>%
  mutate(Station2=if_else(str_detect(Station, "EZ"), paste(Station, Date), Station),
         Year=year(Date))%>%
  group_by(Year, Station, Station2, Source)%>%
  summarise(Latitude=unique(Latitude), Longitude=unique(Longitude), N=n(), .groups="drop")%>%
  bind_rows(YBFMP_zoop)%>%
  mutate(Parameter="Zooplankton")

# Water quality and fish -------------------------------------------------------------


## EDSM --------------------------------------------------------------------


download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.3&entityid=d468c513fa69c4fc6ddc02e443785f28", file.path(tempdir(), "EDSM_20mm.csv"), mode="wb")
#EDSM KDTR
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.3&entityid=4d7de6f0a38eff744a009a92083d37ae", file.path(tempdir(), "EDSM_KDTR.csv"), mode="wb")

EDSM <- read_csv(file.path(tempdir(), "EDSM_20mm.csv"),
                 col_types=cols_only(Date="c", Time="c", TargetLat="d", TargetLong="d", Station="c"))%>%
  rename(Latitude=TargetLat, Longitude=TargetLong)%>%
  bind_rows(read_csv(file.path(tempdir(), "EDSM_KDTR.csv"),
                     col_types=cols_only(Date="c", Time="c", TargetLat="d", TargetLong="d", Station="c"))%>%
              rename(Latitude=TargetLat, Longitude=TargetLong))%>%
  filter(!is.na(Latitude) & !is.na(Longitude))%>%
  mutate(Source="EDSM",
         Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
         Year=year(Date),
         Station2=Station)%>%
  select(-Time)%>%
  distinct(Source, Station, Latitude, Longitude, Date, Datetime, .keep_all=T)%>%
  group_by(Year, Station, Source, Station2)%>%
  summarise(Latitude=mean(Latitude), Longitude=mean(Longitude), N=n(), .groups="drop")


## DJFMP --------------------------------------------------------------------


download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.4&entityid=e17878fccccafeddaff57eac97d8e170", file.path(tempdir(), "DJFMP_Seine.csv"), mode="wb")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.4&entityid=99a038d691f27cd306ff93fdcbc03b77", file.path(tempdir(), "DJFMP_stations.csv"), mode="wb")

DJFMP_stations <- read_csv(file.path(tempdir(), "DJFMP_stations.csv"),
                           col_types=cols_only(StationCode="c", Latitude_location="d", Longitude_location="d"))%>%
  rename(Station=StationCode, Latitude=Latitude_location, Longitude=Longitude_location)

DJFMP<-read_csv(file.path(tempdir(), "DJFMP_Seine.csv"),
                col_types = cols_only(StationCode = "c", SampleDate="c", SampleTime="c"))%>%
  rename(Station=StationCode, Date=SampleDate, Time=SampleTime)%>%
  mutate(Source="DJFMP",
         Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%H:%M:%S", tz="America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         Year=year(Date))%>%
  select(-Time)%>%
  distinct(Station, Date, Datetime, .keep_all = TRUE)%>%
  group_by(Station, Year, Source)%>%
  summarise(N=n(), .groups="drop")%>%
  left_join(DJFMP_stations, by="Station")%>%
  filter(!is.na(Latitude) & !is.na(Longitude))%>%
  mutate(Station2=Station)



## YBFMP -------------------------------------------------------------------


download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.2&entityid=015e494911cf35c90089ced5a3127334", file.path(tempdir(), "YBFMP_Fish.csv"), mode="wb")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.2&entityid=6a82451e84be1fe82c9821f30ffc2d7d", file.path(tempdir(), "YBFMP_Fish_stations.csv"), mode="wb")

YBFMP_stations_fish<-read_csv(file.path(tempdir(), "YBFMP_Fish_stations.csv"),
                              col_types=cols_only(StationCode="c", LatitudeLocation="d", LongitudeLocation="d"))%>%
  rename(Station=StationCode, Latitude=LatitudeLocation, Longitude=LongitudeLocation)%>%
  distinct()

YBFMP_fish<-read_csv(file.path(tempdir(), "YBFMP_Fish.csv"),
                     col_types=cols_only(SampleDate="c", SampleTime="c", StationCode="c"))%>%
  rename(Date=SampleDate, Time=SampleTime, Station=StationCode)%>%
  mutate(Date=parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         Source="YBFMP",
         Year=year(Date))%>%
  select(-Time)%>%
  distinct(Station, Date, Datetime, .keep_all = TRUE)%>%
  group_by(Station, Year, Source)%>%
  summarise(N=n(), .groups="drop")%>%
  left_join(YBFMP_stations_fish, by="Station")%>%
  filter(!is.na(Latitude) & !is.na(Longitude))%>%
  mutate(Station2=Station)

YBFMP_WQ<-bind_rows(YBFMP_fish, YBFMP_zoop)%>%
  group_by(Station, Year, Source, Station2)%>%
  summarise(N=sum(N), Latitude=unique(Latitude), Longitude=unique(Longitude), .groups="drop")


# Smelt Larval Survey -----------------------------------------------------

download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.534.2&entityid=a365f36987dbfeb4cb9150946b7867c0", file.path(tempdir(), "SLS_Fish.csv"), mode="wb")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.534.2&entityid=a96f60dbaa275193abfa5b9cdadd7aa3", file.path(tempdir(), "SLS_stations.csv"), mode="wb")

SLS_stations<-read_csv(file.path(tempdir(), "SLS_stations.csv"),
                       col_types=cols_only(Station="c", Lat="c", Long="c"))%>%
  separate(Lat, into=c("Lat_deg", "Lat_min", "Lat_sec"), sep=" ", convert=T)%>%
  separate(Long, into=c("Long_deg", "Long_min", "Long_sec"), sep=" ", convert=T)%>%
  mutate(Latitude=Lat_deg+Lat_min/60+Lat_sec/3600,
         Longitude=(Long_deg+Long_min/60+Long_sec/3600)*(-1))%>%
  bind_rows(tibble(Station="912", Latitude=37.966, Longitude=-121.367))%>% # Station 912 currently missing from stations
  select(Station, Latitude, Longitude)

SLS<-read_csv(file.path(tempdir(), "SLS_Fish.csv"),
              col_types=cols_only(Date="c", Time="c", Station="c"))%>%
  mutate(Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
         Source="SLS",
         Year=year(Date))%>%
  select(-Time)%>%
  distinct(Station, Date, Datetime, .keep_all = TRUE)%>%
  group_by(Station, Year, Source)%>%
  summarise(N=n(), .groups="drop")%>%
  left_join(SLS_stations, by="Station")%>%
  filter(!is.na(Latitude) & !is.na(Longitude))%>%
  mutate(Station2=Station)

## Integrated water quality ------------------------------------------------

WQ<-wq(End_year = 2021,
       Sources = c("EMP", "STN", "FMWT", "DJFMP", "SKT", "20mm", "Suisun", # Everyone except EDSM since I'll load that in separately to pull the target coordinates
                   "Baystudy", "USBR", "USGS"))%>%
  filter(!is.na(Latitude) & !is.na(Longitude))%>%
  mutate(Station2=case_when(
    str_detect(Station, "EZ")~ paste(Station, Date), 
    TRUE ~ Station))%>%
  group_by(Year, Station, Station2, Source)%>%
  mutate(n_Lat=n_distinct(Latitude), 
         n_Lon=n_distinct(Longitude))%>%
  ungroup()%>%
  mutate(Station2=if_else(n_Lat>1 | n_Lon>1, paste(Station2, Datetime), Station2))%>%
  group_by(Year, Station, Station2, Source)%>%
  summarise(Latitude=unique(Latitude), Longitude=unique(Longitude), N=n(), .groups="drop")%>%
  bind_rows(EDSM, DJFMP, YBFMP_WQ, SLS)%>% # Add DJFMP seine data since they are not in the integrated WQ dataset
  mutate(Parameter="Water_quality")


## Fish --------------------------------------------------------------------


Fish<-WQ%>%
  filter(Source%in%c("STN", "FMWT", "EDSM", "DJFMP", "SKT", "20mm", "Suisun",
                     "Baystudy", "SLS"))%>%
  bind_rows(YBFMP_fish)%>%
  mutate(Parameter="Fish")

# Benthic and phytoplankton ----------------------------------------------


Data<-DeltaDater(Start_year=1950, End_year=2021, 
                 Variables = c("Bivalves", "Phytoplankton"),
                 Regions=NULL, Phyt_start = 1950)

## Benthic ----------------------------------------------------------------

## Removing the L, R, and C stations and just combining them into 1 summary statistic

Benthic<-Data$Bivalves%>%
  select(Date, Station, Source, Latitude, Longitude)%>%
  separate(Station, into=c("Station", "Station2"), sep="-")%>%
  mutate(Year=year(Date),
         Station2=Station)%>%
  group_by(Year, Station, Station2, Source)%>%
  summarise(N=n(), .groups="drop")%>%
  mutate(Parameter="Benthic")%>%
  left_join(WQ%>%
              filter(Source=="EMP")%>%
              select(Station, Latitude, Longitude)%>%
              distinct(),
            by="Station")

## Phytoplankton ----------------------------------------------------------------


Phytoplankton<-Data$Phytoplankton%>%
  select(Date, Station, Source, Latitude, Longitude)%>%
  mutate(Year=year(Date),
         Station=case_when(
           str_detect(Station, "EZ6 SJR") ~ "EZ6 SJR",
           str_detect(Station, "EZ2 SJR") ~ "EZ2 SJR",
           str_detect(Station, "EZ2") ~ "EZ2",
           str_detect(Station, "EZ6") ~ "EZ6",
           TRUE ~ Station),
         Station2=if_else(str_detect(Station, "EZ"), paste(Station, Date), Station))%>%
  group_by(Year, Station, Station2, Source)%>%
  summarise(N=n(), .groups="drop")%>%
  mutate(Parameter="Phytoplankton")%>%
  left_join(WQ%>%
              filter(Source=="EMP")%>%
              select(Station, Station2, Latitude, Longitude)%>%
              distinct(),
            by=c("Station", "Station2"))%>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Match EMP zoop to WQ stations -------------------------------------------

zoop_stations<-zooper::stations%>%
  filter(Source=="EMP" & !is.na(Latitude) & !is.na(Longitude))%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)%>%
  st_transform(crs=26910)

WQ_stations<-WQ%>%
  filter(Source=="EMP" & !str_detect(Station, "EZ"))%>%
  distinct(Station, Latitude, Longitude)%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)%>%
  st_transform(crs=26910)

match<-st_nearest_feature(zoop_stations, WQ_stations)

dist<-st_distance(zoop_stations, WQ_stations[match,], by_element=T)

zoop_stations_match<-zoop_stations%>%
  mutate(WQ_station=WQ_stations$Station[match],
         Distance=as.numeric(dist))%>%
  st_drop_geometry()%>%
  left_join(WQ_stations%>%
              st_drop_geometry()%>%
              select(WQ_lat=Latitude, WQ_lon=Longitude, Station),
            by=c("WQ_station"="Station"))


#ggplot(data=filter(zoop_stations_match, Distance<=1000))+
#  #geom_sf(data=deltamapr::WW_Delta%>%st_transform(crs=4326))+
#  geom_point(aes(y=Latitude, x=Longitude, color=Distance), shape=16)+
#  geom_point(aes(y=WQ_lat, x=WQ_lon, color=Distance), shape=17)+
#  geom_segment(aes(y=Latitude, x=Longitude, yend=WQ_lat, xend=WQ_lon))+
#  scale_color_viridis_c()+
#  theme_bw()

zoop_stations_match_final<-zoop_stations_match%>%
  filter(Distance<=1000)%>%
  mutate(Source="EMP")

Zoop_final<-Zoop%>%
  left_join(zoop_stations_match_final%>%
              select(Source, Station, WQ_station, WQ_lat, WQ_lon),
            by=c("Source", "Station"))%>%
  mutate(Station=if_else(is.na(WQ_station), Station, WQ_station),
         Station2=if_else(is.na(WQ_station), Station2, WQ_station),
         Latitude=if_else(is.na(WQ_station), Latitude, WQ_lat),
         Longitude=if_else(is.na(WQ_station), Longitude, WQ_lon))%>%
  select(-WQ_station, -WQ_lat, -WQ_lon)


# Create master list of other EMP station names ---------------------------

zoop_stations_final<-zoop_stations_match_final%>%
  select(Station, WQ_station)%>%
  distinct()%>%
  rename(Zoop_station=Station, Station=WQ_station)

Benthic_stations<-Data$Bivalves%>%
  select(Station)%>%
  distinct()%>%
  separate(Station, into=c("Station", "Benthic_station"), sep="-")%>%
  mutate(Benthic_station=paste(Station, Benthic_station, sep="-"))%>%
  group_by(Station)%>%
  summarise(Benthic_station=list(unique(Benthic_station)))

BenZoop_stations<-full_join(zoop_stations_final, Benthic_stations,
                            by="Station")


# CDEC --------------------------------------------------------------------

CDEC_stations<-read_excel("data/Water Quality Delta Stations.xlsx")%>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F)%>%
  st_transform(crs=st_crs(deltamapr::R_EDSM_Subregions_Mahardja))%>%
  st_filter(deltamapr::R_EDSM_Subregions_Mahardja)%>%
  st_drop_geometry()%>%
  mutate(Station=paste0(station_name, " (", station_id, ")"),
         start_year=year(start_date),
         end_year=year(end_date),
         Year=start_year)%>%
  rename(Latitude=latitude, Longitude=longitude, Sensor=sensname)%>%
  group_by(Station, Latitude, Longitude, Sensor)%>%
  complete(Year=full_seq(c(start_year, end_year), 1))%>%
  ungroup()%>%
  select(Station, Latitude, Longitude, Year, Sensor)

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

# Merge everything --------------------------------------------------------

Stations<-bind_rows(Benthic, Phytoplankton, Zoop_final, WQ, Fish)%>%
  mutate(Source=recode(Source, twentymm="20mm", USGS="SFBS", USBR="SDSCS"),
         Station=recode(Station, NZEZ2="EZ2", NZEZ6="EZ6", NZEZ2SJR="EZ2-SJR", NZEZ6SJR="EZ6-SJR"),
         StationID=paste(Source, Station2))

# Multiples<-Stations%>%
#   group_by(StationID)%>%
#   summarise(N_Lat=n_distinct(Latitude), N_Lon=n_distinct(Longitude), .groups="drop")%>%
#   filter(N_Lat>1 | N_Lon>1)%>%
#   distinct(StationID)
# 
# Multiples2<-Stations%>%
#   filter(StationID%in%Multiples$StationID)%>%
#   distinct(StationID, Latitude, Longitude)%>%
#   group_by(StationID)%>%
#   mutate(ID=1:2)%>%
#   ungroup()%>%
#   pivot_wider(names_from = c(ID), values_from=c(Latitude, Longitude))
# 
# ggplot()+
#   geom_sf(data=deltamapr::WW_Delta%>%st_transform(crs=4326))+
#   geom_point(data=Multiples2, aes(x=Longitude_1, y=Latitude_1), color="blue")+
#   geom_point(data=Multiples2, aes(x=Longitude_2, y=Latitude_2), color="red")+
#   geom_segment(data=Multiples2, aes(x=Longitude_2, y=Latitude_2, xend=Longitude_1, yend=Latitude_1), color="green", size=2)

Stations_final<-Stations%>%
  group_by(StationID)%>%
  mutate(Latitude=mean(Latitude), Longitude=mean(Longitude), N_years=length(unique(Year)))%>%
  ungroup()%>%
  pivot_wider(names_from = Parameter, values_from=N)%>%
  mutate(across(c(Benthic, Phytoplankton, Zooplankton, Water_quality, Fish), ~replace_na(.x, 0)))%>%
  left_join(BenZoop_stations, by="Station")%>%
  rowwise()%>%
  mutate(Max=max(Benthic, Phytoplankton, Zooplankton, Water_quality, Fish),
         Extra_stations=case_when(
           is.null(unlist(Benthic_station)) & is.na(Zoop_station) ~ NA_character_,
           !is.null(unlist(Benthic_station)) & is.na(Zoop_station) ~ paste0("Benthic: ", paste0(unlist(Benthic_station), collapse=", ")),
           is.null(unlist(Benthic_station)) & !is.na(Zoop_station) ~ paste0("Zooplankton: ", Zoop_station),
           TRUE ~ paste0(paste0("Benthic: ", paste0(unlist(Benthic_station), collapse=", ")), " | ", paste0("Zooplankton: ", Zoop_station))
         ))%>%
  ungroup()

saveRDS(Stations_final, file="app/Sampling.Rds")
