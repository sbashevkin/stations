require(dplyr)
require(deltareportr)
require(lubridate)
require(stringr)
require(discretewq)
require(readr)
require(tidyr)

# Bivalves and phytoplankton ----------------------------------------------


Data<-DeltaDater(Start_year=1950, End_year=2021, 
                 Variables = c("Bivalves", "Phytoplankton"),
                 Regions=NULL, Phyt_start = 1950)

## Bivalves ----------------------------------------------------------------


Bivalves<-Data$Bivalves%>%
  filter(!is.na(Latitude) & !is.na(Longitude))%>%
  select(Date, Station, Source, Latitude, Longitude)%>%
  separate(Station, into=c("Station", "Station2"), sep="-")%>%
  mutate(Year=year(Date),
         Station2=paste(Station, Station2, sep="-"))%>%
  group_by(Year, Station, Station2, Source)%>%
  summarise(Latitude=unique(Latitude), Longitude=unique(Longitude), N=n(), .groups="drop")%>%
  mutate(Parameter="Bivalves")

## Phytoplankton ----------------------------------------------------------------


Phytoplankton<-Data$Phytoplankton%>%
  filter(!is.na(Latitude) & !is.na(Longitude))%>%
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
  summarise(Latitude=unique(Latitude), Longitude=unique(Longitude), N=n(), .groups="drop")%>%
  mutate(Parameter="Phytoplankton")


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
  bind_rows(EDSM, DJFMP, YBFMP_WQ)%>%
  mutate(Parameter="Water_quality")


## Fish --------------------------------------------------------------------


Fish<-WQ%>%
  filter(Source%in%c("STN", "FMWT", "EDSM", "DJFMP", "SKT", "20mm", "Suisun",
                     "Baystudy"))%>%
  bind_rows(YBFMP_fish)%>%
  mutate(Parameter="Fish")


# Merge everything --------------------------------------------------------

Stations<-bind_rows(Bivalves, Phytoplankton, Zoop, WQ, Fish)%>%
  mutate(StationID=paste(Source, Station2))

Multiples<-Stations%>%
  group_by(StationID)%>%
  summarise(N_Lat=n_distinct(Latitude), N_Lon=n_distinct(Longitude), .groups="drop")%>%
  filter(N_Lat>1 | N_Lon>1)%>%
  distinct(StationID)

Multiples2<-Stations%>%
  filter(StationID%in%Multiples$StationID)%>%
  distinct(StationID, Latitude, Longitude)%>%
  group_by(StationID)%>%
  mutate(ID=1:2)%>%
  ungroup()%>%
  pivot_wider(names_from = c(ID), values_from=c(Latitude, Longitude))

ggplot()+
  geom_sf(data=deltamapr::WW_Delta%>%st_transform(crs=4326))+
  geom_point(data=Multiples2, aes(x=Longitude_1, y=Latitude_1), color="blue")+
  geom_point(data=Multiples2, aes(x=Longitude_2, y=Latitude_2), color="red")+
  geom_segment(data=Multiples2, aes(x=Longitude_2, y=Latitude_2, xend=Longitude_1, yend=Latitude_1), color="green", size=2)
  
Stations_final<-Stations%>%
  group_by(StationID)%>%
  mutate(Latitude=mean(Latitude), Longitude=mean(Longitude))%>%
  pivot_wider(names_from = Parameter, values_from=N)%>%
  mutate(across(c(Bivalves, Phytoplankton, Zooplankton, Water_quality, Fish), ~replace_na(.x, 0)))
