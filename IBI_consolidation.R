library(tidyverse); library(dataRetrieval); library(lubridate); library(sf) # load libraries
library(readxl); library(cowplot); library(MetBrewer)

# want to create a spreadsheet with info on taxa, ibi, site, date, location
ibis<-NULL
# locations of our sites
sites<-read.csv('../DeclinesMussels/data/site_loc.csv') %>% 
  bind_rows(read.csv('IBI_data/extraMNsites.csv') %>% mutate(State = 'Minnesota')) %>% 
  filter(!is.na('Latitude'), !is.na(Longitude), !is.na(Stream)) %>%
  st_as_sf(coords=c('Longitude','Latitude'), crs = 4269)

# Alabama ===============
# downloaded fish data from https://warcapps.usgs.gov/SHU/Map#
# pdf sent by PJ has Flint north of Huntsville (FLTM) as IBI = 38, Fair
ibis <- bind_rows(ibis, 
                  read.csv('IBI_data/AL_fish_ibi.csv') %>%
                    rename(StreamName = Site, IBICat=Ranking, IBIScore=Score) %>%
                    mutate(taxa='fish') %>%
                    st_as_sf(coords=c('Long','Lat'), crs=4269))

# ibi from TVA
al_fish_tva<-read_excel('IBI_data/AL_Flint_Paint_Rock _Rivers_TVA_IBI_Data.xlsx', sheet=4) %>%
  st_as_sf(coords=c('LONG  DEC', 'LAT  DEC'), crs=4269) 
# look at the proximity of the sites
ggplot()+geom_sf(data=sites %>% filter(SiteCode %in% c('ALFLT','ALPRK')), color='red')+
  geom_sf(data=al_fish_tva, aes(color=`STATION  DESCRIPTION`), alpha=0.4)
# which station names
al_fish_tva[st_nearest_feature(sites[sites$SiteCode %in% c('ALFLT','ALPRK'),],al_fish_tva),] %>%  pull(`STATION  DESCRIPTION`)
al_fish_tva %>% filter(`STATION  DESCRIPTION` %in% c('Hwy 72', 'Butler Mill Road Bridge')) %>% View()
al_fish_tva %>% filter(`RIVER MILE` < 40, `RIVER MILE` > 10) # could get more recent time periods and within 20miles
# different from GSA provided IBI scores
#View(al_fish_tva %>% filter(`STATION  DESCRIPTION` %in% c('Hwy 72', 'Butler Mill Road Bridge')))
#View(ibis %>% filter(SiteCode %in% c('ALFLT','ALPRK')))
# dropping the EPT scores because I don't know how to reconcile with IBI ranks
ibis <- bind_rows(ibis, 
                  al_fish_tva %>%
                    filter(`STATION  DESCRIPTION` %in% c('Hwy 72', 'Butler Mill Road Bridge')) %>%
                    mutate(SiteCode = ifelse(STREAM == 'Paint Rock River', 'ALPRK', 'ALFLT'),
                           Notes = paste('River mile: ', `RIVER MILE`, ' source TVA'), 
                           Date = format(`SAMPLE DATE`, '%m/%d/%Y')) %>%
                    rename(StreamName = `STATION  DESCRIPTION`,
                           IBICat=`IBI RATING`, IBIScore=`IBI SCORE`) %>%
                    select(Date, StreamName, IBICat, IBIScore, Notes, SiteCode) %>%
                    mutate(taxa='fish'))

# AL macroinvertebrates from ADEM
al_m<-read_excel('IBI_data/AL_ADEM Macroinvertebrate data.xlsx') %>%
  st_as_sf(coords=c('LONGITUDE','LATITUDE'), crs=4269)
# find the closest stations
st_nearest_feature(sites %>% filter(State == 'Alabama'), al_m %>% distinct(LOCATION_DESC, LOCALE_NAME, .keep_all = T))
al_m %>% distinct(LOCATION_DESC, LOCALE_NAME, .keep_all = T) %>% slice(c(1,5,4))%>% pull(STATION)
al_m %>% as_tibble() %>% count(STATION, TEXT_VISIT_DATE) #check the date range of the available data

# hand calculated IBI scores using AL ADEM SOP #6004
al_submet<-al_m %>% as_tibble() %>%
  filter(STATION %in% c("BCNS-24", "TEPC-1A", "FLTM-2"),
         richness == 'y') %>% # some taxa are not included 
  mutate(comid=paste(STATION, TEXT_VISIT_DATE, sep='_')) %>% # create a unique identifier to join with diversity index
  group_by(STATION, TEXT_VISIT_DATE, comid) %>% 
  summarize(ept=sum(ord %in% c('Ephemeroptera', 'Plecoptera', 'Trichoptera'), na.rm=T),# count of ept taxa
       tottax=length(unique(TAXON_NAME)), # count of total taxa
       noninstax=sum(ord %in% c(NA, 'isopoda','gastropoda','collembola','heterodonta',
                                'mesogastropoda','amphipoda','limnophila','oligochaeta'), na.rm=T), # count of noninsect taxa
       nonisnptax=noninstax/tottax*100,
       toltax=sum(Tol >= 6.5, na.rm=T), # count to tolerant taxa, with tolerant have H scores > 6.5
       tolptax=toltax/tottax*100, .groups='drop') %>% arrange(comid) 
# use vegan to calculate shannon
library(vegan); ?diversity
# create a community matrix, with each sample being its own row
com.df<-al_m %>% as_tibble() %>%
  filter(STATION %in% c("BCNS-24", "TEPC-1A", "FLTM-2"), richness == 'y') %>%
  mutate(comid=paste(STATION, TEXT_VISIT_DATE, sep='_')) %>%
  select(comid, TAXON_NAME, TAXON_COUNT) %>%
  pivot_wider(names_from = TAXON_NAME, values_from=TAXON_COUNT) 
com.df[is.na(com.df)]<-0 # replace NA with 0
al_shan<-bind_cols(com.df[,1], shannon=diversity(com.df[,-1])) #calculate shannon diversity index
# count the percent predators in each sample
sample.ffg<-al_m %>% filter(STATION %in% c("BCNS-24", "TEPC-1A", "FLTM-2")) %>%
  group_by(STATION, TEXT_VISIT_DATE) %>%
  pivot_wider(names_from=FFG, values_from = TAXON_COUNT) %>%
  summarize(totalP=sum(`5`, na.rm=T), totalCount=sum(c(`5`,`4`,`3`,`2`,`1`,`NA`,`6`), na.rm = T),.groups = 'drop') %>%
  mutate(precpct=totalP/totalCount*100) 
# count the percent of ept taxa - baetidae and helicopsychidae
ept.ind<-al_m %>% as_tibble() %>% filter(STATION %in% c("BCNS-24", "TEPC-1A", "FLTM-2")) %>%
  group_by(STATION, TEXT_VISIT_DATE) %>% 
  mutate(total.ind=sum(TAXON_COUNT, na.rm=T)) %>%
  filter(ord %in% c('Ephermeroptera', 'Plecoptera', 'Trichoptera'), 
         !(fam %in% c('Baetidae', 'baetidae', 'hydropsychidae'))) %>%
  summarize(total.ind=unique(total.ind), eptminHB=sum(TAXON_COUNT), 
            eptminhb=eptminHB/total.ind*100, .groups='drop')
# combine all the submetrics together
al_submet<-left_join(al_submet, al_shan, by='comid') %>%
  left_join(sample.ffg, by=c('STATION', 'TEXT_VISIT_DATE')) %>%
  left_join(ept.ind, by=c('STATION', 'TEXT_VISIT_DATE')) %>%
  rowwise() %>% # and calculate each scaled metric that constitutes the total IBI score
  mutate(EPTtax=max(c(0, min(c(100, 100*(ept-4)/23)))), 
         NonInsPTax=max(c(0, min(c(100,100*(22.8-nonisnptax)/20.3)))), 
         Shannon=max(c(0, min(c(100,100*(shannon-2.7)/2.15)))), 
         EPTPctminHB=max(c(0, min(c(100,100*(eptminhb-0.8)/44.8)))),
         TolerantPTax=max(c(0, min(c(100,100*(48.3-tolptax)/33.5)))),
         PredatorsPct=max(c(0, min(c(100,100*(precpct-1.9)/21.5))))) # not included in river valley
ibis<-bind_rows(ibis, # combine with the big dataset
                al_submet %>% group_by(STATION, TEXT_VISIT_DATE) %>%
  summarize(IBIScore=ifelse(STATION %in% c('BCNS-24', 'TEPC-1A'), 
                            mean(c(EPTtax, NonInsPTax, Shannon, EPTPctminHB, TolerantPTax)),
                            mean(c(EPTtax, NonInsPTax, Shannon, EPTPctminHB, TolerantPTax, PredatorsPct))),
            IBICat=case_when(STATION %in% c('BCNS-24', 'TEPC-1A') & IBIScore <= 46 ~ 'poor',
                             STATION %in% c('BCNS-24', 'TEPC-1A') & IBIScore > 47 ~ 'fair',
                             STATION =='FLTM-2' & IBIScore > 44 ~ 'good'), 
            SiteCode = case_when(STATION == 'BCNS-24' ~ 'ALBCA',
                                 STATION == 'TEPC-1A' ~ 'ALTER',
                                 STATION =='FLTM-2' ~ 'ALFLT') ) %>%
    left_join(al_m %>% distinct(STATION, geometry), by='STATION') %>%
    mutate(Date=format(TEXT_VISIT_DATE, '%m/%d/%Y'), taxa='macroinvertebrates') %>%
    rename(StreamName = STATION) %>% select(-TEXT_VISIT_DATE))
rm(al_shan, com.df, ept.ind, sample.ffg)# removing unneeded data frames

# Georgia =========
# first find most appropriate sites
x<-findNLDI(location = sites[sites$SiteCode=='GACON',], nav = c("UM", "UT", 'DM'), find = c("flowlines"), distance_km = 10)
y<-findNLDI(location = sites[sites$SiteCode=='GAARM',], nav = c("UM", "UT", 'DM'), find = c("flowlines"), distance_km = 10)
# searched for the data on GAEP database for those sites
ga_m_loc<-read_excel('IBI_data/GAEPD_Report_2023-10-12_03-48-43-PM.xlsx') %>%
  filter(Status != 'Excluded' | is.na(Status), Parameter == 'Count') %>%
  count(`Monitoring Location`, `Monitoring Location ID`, `Date`) %>%
  left_join(read_excel('IBI_data/GAEPD_Report_2023-10-12_03-48-43-PM.xlsx', sheet=2)) %>%
  st_as_sf(coords=c('Longitude', 'Latitude'), crs=4269) %>%
  mutate(yr=year(Date))
# this code is investigating which stations / data to use
st_nearest_feature(sites[sites$State == 'Georgia',], ga_m_loc)
st_nearest_feature(sites[sites$State == 'Georgia',], ga_m_loc %>% 
                     filter(`Ecoregion Level 4 Number`!= '66g', Date > ymd('2010-01-01')))
which(ga_m_loc$`Monitoring Location ID` %in% (ga_m_loc %>% filter(`Ecoregion Level 4 Number`!= '66g', Date > ymd('2010-01-01')) %>% 
  slice(1:2) %>% pull(`Monitoring Location ID`)))
ggplot()+
  geom_sf(data=x$DM_flowlines)+geom_sf(data=y$DM_flowlines)+
  geom_sf(data=ga_m_loc, aes(color=yr))+
  geom_sf_text(data=ga_m_loc[c(7,21,48,20),], color='purple', aes(label=`Monitoring Location ID`))+
               # aes(label=`Ecoregion Level 4 Number`))+
  geom_sf_text(data=sites[sites$State == 'Georgia',], aes(label=SiteCode),
               color='red', alpha=0.5, size=3)+
  theme_classic()
ga_m_loc[c(7,21,48,20),] %>% select(`Monitoring Location`, starts_with('Ecoregion')) # which IBI score do I need to use
ga_m<-read_excel('IBI_data/GAEPD_Report_2023-10-12_03-48-43-PM.xlsx') %>% # bring in the data
  filter(`Monitoring Location ID` %in% ga_m_loc[c(7,21,48,20),]$`Monitoring Location ID`,
         Parameter=='Count')
unique(ga_m$`Taxon Name`)
View(ga_m %>% select(`Monitoring Location ID`, `Monitoring Location`,
                     Date, Parameter, `Result (Pick List)`, `Result (Value)`, Units,
                     `Taxon Name`, Status, `Life Stage`))
# build a taxa table to help reconcile typos
ga_m_wt<-ga_m %>% 
  filter(Status != 'Excluded' | is.na(Status)) %>%
  select(`Monitoring Location`, `Monitoring Location ID`, Date, `Result (Value)`, Units, `Taxon Name`) %>%
  # fix some known taxa issues
  mutate(TaxonName = case_when(`Taxon Name` == 'Potthastia longimanus' ~ 'Potthastia longimana',
                               `Taxon Name` == 'Caenis diminuta group' ~ 'Caenis',
                               `Taxon Name` == 'Elimia clavaeformis' ~ 'Elimia',
                               `Taxon Name` == 'Naididae' ~ 'Naidinae', # assuming this is the correct subfamily :(
                               `Taxon Name` %in% c('Paratanytarsus longistilus', 'Stempellinella fimbriata',
                                                   'Nanocladius (Plecopteracoluthus)', 'Cricotopus (Cricotopus)') ~ 
                                 gsub(' .*', '', `Taxon Name`),
                               `Taxon Name` == 'Pteronarcys' ~ 'Pteronarcys dorsata',
                               `Taxon Name` == 'Anafroptilum' ~ 'Centroptilum', #assuming similair genus
                               `Taxon Name` == 'Cinygmula subaequalis' ~'Heptageniidae',
                               `Taxon Name` == 'Drunella' ~ 'Ephemerellidae',
                               `Taxon Name` == 'Heterocloeon amplum' ~ 'Heterocloeon',
                               `Taxon Name` == 'Heterotrissocladius cladwell/boltoni complex' ~ 'Heterotrissocladius cladwell/boltoni complex',
                               `Taxon Name` == 'Bezzia complex' ~ 'Bezzia complex',
                                   T ~ gsub(' complex', '', `Taxon Name`))) %>%
  left_join(read_xlsx('IBI_data/ga_taxa.xlsx'), by=c('TaxonName' = 'FinalID')) #%>%
ga_m_wt %>% filter(is.na(FFG)) %>% pull(TaxonName) %>% unique() # I have functional feeding groups for all taxa
ga_smp<-ga_m_wt %>% count(`Monitoring Location ID`, Date)
ga_m_loc[c(7,21,48,20),] %>% select(`Monitoring Location ID`, starts_with('Ecoregion')) %>% arrange(`Ecoregion Level 4 Number`)

# for loop guts for 67g
ga67g<-NULL
for(i in 1:2){ # doing this by rows
  ga.sit.dt<-left_join(ga_smp[ga_smp$`Monitoring Location ID` != 'RV_14_4846',][i,], ga_m_wt)
hydro.c<-ga.sit.dt %>% filter(Family == 'Hydropsychidae') %>% pull(`Result (Value)`) %>% as.numeric() %>% sum()
tric.c<-ga.sit.dt %>% filter(Order == 'Trichoptera') %>% pull(`Result (Value)`) %>% as.numeric() %>% sum()
ortho.c<-ga.sit.dt %>% filter(Family == 'Orthocladiinae') %>% pull(`Result (Value)`) %>% as.numeric() %>% sum()
chiron.c<-ga.sit.dt %>% filter(Order == 'Chironomidae') %>% pull(`Result (Value)`) %>% as.numeric() %>% sum()
ga67g<-bind_rows(ga67g,
                 data.frame(rowid=i,
                            `Monitoring Location ID` = ga_smp[ga_smp$`Monitoring Location ID` != 'RV_14_4846',][i,1],
                            Dt=ga_smp[ga_smp$`Monitoring Location ID` != 'RV_14_4846',][i,2],
                            nPlecT=sum(ga.sit.dt$Order == 'Plecoptera', na.rm=T)/5.18*100,#Plecoptera Taxa
                            hydro.per = (92-(hydro.c/tric.c*100))/92*100, # % Hydropsychidae/Total Trichoptera
                            ortho.per = ifelse(is.nan(ortho.c/chiron.c), 0, ortho.c/chiron.c*100)/79*100, #% Orthocladiinae/ Total Chironomidae
                            nShredT= sum(ga.sit.dt$FFG == 'SH', na.rm=T)/5*100, #Shredder Taxa
                            nCollT = sum(ga.sit.dt$FFG == 'CG', na.rm=T)/24.55*100, #Collector Taxa
                            nSpraT = sum(ga.sit.dt$Habit %in% c('SP'), na.rm=T)/15.1*100 #Sprawler Taxa
                 ))
}
ga67gF<-ga67g %>%
  mutate(across(nPlecT:nSpraT, function(x){ifelse(x > 100, 100, x)})) %>%
  rowwise() %>%
  mutate(IBIScore = round(mean(c(nPlecT, hydro.per, ortho.per, nShredT, nCollT, nSpraT))),
         SiteCode='GAARM')

# for loop guts for 67f
ga67f<-NULL
for(i in unique(ga_m_wt[ga_m_wt$`Monitoring Location ID` == 'RV_14_4846',]$Date)){ # doing this by date since one site
  ga.sit.dt<-ga_m_wt %>% filter(`Monitoring Location ID` == 'RV_14_4846', Date == i)
  tot.ind<-ga.sit.dt %>% pull(`Result (Value)`) %>% as.numeric() %>% sum()
  tot.ept.ind<- ga.sit.dt %>% 
  filter(Order %in% c('Ephemeroptera','Plecoptera','Trichoptera')) %>%
           pull(`Result (Value)`) %>% as.numeric() %>% sum()
  cling.ind<-ga.sit.dt %>% filter(Habit == 'CN') %>% pull(`Result (Value)`) %>% as.numeric() %>% sum()
  ncbi<-ga.sit.dt %>% filter(!is.na(TolValNC)) %>%
    mutate(nctol=as.numeric(`Result (Value)`)*TolValNC) %>% pull(nctol) %>% sum(., na.rm=T)
  tot.ind.nc<-ga.sit.dt %>% filter(!is.na(TolValNC)) %>% pull(`Result (Value)`) %>% as.numeric() %>% sum()
 ga67f<-bind_rows(ga67f,
                  data.frame(`Monitoring Location ID` = 'RV_14_4846',
                             Date=unique(ga.sit.dt$Date),
                             nEPTtax=sum(ga.sit.dt$Order == 'Plecoptera' |
                        ga.sit.dt$Order == 'Ephemeroptera' | ga.sit.dt$Order == 'Trichoptera', na.rm=T)/16.6*100,#EPT Taxa
                        nPlecT=sum(ga.sit.dt$Order == 'Plecoptera', na.rm=T)/6*100, #Plecoptera Taxa	
                        perEPT = (tot.ept.ind/tot.ind*100)/48.5*100, #% EPT	
                        NCBI = (7.23-(ncbi/tot.ind.nc))/(7.23-3.70941)*100,#NCBI	
                        nScrapT=sum(ga.sit.dt$FFG =='SC', na.rm=T)/7*100, #Scraper Taxa	
                        perCling = (cling.ind/tot.ind*100)/55.66667*100 #% Clinger	
                        ))
}
ga67fF<-ga67f %>%
  mutate(across(nEPTtax:perCling, function(x){ifelse(x > 100, 100, x)})) %>%
  rowwise() %>%
  mutate(IBIScore = round(mean(c(nEPTtax, nPlecT, perEPT, NCBI, nScrapT, perCling))),
         IBICat = 'Very Good', SiteCode = 'GACON') 

ibis<-bind_rows(ibis,
          bind_rows(ga67fF, ga67gF) %>% 
  select(Monitoring.Location.ID, Date, IBIScore, IBICat, SiteCode) %>%
  left_join(ga_m_loc %>% select(starts_with('Monitoring'), 'Date'), 
            by=c("Monitoring.Location.ID"="Monitoring Location ID", 'Date')) %>%
  select(-`Monitoring Location Type`) %>%
  rename(StreamName=`Monitoring Location`, Notes=Monitoring.Location.ID) %>%
  mutate(taxa='macroinvertebrates', 
         Date=substr(Date, 1,10)))
# removing unneeded data frames
rm(ga_m, ga_smp, ga67f, ga67g, ga.sit.dt, ga67fF, ga67gF, tot.ind.nc, tric.c, 
   chiron.c, cling.ind, hydro.c, i, ncbi, ortho.c, tot.ept.ind, tot.ind)
# georgia fish IBI data
ga_fish<-read_excel("IBI_data/GA_Whitfiled County.xls") %>%
  bind_rows(read_excel("IBI_data/GA_Floyd County IBI.xls")) %>%
  st_as_sf(coords=c('X','Y'), crs=4269)
st_nearest_feature(sites[sites$State == 'Georgia',], ga_fish)
plot_grid(
  ggplot()+geom_sf(data=x$UT)+ geom_sf(data=ga_fish[c(11),]) +
    geom_sf(data=sites[sites$SiteCode=='GACON',], color='red', alpha=0.5, size=3),
  ggplot()+geom_sf(data=y$UT)+ geom_sf(data=y$DM)+geom_sf(data=ga_fish[c(38,45),]) +
    geom_sf(data=sites[sites$SiteCode=='GAARM',], color='red', alpha=0.5, size=3))
ga_fish[c(11,38),"Date"]

ga_fish[c(11,38,45),] %>%
  select(-FMRegion, -EcoRegion, -Basin) %>%
  mutate(siteCode = c('GACON','GAARM','GAARM')) %>% View()
which(grepl('Armuchee', ga_fish$StreamName))
ibis <- bind_rows(ibis, ga_fish[c(11,38,45),] %>%
                    select(StreamName, Date, IBIScore, IBICat) %>%
                    mutate(SiteCode = c('GACON','GAARM','GAARM'),
                           taxa='fish',
                           Date=as.character(Date)))

# Illinois ============
# IL 2021 sites
il_m<-read.csv('IBI_data/IL_ibi.csv') %>%
  st_as_sf(coords=c("Monitoring.Location.Longitude", "Monitoring.Location.Latitude"),crs=4269) # mIBI
# trying to find the closest locations
x<-findNLDI(location = sites[sites$SiteCode=='ILMFV',], 
            nav = c("UM", "UT", 'DM'), find = c("flowlines"), distance_km = 10)
y<-findNLDI(location = sites[sites$SiteCode=='ILSFV',], 
            nav = c("UM", "UT", 'DM'), find = c("flowlines"), distance_km = 10)
st_nearest_feature(sites[sites$State == 'Illinois' & sites$SiteCode != 'ILEMB', ], il_m)
ggplot()+
  geom_sf(data=x$UT_flowlines, color='black')+geom_sf(data=y$UT_flowlines)+
  geom_sf(data=sites[sites$State == 'Illinois' &
                       !(sites$SiteCode %in% c('ILEMB','ILKIL','ILSBK','ILPIS','ILCOO','ILKIS','ILSAN')), ], 
          color='red',size=3, alpha=0.5)+
  geom_sf(data=il_m, aes(color=Index.Score))+
  theme_void()
ibis<-bind_rows(ibis %>% filter(!is.na(taxa)),
                il_m %>% select(Monitoring.Location.Name, Date, Index.Score) %>%
  mutate(Date = as.character(Date), taxa='macroinvertebrates',
         SiteCode=c('ILMFV','ILSFV', 'ILSFV','ILMFV')) %>%
  rename(IBIScore = Index.Score, StreamName = Monitoring.Location.Name))
# second round of IBI scores for 2023 IL sites
il_2<-read_excel('IBI_data/IL_ibis_09282023.xlsx', sheet=2) %>%
  mutate(Date=substr(Date, 1, 10), taxa='macroinvertebrates') %>%
  left_join(sites, by=c('Monitoring Location Name'='Stream'))
ibis<-bind_rows(ibis,
          il_2 %>% select(Date, `Monitoring Location Name`, `Monitoring Location Latitude`, `Monitoring Location Longitude`,
                `Index Score`,  SiteCode, taxa) %>%
            rename('StreamName' = 'Monitoring Location Name', 'IBIScore'='Index Score') %>%
            st_as_sf(coords=c('Monitoring Location Longitude', 'Monitoring Location Latitude'), crs=st_crs(ibis)))
rm(x,y)
          
# fish data
il_f <- read_excel('IBI_data/IL_fish IBI Results for Traci Dubose.xlsx', sheet=3)  %>%
  st_as_sf(coords=c('Longitude','Latitude'), crs=st_crs(sites)) %>%
  rename('StreamName' = 'Waterbody Name', 'IBIScore'='fIBI', 
         'IBICat' = 'Biotic Integrity Class', 'Date'='Sample Date') %>%
  mutate(Notes=paste(ifelse(`Was fIBI extrapolated beyond defined relationship?`=='Yes', 'extrapolated', ''),
                     ifelse(`Was fIBI corrected for small sample size?` == 'Yes', 'small sample size', ''), 
                     `IBI Region`, `Station Code`),
         taxa='fish') %>%
  select(StreamName, IBIScore, IBICat, Date, Notes, taxa) %>% 
  st_join(sites %>% filter(State == 'Illinois') %>% select(Stream, SiteCode, State),
                 join=st_nearest_feature)
il_f %>%
  ggplot()+geom_sf(color='red', alpha=0.3)+
  geom_sf(data=sites %>% filter(State=='Illinois'), color='black', alpha=0.3)

ibis<-bind_rows(ibis, il_f %>% mutate(Date=format(Date, '%Y-%m-%d')) %>%
                  select(-Stream, -State))

# Indiana ============
in_fish<-NULL # indiana sent me individual files for each basin, this brings them in
for(i in list.files('IBI_data', pattern='IDEM Fish', full.names = T)){
  in_fish<-bind_rows(in_fish,
          read_excel(i, sheet=1) %>%
            left_join(read_excel(i, sheet=3) %>% # locational information
                        filter(grepl('IBI', METRIC_NAME))%>%
                        select(-METRIC_NAME) %>% rename(IBI=METRIC_VALUE),
                      by=c('ACTIVITY_RID','EVENT_NUM')))}
in_fish <- in_fish %>% st_as_sf(coords=c('LONGITUDE_MEASURE', 'LATITUDE_MEASURE'), crs=4269)
# investigating where scores are available
ggplot()+geom_sf(data=in_fish)+
  geom_sf(data=sites[sites$State == 'Indiana'& sites$SiteCode != 'INWBA',], # doesn't have a match
          alpha=0.5, color='red', size=3)
st_nearest_feature(sites[sites$State == 'Indiana',], in_fish)
in_fish_stat<-st_join(sites[sites$State == 'Indiana',], in_fish, join=st_nearest_feature) %>%
  filter(County == COUNTY_NAME) %>%
  select(SiteCode, Stream, County, STATION_NAME)
in_fish %>% filter(STATION_NAME %in% in_fish_stat$STATION_NAME) %>%
  group_by(STATION_NAME) %>% summarize(n=n(),
                                       dts=paste(ACTIVITY_END_DATE, collapse = '; '))
# duplicated station for Wildcat Creek

ibis<-bind_rows(ibis %>% filter(!is.na(taxa), !is.na(SiteCode)),
               in_fish %>% filter(STATION_NAME %in% in_fish_stat$STATION_NAME) %>%
  rename(County=COUNTY_NAME, IBIScore=IBI, StreamName=WATERBODY_NAME) %>%
  mutate(Date=substr(ACTIVITY_END_DATE, 1,10), taxa = 'fish') %>%
  left_join(in_fish_stat %>% as_tibble() %>% select(-geometry),
            relationship='many-to-many', by=c('STATION_NAME', 'County')) %>%
  select(StreamName, STATION_NAME, SiteCode, Date, IBIScore, taxa) %>%
  rename(Notes=STATION_NAME))

list.files(pattern='IDEM_Water', recursive = T)
in_fish2<-read.csv('IBI_data/IDEM_Water_Quality_Assessment_Information_Management_System (1).csv') %>%
  bind_rows(read.csv('IBI_data/IDEM_Water_Quality_Assessment_Information_Management_System (2).csv')) %>%
  st_as_sf(coords=c('longitude_measure','latitude_measure'), crs=4269)
in_fish2_loc<-st_join(sites[sites$State == 'Indiana', ], in_fish2, join=st_nearest_feature)
# trying to pair up data with sites
ggplot()+geom_sf(data=in_fish2 %>% filter(ï..station_name %in% in_fish2_loc$ï..station_name))+
  geom_sf(data=sites[sites$State == 'Indiana',],alpha=0.5, color='red', size=3)
names(in_fish2)
in_fish2 %>% filter(ï..station_name %in% in_fish2_loc$ï..station_name) %>%
  group_by(ï..station_name) %>%
  summarize(n=n(), n_distinct(activity_end_date),
            dts=paste(activity_end_date, collapse = '; '))
  
# invert data but difficult to get location data & would need to self calculate
in_hm<-read.csv('IBI_data/Hoosier Riverwatch Invert Samples.csv')
in_hm %>% filter(Stream %in% sites[sites$State=='Indiana',]$Stream) %>%
  pull(Stream) %>% unique()
rm(in_hm)
# Mitchell Owens sent me IBI data
in_sitedf<-read_excel('IBI_data/IDEM_Macroinvertebrate Data Request.xlsx', sheet=1) %>%
  bind_rows(read_excel('IBI_data/IDEM_Macroinvertebrate Data Request Extras.xlsx', sheet=1))
in_macro<-read_excel('IBI_data/IDEM_Macroinvertebrate Data Request.xlsx', sheet=2) %>%
  bind_rows(read_excel("IBI_data/IDEM_Macroinvertebrate Data Request Extras.xlsx", sheet=2))
names(in_macro)
ibis<-bind_rows(ibis %>% filter(!is.na(taxa)), 
                in_macro %>% select(Date, Stream, LSite, `12-Digit HUC`, METRIC_SCORE_MIBI) %>%
  left_join(in_sitedf %>% select(LSite, Date, starts_with('LATITUDE'), starts_with('LONGTITUDE'))) %>%
  mutate(Date= substr(Date, 1,10),
         decLat=LATITUDE_DEGREE + (LATITUDE_MINUTE / 60) + (LATITUDE_SECOND/3600),
         decLong=(abs(LONGTITUDE_DEGREE)+(LONGTITUDE_MINUTE/60)+(LONGTITUDE_SECOND/3600))*-1) %>% 
  select(-starts_with("LATITUDE"), -starts_with("LONGTITUDE")) %>%
  left_join(sites %>% select(Stream, SiteCode, HUC12)) %>% 
  filter(!is.na(SiteCode)) %>%
  st_as_sf(coords=c('decLong','decLat'), crs=st_crs(ibis)) %>%
    rename(StreamName = Stream, IBIScore = METRIC_SCORE_MIBI) %>%
    select(-LSite, -`12-Digit HUC`, -HUC12) %>%
    mutate(taxa='macroinvertebrates'))
rm(in_sitedf, in_fish2, in_fish2_loc)
# Mitchell also sent QHEI scores (based on habitat) and some scores based on Kick samples (pre-MIBI or 2004)

# Kentucky ===========
ky.pre13<-read_xlsx('IBI_data/KY_MBI and KIBI through 2013_EDAS.xlsx') %>%
  rename(IBIScore = Biological_Indices, IBICat=Narrative_Rating,
         Date=CollDate) %>%
  select(StationID, StreamName, Basin, Lat_Dec, Long_Dec, 
         Date, Data_Type, IBIScore, IBICat) %>%
  st_as_sf(coords=c('Long_Dec', 'Lat_Dec'), crs=st_crs(sites)) %>%
  st_join(sites %>% select(Stream:SiteCode), join=st_nearest_feature) %>% 
  mutate(SiteCode = case_when(StreamName %in% 
                                c('SOUTH FORK ROCKCASTLE RIVER','ROCKCASTLE RIVER') ~ 'KYROC',
                              T~SiteCode),
         taxa=ifelse(Data_Type == 'Benthic Macroinvertebrate', 'macroinvertebrates',tolower(Data_Type))) %>%
  select(-Data_Type, -Basin, -StationID, -Stream, -State)
excel_sheets('IBI_data/KY_MBI and KIBI 2014 to pres_KWADE.xlsx')
ky.2014<-bind_rows(read_xlsx('IBI_data/KY_MBI and KIBI 2014 to pres_KWADE.xlsx', sheet=1) %>%
            mutate(taxa='macroinvertebrates'),
          read_xlsx('IBI_data/KY_MBI and KIBI 2014 to pres_KWADE.xlsx', sheet=2) %>%
            mutate(taxa='fish')) %>% 
  rename(IBICat=INDEX_RATING, IBIScore = INDEX_SCORE, Date=ACTIVITY_DATE,
         StreamName = LOCALE_NAME, Notes=VISIT_COMMENTS) %>%
  st_as_sf(coords=c('LONGITUDE', 'LATITUDE'), crs=st_crs(sites)) %>%
  st_join(sites %>% select(Stream, SiteCode, HUC12), join=st_nearest_feature) %>% 
  #ggplot()+ geom_sf(aes(color=SiteCode))+geom_sf(data=sites[sites$SiteCode %in% c('KYHLC', 'KYRND'),], color='red')
  mutate(SiteCode = ifelse(HUC_12 == '051301020401', 'KYHLC',SiteCode)) %>% # correcting wrong join with KYRND        
  select(SiteCode, StreamName, Date, IBICat, IBIScore, Notes, taxa)

ibis<-bind_rows(ibis, bind_rows(ky.2014, ky.pre13) %>%
            mutate(Date=substr(Date, 1, 10)))

# Missouri ============
mo.ibi <- read.csv('IBI_data/MO_invert_IBIdata.csv') %>%
  st_as_sf(coords=c('Long','Lat'), crs=st_crs(sites))
ggplot()+geom_sf(data=mo.ibi)+
  geom_sf_text(data=sites[sites$state.abbreviation=='MO',],
               aes(label=SiteCode), color='red')
names(ibis);names(mo.ibi)
ibis <- bind_rows(ibis, 
                  mo.ibi %>% rename(StreamName = SampledRiver) %>% 
                    select(Date, SiteCode, StreamName, IBIScore, Notes) %>%
                    mutate(taxa='macroinvertebrates'))

# Minnesota ==========
mn_ibis<-read_excel('IBI_data/MN_ibi_data.xlsx')

ibis<-bind_rows(ibis,
                mn_ibis %>% select(-ATTAINSau, -StationNumber) %>%
                  mutate(Date=substr(Date,1,10))%>%
                  rename(IBICat = IBICategory) %>%
                  st_as_sf(coords=c('long','lat'), crs=st_crs(ibis)))

# North Carolina ============
list.files('IBI_data/', pattern='NC')
nc.ibi.sites<-NULL # NC sent lots of data sheets, joining them together
for(u in list.files('IBI_data/', pattern='SITE', full.names = T)){
  nc.sites<-read_xlsx(u, skip=1, col_names = F) %>% t()
  colnames(nc.sites)<-nc.sites[1,]
  nc.sites<-as_tibble(nc.sites[-1,])
  nc.ibi.sites<-bind_rows(nc.ibi.sites, nc.sites)
}
nc.ibi.sites %>% distinct(Stream, `Site Location`, `Site ID`, .keep_all=T) %>% pull(`Stream Segment AU`)
nc.au<-sites %>% filter(State == 'North Carolina') %>% select(SiteCode, Stream, ATTAINSau) %>%
  mutate(`Stream Segment AU` = gsub('NC', '', ATTAINSau)) %>% pull(`Stream Segment AU`)
nc.ibi.sites %>% distinct(Stream, `Site Location`, `Site ID`, `Stream Segment AU`, .keep_all=T) %>% 
  filter(`Stream Segment AU` %in% nc.au) %>% View()
nc.ibi.sites <- nc.ibi.sites %>% distinct(Stream, `Site Location`, `Site ID`, .keep_all=T) %>%
  mutate(SiteCode = case_when(Stream == 'CATAWBA R' ~ 'NCCAT', Stream == 'CHEOAH R'~ 'NCCHE',
                              Stream == 'L TENNESSEE R' ~ 'NCLTN', Stream == 'CARTOOGECHAYE CR' ~ 'NCCTG',
                              Stream == 'MIDDLE CR' ~ 'NCMDL', Stream == 'LITTLE R' ~ 'NCLTL',
                              Stream == 'FISHING CR' ~ 'NCFSH', Stream == 'L FISHING CR' ~ 'NCLFS',
                              Stream == 'WILSON CR' ~ 'NCWIL', Stream == 'SWIFT CR' ~ 'NCSWN',
                              Stream == 'UPPER CR' ~ 'NCUPP', Stream == 'TUCKASEGEE R' ~ 'NCTKG')) %>%
  st_as_sf(coords=c('Longitude (º)', 'Latitude (º)'), crs=st_crs(sites))
nc.ibi.s<-NULL
for(i in unique(nc.ibi.sites$SiteCode)){
nc.sss<-nc.ibi.sites %>% filter(SiteCode == i) %>%
  mutate(dist.to.site=st_distance(., sites[sites$SiteCode == i,])[,1]) 
nc.ibi.s<-bind_rows(nc.ibi.s, nc.sss)
}
nc.ibi.s %>% select(Stream, `Site Location`, `Site ID`, SiteCode, dist.to.site)
nc.ibi.closest<-nc.ibi.s %>% group_by(SiteCode) %>% 
  filter(min(as.numeric(dist.to.site))==as.numeric(dist.to.site))
#nc.ibi.closest %>% group_by(SiteCode) %>% count() %>% arrange(desc(n))

nc.met<-read_xlsx("IBI_data/NC_DuBose METRICS_editted.xlsx", col_names=F,
                  col_types='text') %>% t()
colnames(nc.met)<-nc.met[1,]
nc.met<-nc.met[-1,]
nc.met[1:5, 1:10]
nc.metrics<-nc.met %>% as_tibble() %>%
  mutate(dt=as.Date(as.numeric(nc.met[,5]), origin='1899-12-30')) %>%
 # filter(dt > '2000-01-01') %>%
  select(Stream, `Site Location`, County, dt, Bioclassification, `Total taxa richness`, `Total EPT`, `NCBI`, `Corrected NCBI`)
 
nc.metrics %>% 
  left_join(nc.ibi.closest, by = c('Stream', 'Site Location', 'County')) %>%
  filter(!is.na(dist.to.site)) %>%
  group_by(SiteCode, `Site Location`) %>% mutate(total.ibis=n()) %>%
  group_by(SiteCode, `Site Location`, Bioclassification) %>% 
  summarize(n=n(), last.dt=max(dt), total.ibis=mean(total.ibis), .groups='drop') %>%
  arrange(SiteCode, desc(last.dt)) %>% 
  group_by(SiteCode) %>% slice(1)

nc.metrics %>% 
  filter(Stream == 'WILSON CR') %>% arrange(desc(dt))

ibis<-bind_rows(ibis %>% filter(!is.na(SiteCode)), 
                nc.metrics %>% 
                  left_join(nc.ibi.closest %>%
                              select(c('Stream','Site Location', 'County','dist.to.site', 'SiteCode')), 
                            by = c('Stream', 'Site Location', 'County')) %>%
                  filter(!is.na(dist.to.site)) %>%
  mutate(Date=as.character(dt), taxa='macroinvertebrates',
         StreamName=paste(Stream, `Site Location`)) %>%
  rename(IBICat=Bioclassification) %>%
  left_join(data.frame(IBICat=c('Excellent','Good','Good-Fair','Fair','Poor'),
                       IBIScore=5:1)) %>%
  select(-`Total taxa richness`, -`Total EPT`, -NCBI, -`Corrected NCBI`,
         -Stream, -`Site Location`, -dt, -County, -dist.to.site))

# fish data
nc.fsh<-read_xlsx('IBI_data/NCDWR_Fish_Data_Traci_DuBose.xlsx')
View(nc.fsh)
nc_fish<-nc.fsh %>% filter(StationID != 'StationID' & !is.na(StationID)) %>%
  distinct(StationID,Waterbody, Date, Latitude,Longitude,
             `IBI Score`, Bioclass) %>% 
  mutate(Date=as.Date(as.numeric(Date), origin='1899-12-30')) %>%
  st_as_sf(coords=c('Longitude','Latitude'), crs=st_crs(sites))%>%
  rename(StreamName = Waterbody, IBICat=Bioclass, IBIScore=`IBI Score`, Notes=StationID) %>%
  st_join(sites[sites$State =='North Carolina',] %>% select(SiteCode, Stream), join=st_nearest_feature) %>%
  mutate(SiteCode = ifelse(StreamName == 'Swift Cr', 'NCSWN',SiteCode))
x<-findNLDI(location = sites[sites$SiteCode=='NCSWN',], 
            nav = c("UM", "UT", 'DM'), find = c("flowlines"), distance_km = 70)
ggplot()+
  geom_sf(data=x$UT_flowlines)+geom_sf(data=x$DM_flowlines)+
  geom_sf(data=nc_fish %>% filter(StreamName %in%c('Swift Cr','Little R')), aes(color=IBIScore), size=3) + 
  geom_sf_text(data=sites[sites$SiteCode %in% c('NCMDL','NCSWN',"NCLTL"),], aes(label=SiteCode))+
  theme_bw()
ggplot()+
  geom_sf(data=nc_fish, aes(color=IBIScore), size=3) + 
  geom_sf_text(data=sites[sites$State == 'North Carolina',], aes(label=SiteCode))+
  theme_bw()

ibis<-bind_rows(ibis, nc_fish %>% select(-Stream) %>% 
                  mutate(Date=as.character(Date), IBIScore=as.numeric(IBIScore), taxa='fish'))
names(ibis)

# Ohio ============
list.files(pattern='OH')
oh_m <- read.csv('IBI_data/OH_Aquatic_Life_Use_Monitoring_(2020).csv') %>%
  st_as_sf(coords=c('ï..X','Y'), crs=4269)
#ggplot()+geom_sf(data=oh_m)
oh_m_loc<-st_join(sites[sites$State=='Ohio', ], oh_m, join=st_nearest_feature)
ggplot()+geom_sf(data=oh_m %>% filter(Station_ID %in% oh_m_loc$Station_ID))+
  geom_sf(data=sites[sites$State == 'Ohio',],
          alpha=0.5, color='red', size=3)
st_is_within_distance(sites[sites$State == 'Ohio',], oh_m, 2050)
sites[sites$State == 'Ohio',]$SiteCode
ibis<-bind_rows(ibis,
                oh_m[c(392, 443, 444, 3703, 2250, 2603, 4149, 4150),] %>%
  mutate(SiteCode = c(rep('OHBDB', 3), 'OHKIL','OHLMI','OHLMI','OHWAL','OHWAL')) %>%
  select(Station_Name, SiteCode, ends_with('Year'), ends_with('Score'), 
         'Invert_Narr') %>%
    # rescaling Miwb to be equivalent with other fish IBI score
    mutate(MIWB_Score=MIWB_Score/10*60) %>% #standardize by base value, then multiply by max IBI score
  pivot_longer(ends_with('Score'), values_to = 'IBIScore') %>%
  mutate(taxa=case_when(name %in% c('IBI_Score', 'MIWB_Score') ~ 'fish',
                        name == 'ICI_Score' ~ 'macroinvertebrates',
                        name == 'QHEI_Score' ~ 'habitat'),
         Date = case_when(taxa == 'fish'~ paste0(Fish_Year, '-06-30'),
                          taxa == 'macroinvertebrates' ~ paste0(Bug_Year, '-06-30'),
                          taxa == 'habitat' ~ as.character(mean(c(Fish_Year, Bug_Year), na.rm=T))),
         IBICat = ifelse(taxa == 'macroinvertebrates', Invert_Narr, NA)) %>%
  rename(StreamName=Station_Name) %>%
  select(StreamName, SiteCode, IBIScore, IBICat, taxa, Date) %>%
  filter(!(is.na(IBIScore) & is.na(IBICat))))

oh_fish<-NULL
for(i in 1:4){
  oh_fish<-bind_rows(oh_fish,
                     read_excel('IBI_data/OH_Mussel_IBI_ICI_QHEI_BDC_Killbuck_LMR_Walhonding.xlsx', sheet=i) %>%
                       mutate(MIWBI = as.character(MIWBI)))}
which(!(oh_fish$Station %in% gsub(" ", '', unique(oh_m$Station_ID)))) 
# so all data is in oh_m but more recent scores 
oh_fish %>% filter(Station %in% gsub(' ', '', oh_m_loc$Station_ID)) %>% pull(`Sample Year`)
# need to add this in I think
oh_fish %>% filter(`Station Name` %in% ibis[ibis$SiteCode %in% c('OHBDB','OHKIL','OHLMI','OHWAL'),]$StreamName,
                   `Sample Year` > max(substr(ibis[ibis$SiteCode %in% c('OHBDB','OHKIL','OHLMI','OHWAL'),]$Date,1,4)))

ibis<-bind_rows(ibis, 
                oh_fish %>% filter(`Station Name` == 'KILLBUCK CREEK AT HELMICK @ COVERED BRIDGE (TWP. RD. 25)',
                                   `Sample Year`== 2021) %>%
                  mutate(SiteCode = 'OHKIL', Date=paste(`Sample Year`, `10`,`12`, sep='-'), 
                         taxa='fish', MIWBI=as.numeric(MIWBI)/10*60) %>%
                  pivot_longer(cols=c('IBI', 'MIWBI'), values_to='IBIScore') %>%
                  rename(StreamName=`Station Name`) %>%
                  select(StreamName, SiteCode, Date, IBIScore, taxa) %>%
                  left_join(ibis[ibis$SiteCode=='OHKIL',][1,2], by='StreamName'))
# Pennsylvania ============
pa_m<-read_excel('IBI_data/PA_Macroinvertebrate_Data.xlsx', col_types = 'text') %>%
  st_as_sf(coords=c('Longitude','Latitude'), crs=4269)
#ggplot()+geom_sf(data=pa_m)
pa_inv <- unlist(st_is_within_distance(sites[sites$State == 'Pennsylvania', ], pa_m, 3800))
ggplot()+geom_sf(data=pa_m[pa_inv,])+
  geom_sf(data=sites[sites$State == 'Pennsylvania',],
          alpha=0.5, color='red', size=3)
pa_m[pa_inv,] %>% filter(duplicated(geometry))# two have been duplicated in 2011 and 2017
pa_m_loc<-st_join(sites[sites$State == 'Pennsylvania',] %>%
                    select(Stream, SiteCode, HUC12), pa_m, join=st_nearest_feature) 
pa_m_loc %>% select(Stream, SiteCode, `Stream Name`)
pa_m_loc %>% select(SiteCode, starts_with("HUC")) %>% filter(paste0('0', substr(HUC12.x, 1,9))!=HUC10)
nrow(pa_m_loc)==nrow(sites[sites$State == 'Pennsylvania',]) 
ggplot()+geom_sf(data=pa_m %>% filter(StationID %in% pa_m_loc$StationID))+
  geom_sf(data=sites[sites$State == 'Pennsylvania',],
          alpha=0.5, color='red', size=3)
# some sites are duplicated on one IBI value; 
# also likely need to double check Shenango is appropriate for PABVR, PAMHR and PASHN
names(pa_m_loc)
ibis<-bind_rows(ibis, 
                pa_m_loc %>% 
                  mutate(`Small Stream IBI - Freestone` = ifelse(StationID %in% c("20070827-0913-WQN",
                                                                                  "20110318-0930-dcounahan"),
                                                                 NA, `Small Stream IBI - Freestone`)) %>%
                  pivot_longer(c("Small Stream IBI - Freestone", "Large Stream IBI - Freestone",
                            "Limestone IBI", "Multihabitat IBI"),
                          values_to = 'IBIScore', names_to='Notes') %>%
  mutate(Date=paste(Year, Month, Day, sep='-'),
         taxa='macroinvertebrates',
         IBIScore=as.numeric(IBIScore)) %>%
  select(SiteCode, `Stream Name`, Date, Notes, IBIScore, taxa) %>%
  filter(!is.na(IBIScore)) %>%
    rename(StreamName = `Stream Name`))

# fish data sent by Timothy Wertz DEP --- they use a different index!
pa_f<-read_excel('IBI_data/PA_20240305_DataRequest_Traci_DuBose.xlsx', sheet=3) %>% 
  left_join(sites %>% as_tibble() %>% select(Stream, SiteCode), by='Stream') %>%
  left_join(read_excel('IBI_data/PA_20240305_DataRequest_Traci_DuBose.xlsx', sheet=2) %>%
              select(Stream, ProximalSitesComments), by='Stream') %>%
  mutate(Notes=paste(DEP_ProximalSites,ProximalSitesComments))
head(pa_f)
ibis<-bind_rows(ibis, 
          pa_f %>%
            mutate(Date=as.character(substr(SURVEY_DATE_START,1,10)), 
                   taxa='fish') %>%
            select(Stream, Date, TFI, SiteCode, taxa, Notes,
                          starts_with('DEP_SITE')) %>%
            st_as_sf(coords=c('DEP_SITE_LONGITUDE','DEP_SITE_LATITUDE'), crs=st_crs(sites)) %>%
            rename(StreamName = Stream, IBIScore=TFI))

pa_f %>%
  mutate(Date=as.character(substr(SURVEY_DATE_START,1,10)), 
         taxa='fish') %>%
  select(Stream, Date, TFI, SiteCode, taxa, Notes,
         starts_with('DEP_SITE')) %>%
  st_as_sf(coords=c('DEP_SITE_LATITUDE', 'DEP_SITE_LONGITUDE'), crs=st_crs(sites)) %>%
  rename(StreamName = Stream, IBIScore=TFI) %>%
  ggplot()+geom_sf()+geom_sf(data=sites[sites$State == 'Pennsylvania',], color='red')
            
# Tennessee ============
tn_m<-read.csv("IBI_data/TN_SQSH Data.csv")
tn_m %>% 
  filter(Waterbody.ID %in% (sites %>% filter(State == 'Tennessee') %>% pull(ATTAINSau)) |
           grepl('Emory', Monitoring.Location.Name)) %>%
  mutate(dt=mdy(Activity.Date)) %>%
  group_by(Waterbody.ID) %>% summarize(n=n(), max.date=max(dt), min.date=min(dt))
sites %>% filter(State == 'Tennessee') %>% 
  filter(!(ATTAINSau %in% unique(tn_m$Waterbody.ID))) %>%
  select(Stream, ATTAINSau)

tn_m %>% filter(grepl('Emory', Monitoring.Location.Name) | 
                  grepl('Conasauga', Monitoring.Location.Name) |
                  grepl('Red', Monitoring.Location.Name))
# need to find data for the conasauga (borders georgia) and the red (borders Kentucky)
# maybe look at data from those states to see if there are some close by
# then look to close tributaries; no data for attains id or at county level
tn_m_loc <- read.csv('IBI_data/TN_station_loc.csv') %>%
  filter(DWR.Station.ID %in% tn_m$DWR.Station.ID) %>%
  st_as_sf(coords=c('Longitude','Latitude'), crs=st_crs(sites))

ibis<-bind_rows(ibis,
                tn_m %>% 
  filter(Waterbody.ID %in% (sites %>% filter(State == 'Tennessee') %>% pull(ATTAINSau)) |
           grepl('Emory', Monitoring.Location.Name)) %>%
  left_join(sites %>% as_tibble() %>% select(SiteCode, ATTAINSau), by=c('Waterbody.ID'='ATTAINSau')) %>%
  mutate(Date=as.character(as.Date(mdy(Activity.Date), 'ymd')),
         taxa='macroinvertebrates',
         SiteCode = ifelse(Monitoring.Location.Name == 'Emory River', 'TNEMR', SiteCode)) %>%
    left_join(tn_m_loc %>% select(DWR.Station.ID, Monitoring.Location.Name)) %>%
  rename(Notes = Monitoring.Location, StreamName=Monitoring.Location.Name, 
         IBIScore= TMI) %>%
  select(StreamName, SiteCode, Date, IBIScore, taxa, Notes, geometry) )

# update 4/9/2024: specifically looked for ATTAINS au for TNCON and TNRED
tn.con.red<-read.csv('IBI_data/TN_SQSH Data_con_red.csv') %>% 
  left_join(read.csv('IBI_data/TN_SQSH Data_con_red_locations.csv'), 
            by=c('Monitoring.Location.Name', 'DWR.Station.ID', 'County', 'Hydrocode', 'Eco.IV', 'Drainage.Area')) %>%
  st_as_sf(coords=c('Longitude', 'Latitude'), crs=st_crs(sites))

tn.con.red %>% filter(HUC.8.Name=='Conasauga') %>% 
  ggplot()+geom_sf()+geom_sf_text(data=sites[sites$SiteCode=='TNCON',], aes(label=SiteCode))
con.sites<-which(as.numeric(st_distance(tn.con.red, sites[sites$SiteCode=='TNCON',])) < 500)
tn.con.red[con.sites,]
tn.con.red %>% filter(HUC.8.Name=='Red') %>% 
  ggplot()+geom_sf()+geom_sf_text(data=sites[sites$SiteCode=='TNRED',], aes(label=SiteCode))
red.sites<-which(as.numeric(st_distance(tn.con.red, sites[sites$SiteCode=='TNRED',])) < 5000)
ibis<-bind_rows(ibis, 
                tn.con.red[c(con.sites,red.sites),] %>% filter(!is.na(TMI)) %>%
                  mutate(SiteCode = ifelse(HUC.8.Name == 'Red', 'TNRED', 'TNCON'), 
                         taxa='macroinvertebrates') %>%
                  rename(Date = Activity.Date, IBIScore=TMI, Notes = Monitoring.Location, StreamName=Monitoring.Location.Name) %>%
                  select(Date, StreamName, IBIScore, Notes, SiteCode, taxa))
#View(ibis)
rm(tn.con.red, red.sites, con.sites, tn_m, tn_m_loc)

# Virginia ============
# bring in the station location data
va_s<-read_excel('IBI_data/VA_6B_6C_StationInfo_VSCI_02162024.xlsx', sheet=1) %>%
  st_as_sf(coords=c('Longitude','Latitude'), crs=st_crs(sites))
ggplot()+geom_sf(data=va_s)+geom_sf(data=sites[sites$State=='Virginia',], color='purple')
# not a simple join, so filted to HUC8 and then did a nearest neighbor join
va_j<-NULL # empty dataframe
for(huc in unique(sites[sites$State=='Virginia', ]$HUC8)){
  va_j<-bind_rows(va_j, 
                  va_s %>% filter(Sta_Huc_Code == huc) %>% # filter to huc
    st_join(sites[sites$State=='Virginia' & sites$HUC8 == huc,], # figure out closest mussel site
            join=st_nearest_feature) )
}
# fixing one bad bind
va_j <- va_j %>% mutate(SiteCode = case_when(Sta_Stream_Name == 'Copper Creek' & !is.na(WQS_WATER_NAME) ~'VACOP',
                                     WQS_WATER_NAME == 'Clinch River' & SiteCode == 'VACOP' ~ 'VACR2',
                                     BASINS_VAHU6 == 'TC28' & WQS_WATER_NAME == 'Copper Creek' ~ 'VACOP',
                                     SiteCode == 'VAIND'~'VACR1', #email said no stations for indian creek
                                     SiteCode == 'VABMC' & Sta_Stream_Name == 'North Fork Holston River' ~ 'VANFH', # making names align
                                     T~SiteCode))
va_j %>% as_tibble() %>% count(Sta_Stream_Name, SiteCode) 
va_j %>% filter(SiteCode == 'VANFH'|SiteCode =='VABMC') %>% 
  ggplot()+geom_sf(aes(color=SiteCode))+
  geom_sf(data=sites[sites$SiteCode %in% c('VANFH','VABMC'),])
# map to double check my work
ggplot(data=va_j)+geom_sf(aes(color=SiteCode))+
  geom_sf_text(data=sites[sites$State=='Virginia',], aes(label=SiteCode))
# bring in ibi scores
va_m <-read_excel('IBI_data/VA_6B_6C_StationInfo_VSCI_02162024.xlsx', sheet=2)
va_m$StationID[which(!(va_m$StationID %in% va_j$Sta_Id))]
ibis<-bind_rows(ibis, 
                va_m %>%
  mutate(IBICat=ifelse(`SCI Score`>= `SCI Threshold`, 'good', 'poor')) %>% # threshold used as 25% of reference
  select(StationID, `Collection Date`, `SCI Score`, IBICat) %>% 
  left_join(va_j %>% 
              mutate(StreamName=paste(ifelse(is.na(Sta_Stream_Name), WQS_WATER_NAME, Sta_Stream_Name))) %>%
              select(SiteCode, StreamName,  Sta_Id, Sta_Desc) %>%
              mutate(Notes=paste(Sta_Id, ';', Sta_Desc)), 
            by=c('StationID' = 'Sta_Id')) %>%
  mutate(taxa='macroinvertebrates') %>%
  rename(Date = `Collection Date`, IBIScore = `SCI Score`) %>% 
  select(-StationID, -Sta_Desc))
names(ibis)
rm(va_s, huc)

# West Virginia ============
# consolidated WVDEPBENTHIC sheets into one sheet based on the county and stream name for each site. 
# WVDEPBENTHIC has IBI data for tributaries for the main river
# do not have locations for these sites though, likely would need to do it by hand.
wv_m <- read_excel('IBI_data/WV_macro_data.xlsx')
wv_m %>% count(`Stream Name`, County, `Mile Point`)
wv_loc <-read.csv('IBI_data/WV_loc.csv') %>% filter(!is.na(Mile.Point)) %>%
  st_as_sf(coords=c('long', 'lat'), crs=st_crs(sites)) %>%
  rename(`Mile Point`=Mile.Point, `Stream Name`=River)

ibis<-bind_rows(ibis %>% filter(!(SiteCode %in% c('WVELK','WVWFK','WVLKW','WVHAC'))),
                wv_m %>%
                  select(`Stream Name`, `Mile Point`,`Sample Date`, IBI, `HBI Score`, WVSCI) %>%
                  left_join(wv_loc, by=c('Stream Name', 'Mile Point')) %>%
  rename(StreamName = `Stream Name`) %>%
  pivot_longer(c('HBI Score','WVSCI'), names_to='type', values_to='IBIScore') %>%
  mutate(Date=substr(`Sample Date`, 1,10), 
         Notes=paste('mile', `Mile Point`, ';', IBI),
         taxa=ifelse(type=='WVSCI', 'macroinvertebrates', 'habitat'),
         SiteCode = case_when(StreamName == 'Elk River' ~ 'WVELK',
                              StreamName == 'Hackers Creek' ~ 'WVHAC',
                              StreamName == 'Little Kanawha River' ~ 'WVLKW',
                              StreamName == 'West Fork River' ~ 'WVWFK')) %>%
  select(-type, -`Sample Date`, -`Mile Point`, -IBI))

# i think these are more macroinvertebrate data
wve_loc <- read_excel('IBI_data/WV_fish_data.xlsx', sheet=4) %>% # elk locations
  st_as_sf(coords=c('POR_LON_DD', 'POR_LAT_DD'), crs=4269)
st_nearest_feature(sites[sites$SiteCode == 'WVELK',], wve_loc)
wve_loc[10,] %>% select(STREAM_NAME, MILE_POINT, COUNTY)
wvlk_loc <- read_excel('IBI_data/WV_fish_data.xlsx', sheet=8) %>%  # little Kanawha locations
  st_as_sf(coords=c('POR_LON_DD', 'POR_LAT_DD'), crs=4269)
st_nearest_feature(sites[sites$SiteCode == 'WVLKW',], wvlk_loc)
wvlk_loc[1,] %>% select(STREAM_NAME, MILE_POINT, COUNTY)
wvwf_loc <- read_excel('IBI_data/WV_fish_data.xlsx', sheet=11) %>% # west fork locations
  st_as_sf(coords=c('POR_LON_DD', 'POR_LAT_DD'), crs=4269)
st_nearest_feature(sites[sites$SiteCode %in% c('WVWFK','WVHAC'),], wvwf_loc)
wvwf_loc[c(90,86),] %>% select(STREAM_NAME, MILE_POINT, COUNTY)
which(grepl('Hacker', unique(wvwf_loc$STREAM_NAME)))
unique(wvwf_loc$STREAM_NAME)[which(grepl('West Fork', unique(wvwf_loc$STREAM_NAME)))]
ggplot()+
  geom_sf(data=wvwf_loc[wvwf_loc$STREAM_NAME == 'West Fork River', ], color='green')+
  geom_sf(data=wvwf_loc[c(90,86),])+
  geom_sf(data=sites[sites$SiteCode %in% c('WVWFK','WVHAC'),], color='red', alpha=0.5, size=2)
which(wvwf_loc$STREAM_NAME == 'West Fork River')
names(wvwf_loc)
wvwf_loc[c(1:10, 90, 86),] %>% 
  select(SURVEY_TYPE, STREAM_NAME, SAMPLE_DATE, WVSCI,PCT_OF_THRESHOLD_GLIMPSS_CF, starts_with('NARRA'))

# west virginia fish data
wve_loc <- read_excel('IBI_data/WV_fish_data.xlsx', sheet=1) %>% # elk locations
  st_as_sf(coords=c('POR_LON_DD', 'POR_LAT_DD'), crs=4269)
ggplot(wve_loc)+geom_sf()+geom_sf(data=sites[sites$SiteCode=='WVELK',], color='red', alpha=.3)
st_nearest_feature(sites[sites$SiteCode == 'WVELK',], wve_loc)
wve_loc[1,] %>% count(STREAM_NAME, MILE_POINT)
st_distance(sites[sites$SiteCode == 'WVELK', ], wve_loc[1,])
ggplot()+
  geom_sf(data=wve_loc, aes(color=STREAM_NAME))+
  geom_sf(data=sites[sites$SiteCode %in% c('WVELK'),], color='red', alpha=0.5, size=2)
wvlk_loc <- read_excel('IBI_data/WV_fish_data.xlsx', sheet=5) %>%  # little Kanawha locations
  st_as_sf(coords=c('POR_LON_DD', 'POR_LAT_DD'), crs=4269)
st_nearest_feature(sites[sites$SiteCode %in% c('WVWFK','WVHAC','WVLKW'),], wvlk_loc)
ggplot()+
  geom_sf(data=wvlk_loc[c(266,572),], aes(color=STREAM_NAME))+
  geom_sf(data=sites[sites$SiteCode %in% c('WVLKW', 'WVHAC','WVWFK'),], color='red', alpha=0.5, size=2)
st_distance(sites[sites$SiteCode %in% c('WVLKW'), ], wvlk_loc[c(266),])
# Bear Run = WVLKW
ggplot()+geom_sf(data=wvlk_loc %>% distinct(STREAM_NAME, geometry))+geom_sf(data=sites[sites$SiteCode=='WVLKW',], color='purple')

# can't calculate WV IBI without drainage area regressions &
# WVDEP won't provide because they don't use this IBI and it isn't discriminatory
# combing this with distance between sites, not worth persuing.

# write out summary csv -------
View(ibis)
names(ibis)
unique(ibis$taxa)

length(unique(ibis$SiteCode))
ibis %>% as_tibble() %>% count(SiteCode, taxa) %>% 
  filter(taxa != 'habitat') %>% 
  pivot_wider(names_from=taxa, values_from=n) %>%
  View()

# need to double check and make sure the filtering above is equivalent among states
ibis_d<-NULL
for(sitec in unique(ibis$SiteCode)){
  ibis_d<-bind_rows(ibis_d,
                    ibis %>%
                      filter(SiteCode == sitec) %>%
                      mutate(dist.to.site.m=st_distance(., sites[sites$SiteCode == sitec,])[,1]))
}

ggplot()+geom_histogram(data=ibis_d, aes(x=as.numeric(dist.to.site.m)))
ibis_d %>%
  group_by(SiteCode) %>% filter(min(dist.to.site.m)==dist.to.site.m) %>%
  ungroup() %>%
  summarize(min(dist.to.site.m), max(dist.to.site.m), mean(dist.to.site.m))
ibis_d %>% filter(as.numeric(dist.to.site.m) > 100000) %>% pull(SiteCode) %>% unique()
ibis_d %>% filter(SiteCode %in% c("MOMER", "OHLMI")) %>% View()
  # so those sites do have another IBI value available that is closer

head(ibis_d)
st_write(ibis_d, 'results/ibis_near_sites_250109.csv', layer_options = "GEOMETRY=AS_XY", append=F)
