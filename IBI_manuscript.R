library(tidyverse);library(lubridate);library(sf); library(cowplot); library(readxl)
theme_wendell<- theme_classic()+ # ggplot theme to make plots for wendell
   theme(panel.background=element_rect(fill=NA, color='black'),
        strip.background=element_blank())
# mahi scores from Wendell
mahi.scores <- read_excel('C:/Users/tdubose/OneDrive - DOI/tpd/pre-appointment/IBI manuscript/All sites_2001_2002_2003.xlsx') %>% # fixing some typos below
  rename(SiteCode = `Site code`) %>%
  filter(!is.na(SiteCode), !is.na(Stream)) %>%
  mutate(SiteCode = case_match(SiteCode, 'ILPIC' ~ 'ILPIS','MNLES'~'MNLSR',
                               'MNSUN'~'MNSNR', 'PATML'~'PATMC', 
                               .default = SiteCode)) %>%
  select(-starts_with('...'))
# bring in the site-level data
sites<-read.csv('C:/Users/tdubose/OneDrive - DOI/tpd/pre-appointment/MusselDeclines_site_loc.csv') %>% 
  filter((SiteCode %in% mahi.scores$SiteCode),
         SiteCode != 'KYSLT') %>%
  bind_rows(mahi.scores[,1:6] %>% filter(SiteCode %in% c('MNCAN','MNMIN','MNPOM','MNSNK','MNSTR'))%>%
              mutate(across(c(Latitude, Longitude), as.numeric))) %>%
  st_as_sf(coords=c('Longitude','Latitude'), crs = 4269) 
sites %>% as_tibble() %>% mutate(HUC2=ifelse(nchar(HUC8) == 7, substr(HUC8, 1,1), substr(HUC8, 1,2))) %>% count(HUC2)
sites %>% filter(substr(HUC8, 1,1)=='8')

mahi.scores$SiteCode[(mahi.scores$SiteCode %in% sites$SiteCode == F)] # Mahoning creek in exclude group in Wendells data

ibis_d <- read.csv('C:/Users/tdubose/OneDrive - DOI/tpd/pre-appointment/IBI manuscript/ibis_near_sites_250109.csv') %>%
  st_as_sf(coords=c('X','Y'), crs=st_crs(sites))

# bring in the standardization information for each state and taxa
ibi.stand<-data.frame(taxa=c(rep('macroinvertebrates', 13), rep('fish',10)),
                      State=c('Alabama', 'Georgia','Indiana','Illinois','Kentucky',
                              'Minnesota','Missouri','North Carolina','Ohio','Pennsylvania',
                              'Tennessee','Virginia','West Virginia',
                              'Alabama','Georgia','Indiana','Illinois','Kentucky',
                              'Minnesota', 'North Carolina','Ohio','West Virginia', 'Pennsylvania'),
                      max.score=c(100,100,60,100,100,
                                  100,20,5,60,100,
                                  42,100,100,
                                  100, 60, 60, 100,100,
                                  100, 60, 60, 100, 10)) %>%
  left_join(data.frame(State=state.name, State.abb=state.abb), by='State')
ibi.stand

ibis_d$SiteCode[which(!(ibis_d$SiteCode %in% mahi.scores$SiteCode))] # Mahoning creek, doesn't need to be included
ibis_d <- ibis_d %>% filter(SiteCode != 'PAMAH')

# mahi score results ====
mahi.scores %>% ggplot()+geom_density(aes(x=`MAHI composite`))
mahi.scores %>% filter(!is.na(SiteCode), !is.na(`MAHI composite`)) %>% nrow()
mahi.scores %>% filter(!is.na(SiteCode) & !is.na(`MAHI composite`)) %>%
  summarize(across(`MAHI composite`, list(median=median, IQR=IQR)))
mahi.scores %>% filter(!is.na(SiteCode) & !is.na(`MAHI composite`)) %>%
  mutate(grp=ifelse(`MAHI composite` > 5, 'above','below')) %>%
  group_by(grp) %>% summarize(median(`MAHI composite`))
mahi.scores %>% filter(!is.na(`MAHI composite`)) %>%
  rowwise() %>%
  mutate(cv=sd(c(`MAHI p_richness`, `MAHI density`, `MAHI recruit`, `MAHI pop growth`), na.rm=T)/mean(c(`MAHI p_richness`, `MAHI density`, `MAHI recruit`, `MAHI pop growth`), na.rm=T))  %>% 
  filter(`MAHI composite` != 0) %>% # can't divide by 0, which is mean metric for PADNK
  select(SiteCode, cv) %>% ungroup() %>% 
  summarize(median(cv), min(cv), max(cv))
mahi.scores %>% filter(!is.na(SiteCode) & !is.na(`MAHI composite`)) %>%
  summarize(across(c(`MAHI p_richness`, `MAHI density`, `MAHI recruit`), list(median=median, IQR=IQR))) %>% View()
mahi.scores %>% filter(!is.na(SiteCode), !is.na(`MAHI composite`)) %>% 
  pivot_longer(starts_with('MAHI')) %>% filter(!is.na(value)) %>%
  group_by(name) %>% count()
mahi.scores %>% filter(!is.na(SiteCode) & !is.na(`MAHI composite`),
                       !is.na(`MAHI pop growth`)) %>%
  summarize(median(`MAHI pop growth`), IQR(`MAHI pop growth`), n())

ibis.df <- ibis_d %>% 
  # making sure the filters are equal across states
  filter(as.numeric(dist.to.site.m)<50000,
         # sites not included in mahi.scores
         !(SiteCode %in% c('NCCHE','NCCTG','PABVR','PALMA','PAMAH')),
         taxa %in% c('macroinvertebrates','fish'),
         !is.na(IBIScore)) %>%
  group_by(SiteCode) %>%
  # converting date into a date for better plotting -- gives warning can ignore
  mutate(dt=case_when(substr(SiteCode,1,2) %in% c('AL','MO') ~ mdy(Date), 
                      SiteCode %in% c("TNCON","TNRED") ~ mdy(Date),
                      SiteCode %in% c("ILMFV","ILSFV") & taxa =='macroinvertebrates' ~ mdy(Date),
                      substr(SiteCode,1,2) == 'VA' ~ ymd(substr(Date, 1,10)),
                      T~ymd(Date)),
         yr=year(dt),
         SiteCode = ifelse(SiteCode == 'KYGRE', 'KYGRN', SiteCode)) %>%
  # adding mahi scores
  left_join(mahi.scores, by='SiteCode') %>%
  left_join(ibi.stand, by=c('State'='State.abb','taxa'))  %>% # adding max ibi score info
  mutate(stand.ibi=IBIScore/max.score*10) %>% # calculate relative score
  distinct(SiteCode, dt, taxa, stand.ibi, dist.to.site.m, .keep_all = T) %>% # only keep one row for each site, date, taxa, and score
  filter(!(SiteCode == 'KYBFK' & taxa =='fish' & dist.to.site.m > 16351))  # removing a duplicate!
  
nrow(ibis.df) # how many scores did I get
min(ibis.df$dt); max(ibis.df$dt) # time range captured
min(ibis.df$dist.to.site.m); max(ibis.df$dist.to.site.m) # distance from site captured
ibis.df %>% filter(!is.na(`MAHI composite`)) %>% distinct(SiteCode, taxa) %>% ungroup() %>% count(taxa)
ibis.df %>% filter(!is.na(`MAHI composite`)) %>% distinct(SiteCode, taxa) %>% ungroup() %>% count(SiteCode)
ibis.df %>% filter(!is.na(`MAHI composite`)) %>% distinct(SiteCode, taxa) %>% ungroup() %>% count(SiteCode) %>% View()

# investigate correlation between fish and macroinvertebrate IBIs using all data ====
# distances among fish and macro samples at each site
#max.distance<-NULL
#for(siteC in unique(ibis.df$SiteCode)){
#  s.ibi.dist.mat<-ibis.df %>% filter(SiteCode == siteC) %>% # filter to a single site
#    distinct(geometry, .keep_all = T) %>% st_distance() %>% round(., -2) # only keep distinct geometries
#  max.distance<-bind_rows(max.distance, 
#                          data.frame(SiteCode=siteC, 
#                                     max.dist=max(s.ibi.dist.mat), nmax=sum(s.ibi.dist.mat == max(s.ibi.dist.mat)), 
#                                     min.dist=min(s.ibi.dist.mat), nmin=sum(s.ibi.dist.mat == min(s.ibi.dist.mat))))
#}

#ggplot()+geom_histogram(aes(x=as.numeric(max.distance$max.dist))) # so 3 pairs are > 50km away
#max.distance %>% filter(as.numeric(max.dist) > 50000)
# investigate the far away sites
# first dstant site: VACR2 site
ibis.df %>% filter(SiteCode == 'VACR2') %>% 
  distinct(geometry, .keep_all = T) %>% st_distance() %>% round(., -2) # 1 and 9 too distant
ibis.df %>% filter(SiteCode == 'VACR2') %>% distinct(geometry, .keep_all = T) %>%
  slice(1,9) %>% select(dt, StreamName, IBIScore, Notes, dist.to.site.m, taxa) 
# keep dist.to.site.m = 1818563.3 or Notes = "6BCLN199.15 ; Horton Ford, TN"
# second distant site: VACR1
ibis.df %>% filter(SiteCode == 'VACR1') %>% 
  distinct(geometry, .keep_all = T) %>% 
  slice(-c(9:12)) %>%  #removing 9 to 12, distance from mussel site > 29km
  st_distance() %>% round(., -2) 
ibis.df %>% filter(SiteCode == 'VACR1') %>% distinct(geometry, .keep_all = T) %>%
  slice(1,9) %>% select(dt, StreamName, IBIScore, Notes, dist.to.site.m, taxa) 
# third distant site: VAMFH
ibis.df %>% filter(SiteCode == 'VAMFH') %>% 
  distinct(geometry, .keep_all = T) %>% 
  #slice(10,11)
  st_distance() %>% round(., -2) # 1 and 9 too distant
ibis.df %>% filter(SiteCode == 'VACR2') %>% distinct(geometry, .keep_all = T) %>%
  slice(1,9) %>% select(dt, StreamName, IBIScore, Notes, dist.to.site.m, taxa) 

# rank fish and macro IBI samples for each site -----
# investigate using ranks to prioritize samples
  # rank distance
ibi.rankD<-ibis.df %>% as_tibble() %>% 
  mutate(dist.rnd.m=round(dist.to.site.m, -1)) %>%
  distinct(SiteCode, taxa, dist.rnd.m) %>%
  group_by(SiteCode, taxa) %>% mutate(rnkD=rank(dist.rnd.m), ndist=n())
  # rank year
ibi.rankY<-ibis.df %>% as_tibble() %>% distinct(SiteCode, taxa, yr) %>%
  group_by(SiteCode, taxa) %>% mutate(rnkY=(max(rank(yr))+1)-rank(yr), nyear=n())
  # dataframe of ranks
ibi.ranks<-ibis.df %>% select(SiteCode, taxa, stand.ibi, dt, dist.to.site.m, yr, starts_with('MAHI')) %>%
  mutate(dist.rnd.m=round(dist.to.site.m, -1)) %>%
  left_join(ibi.rankD, by=c('SiteCode', 'taxa', 'dist.rnd.m')) %>% 
  left_join(ibi.rankY, by=c('SiteCode', 'taxa', 'yr')) %>%
  rowwise() %>% mutate(avgRnk=mean(c(rnkD, rnkY)), rnkDif=abs(rnkD-rnkY)) %>%
  arrange(SiteCode, taxa, avgRnk)
  # only keep samples that have the lowest average rank
ibi.m<-ibi.ranks %>% as_tibble() %>% 
  group_by(SiteCode, taxa) %>% filter(min(avgRnk)==avgRnk) %>%
  ungroup() %>% filter(!is.na(`MAHI composite`), !is.na(stand.ibi)) %>%# NA for fish NCCAT is in the original data set
  group_by(SiteCode, taxa) 
ibi.mm<-ibi.m # save the non averaged data
# save out the scores that have the lowest average rank
ibi.mm %>% group_by(SiteCode, taxa) %>% 
  mutate(mean.standardized.ibi=mean(stand.ibi), 
         sd.standardized.ibi=sd(stand.ibi)) %>%
  distinct(SiteCode, taxa, mean.standardized.ibi, .keep_all=T) %>%
  as_tibble() %>% select(SiteCode, starts_with('MAHI'), taxa, mean.standardized.ibi, sd.standardized.ibi) %>%
  pivot_wider(names_from=taxa, values_from = c(mean.standardized.ibi, sd.standardized.ibi)) %>% 
  write.csv('C:/Users/tdubose/OneDrive - DOI/tpd/pre-appointment/IBI manuscript/filtered_ibis_MAHI_250411.csv', row.names = F)
View(ibi.m)

ibi.mm %>% ungroup() %>% summarize(n_distinct(SiteCode))
ibi.m %>% 
  left_join(sites %>% select(SiteCode, YearVisited), by='SiteCode') %>%
  group_by(taxa) %>%
  mutate(YearVisited = ifelse(is.na(YearVisited), 2018, YearVisited)) %>%
  summarize(med.dis=median(dist.to.site.m)/1000, min.d=min(dist.to.site.m)/1000, max.d=max(dist.to.site.m)/1000,
            med.year=median(abs(YearVisited-yr)), min.y=min(YearVisited-yr), max.y=max(YearVisited-yr)) %>% View()
ibi.mm %>%  group_by(SiteCode, taxa) %>% count() %>% filter(n>1) %>%
  arrange(taxa) # need to investigate these
# !!! GAARM, KYBCK, KYDRK, KYLTT, MNCAN, MNSTC have a two top choices (most recent is 2nd distant or vis versa)
# duplicate INBBL b/c Sept and Aug sample same year, both have equal IBI
# duplicate INKLM is true duplicate (two samples, one date)
# duplicate MOMER is spring fall sample in Fox creek
# duplicate TNEFS has a spring sample, a true duplicate, and a fall sample with a hess sampler
# duplicate VACOP, VAMFH is a spring fall sample
ibi.mm %>% filter(SiteCode %in% c('KYBCK','GAARM',"MNCAN", "MNSTC", "INBBL"), 
                 taxa=='fish') %>% View()
ibi.mm %>% filter(SiteCode %in% c('GAARM', 'KYDRK','KYLTT','TNEFS','MOMER','INKLM','VAMFH','VACOP'), 
                  taxa=='macroinvertebrates') %>% View()
ibi.mm %>%  group_by(SiteCode, taxa) %>% count() %>% filter(n>1) %>% pull(n) %>% sum()
View(ibi.mm)
# visually check that filtering is occurring as expected
View(ibi.mm %>% mutate(kept='a') %>% as_tibble() %>% right_join(ibis.df %>% as_tibble()) %>%
       select(SiteCode, taxa, dt, dist.to.site.m, kept, everything()) %>%
       arrange(SiteCode, taxa, kept))

# now that the chosen ibis are all double checked, I'll take their mean
ibi.m <- ibi.m %>% group_by(SiteCode, taxa) %>%
  select(SiteCode, taxa, stand.ibi, starts_with('MAHI'), avgRnk) %>%
  summarize(across(where(is.numeric), mean), .groups='drop')
ibi.m[ibi.m$SiteCode == 'GAARM', ]  # mean at site level
ibi.mm[ibi.mm$SiteCode == 'GAARM', ] # raw data

# RESULTS section info about final dataset ======
unique(ibi.m$SiteCode) %>% length()
ibi.m %>% distinct(SiteCode, taxa) %>% count(taxa)

nrow(ibi.m)
ibi.m %>% count(SiteCode, taxa) %>% filter(n!=1) %>% summarize(sum(n))
ibi.m %>% as_tibble() %>% ungroup() %>% mutate(State=substr(SiteCode, 1,2)) %>% count(State)

ibi.m %>% as_tibble() %>% count(taxa)
ibi.m %>% as_tibble() %>% distinct(SiteCode, taxa) %>% count(taxa)
ibi.m %>% as_tibble() %>% group_by(taxa) %>% 
  summarize(mndt=min(yr), mxdt=max(yr), 
            min.dist=min(dist.to.site.m), mx.dist=max(dist.to.site.m), 
            mn.dist=mean(dist.to.site.m), sd.dist=sd(dist.to.site.m))
ibi.m %>% as_tibble() %>% group_by(taxa) %>% count(yr) %>% arrange(desc(n)) %>% slice(1:3)

ibi.m %>% as_tibble() %>% group_by(taxa) %>% summarize(median(stand.ibi), IQR(stand.ibi))
ibi.m %>% as_tibble() %>% group_by(taxa) %>% summarize(min(stand.ibi))

# Correlate fish and macroinvertebrate scores near our mussel sites-------
# double checking distances among highest ranked fish and macro samples at each site
max.distance.rnk<-NULL
for(siteC in unique(ibis.df$SiteCode)){
  s.ibi.dist.mat<-ibi.ranks %>% as_tibble() %>% 
    group_by(SiteCode, taxa) %>% filter(min(avgRnk)==avgRnk) %>% # get the score with the minimum rank for each site
    ungroup() %>% filter(SiteCode == siteC) %>%  # only select one site
    distinct(geometry, .keep_all = T) %>%  # each row will have a distince geometry
    st_as_sf(crs=st_crs(ibis.df)) %>% st_distance() %>% round(., -2)
  max.distance.rnk<-bind_rows(max.distance.rnk, 
                              data.frame(SiteCode=siteC, nsites=nrow(s.ibi.dist.mat),
                                         max.dist=max(s.ibi.dist.mat), nmax=sum(s.ibi.dist.mat == max(s.ibi.dist.mat)), 
                                         min.dist=min(s.ibi.dist.mat), nmin=sum(s.ibi.dist.mat == min(s.ibi.dist.mat))))
}
max.distance.rnk %>% filter(max(max.dist)==max.dist) # max one is 36600 apart
max(max.distance.rnk$nsites)
max.distance.rnk %>% filter(nsites>2) # should inspect spatial layout of GAARM and KYBCK
# # inspecting spatial layout of GAARM and KYBCK
# library(dataRetrieval)
# gaarm<-findNLDI(location = sites[sites$SiteCode=='GAARM',], nav = c("UM", "UT", 'DM'), find = c("flowlines"), distance_km = 12)
# ibi.ranks %>% as_tibble() %>% 
#   group_by(SiteCode, taxa) %>% filter(min(avgRnk)==avgRnk) %>% ungroup() %>%
#   filter(SiteCode %in% c('GAARM')) %>% 
#   ggplot()+
#   geom_sf(data=gaarm$UT, color='lightgrey')+geom_sf(data=gaarm$UM, color='grey')+geom_sf(data=gaarm$DM)+
#   geom_sf(aes(color=taxa, geometry=geometry), alpha=0.5)+geom_sf(data=sites[sites$SiteCode=='GAARM',])+theme_bw()
# ibi.ranks %>% as_tibble() %>% 
#   group_by(SiteCode, taxa) %>% filter(min(avgRnk)==avgRnk) %>% # get the score with the minimum rank for each site
#   ungroup() %>% filter(SiteCode == 'GAARM', taxa=='fish') %>%  # only select one site
#   distinct(geometry, .keep_all = T) %>%  # each row will have a distince geometry
#   st_as_sf(crs=st_crs(ibis.df)) %>% st_distance() %>% round(., -2)
# kybck<-findNLDI(location = sites[sites$SiteCode=='KYBCK',], nav = c("UM", "UT", 'DM'), find = c("flowlines"), distance_km = 30)
# ibi.ranks %>% as_tibble() %>% 
#   group_by(SiteCode, taxa) %>% filter(min(avgRnk)==avgRnk) %>% ungroup() %>%
#   filter(SiteCode %in% c('KYBCK')) %>% 
#   ggplot()+
#   geom_sf(data=kybck$UT, color='lightgrey')+geom_sf(data=kybck$UM, color='grey')+geom_sf(data=kybck$DM)+
#   geom_sf(aes(color=taxa, geometry=geometry), alpha=0.5)+geom_sf(data=sites[sites$SiteCode=='KYBCK',])+theme_bw()

# taking an average score for each taxa (so max n = 2 based on average ranks)
f.m.comp.df<-ibi.ranks %>% as_tibble() %>% 
  group_by(SiteCode, taxa) %>% filter(min(avgRnk)==avgRnk) %>%
  ungroup() %>%
  mutate(rnd.dist=round(dist.to.site.m,-2)) %>%
  dplyr::select(SiteCode, taxa, stand.ibi) %>%
  group_by(SiteCode, taxa) %>% summarize(mn.score=mean(stand.ibi), sd.score=sd(stand.ibi), n.score=n()) %>%
  pivot_wider(names_from = taxa, values_from=ends_with('score')) %>% ungroup() %>%
  filter(!is.na(mn.score_fish) & !is.na(mn.score_macroinvertebrates),
         SiteCode %in% mahi.scores[!is.na(mahi.scores$`MAHI composite`),]$SiteCode) 

# are the scores approximately normal? 
f.m.comp.df %>% pivot_longer(starts_with('mn.score')) %>% 
  ggplot()+geom_histogram(aes(x=value))+facet_wrap(~name)
shapiro.test(f.m.comp.df$mn.score_fish); qqnorm(f.m.comp.df$mn.score_fish)
shapiro.test(f.m.comp.df$mn.score_macroinvertebrates); qqnorm(f.m.comp.df$mn.score_macroinvertebrates)
# NOT normal, safer to do non parametric
# how many samples do I have?
f.m.comp.df %>% filter(!is.na(mn.score_fish) & !is.na(mn.score_macroinvertebrates)) %>% nrow()
f.m.comp.df %>% left_join(max.distance.rnk, by='SiteCode') %>% filter(!is.na(mn.score_fish) & !is.na(mn.score_macroinvertebrates)) %>%
  ungroup() %>%
  summarize(max.dist.m=max(as.numeric(max.dist, na.rm=T)), mn.dist.m=mean(as.numeric(max.dist)), sd.dist.m=sd(as.numeric(max.dist, na.rm=T)))

cor.test(f.m.comp.df$mn.score_fish, f.m.comp.df$mn.score_macroinvertebrates, method='pearson')
nrow(f.m.comp.df)
f.m.comp.df %>% 
  summarize(across(starts_with('sd'), list(~mean(.x, na.rm=T), ~max(.x, na.rm=T))))
f.m.comp.df %>% filter(sd.score_fish == max(sd.score_fish, na.rm=T))
ibi.m %>% filter(SiteCode == 'GAARM'); max.distance.rnk %>% filter(SiteCode=='GAARM')

# Correlate IBIs and MAHI =======
# check that distance between sites doesn't matter and time doesn't matter
ibi.mw<-ibi.mm %>%
  mutate(mahi.diff=`MAHI composite`-stand.ibi, # difference between mahi and ibi should be affected by space and time
         dist.to.site.log10.km=log10(round(dist.to.site.m+10,0)/1000)) %>% # transforming distance and time to be same magnitude and close to 0
  left_join(sites %>% as_tibble() %>% dplyr::select(SiteCode, YearVisited), by='SiteCode') %>%
  mutate(YearVisited = ifelse(is.na(YearVisited), 2020, YearVisited),
    yr.dif=YearVisited-yr) %>%
  group_by(SiteCode, taxa) %>%
  summarize(across(where(is.numeric), mean), .groups='drop') %>%
  arrange(SiteCode)
# are the covariates approximating normality?
plot_grid(ggplot()+geom_histogram(aes(x=ibi.mw$mahi.diff)),
          ggplot()+geom_histogram(aes(x=ibi.mw$dist.to.site.log10.km)),
          ggplot()+geom_histogram(aes(x=ibi.mw$yr.dif)), nrow=1)
covar.mod.f1<-lm(mahi.diff ~ dist.to.site.log10.km*yr.dif, data=ibi.mw[ibi.mw$taxa=='fish',])
plot(covar.mod.f1)
(covar.mod.f<-summary(covar.mod.f1))
covar.mod.m1<-lm(mahi.diff ~ dist.to.site.log10.km*yr.dif, data=ibi.mw[ibi.mw$taxa=='macroinvertebrates',])
plot(covar.mod.m1)
(covar.mod.m<-summary(covar.mod.m1))

# correlate fish IBI with MAHI
(fish.cor<-cor.test(ibi.m[ibi.m$taxa=='fish',]$`MAHI composite`, 
         ibi.m[ibi.m$taxa=='fish',]$stand.ibi, method='pearson'))
nrow(ibi.m[ibi.m$taxa=='fish',])
ggplot()+geom_text(data=ibi.m %>% filter(taxa=='fish'), aes(x=stand.ibi, y=`MAHI composite`, label=SiteCode))+
  geom_abline()
ibi.m %>%  filter(SiteCode %in% c('KYLSF', 'INMFW', 'GAARM', 'GACON', 'KYBCK', "INSFW"), taxa=='fish')
ibi.mm %>% filter(taxa=='fish') %>% mutate(mahiR=rank(`MAHI composite`), fishR=rank(stand.ibi), rank.dif=fishR-mahiR) %>%
  arrange(desc(rank.dif)) %>% slice(1:5) %>% 
  select(SiteCode, stand.ibi, `MAHI composite`, rank.dif, dt, dist.to.site.m)
ibi.m %>% filter(taxa == 'fish', SiteCode == 'KYHLC')

# correlate macroinvertebrate IBI with MAHI
(mac.cor<-cor.test(ibi.m[ibi.m$taxa=='macroinvertebrates',]$`MAHI composite`, 
         ibi.m[ibi.m$taxa=='macroinvertebrates',]$stand.ibi, method='pearson'))
nrow(ibi.m[ibi.m$taxa=='macroinvertebrates',])
ggplot()+geom_text(data=ibi.m %>% filter(taxa=='macroinvertebrates'), aes(x=stand.ibi, y=`MAHI composite`, label=SiteCode))+
  geom_abline(slope = -1, intercept=10)
ibi.m %>% filter(taxa=='macroinvertebrates') %>% mutate(mahiR=rank(`MAHI composite`), macR=rank(stand.ibi), rank.dif=mahiR-macR) %>%
  arrange(rank.dif) %>% slice(1:5) %>%
  select(SiteCode, stand.ibi, dt, dist.to.site.m, `MAHI composite`, rank.dif)
ibi.m %>% filter(SiteCode %in% c('NCFSH','MNCTW'))

# Figure 2 -- stacked correlations between ibi and mahi
plot_grid(plot_grid(ggplot(data=f.m.comp.df, aes(x=mn.score_macroinvertebrates, y=mn.score_fish))+
            #geom_linerange(aes(xmin=mn.score_macroinvertebrates-(sd.score_macroinvertebrates/sqrt(n.score_macroinvertebrates)),
            #                  xmax=mn.score_macroinvertebrates+(sd.score_macroinvertebrates/sqrt(n.score_macroinvertebrates))))+
            #geom_linerange(aes(ymin=mn.score_fish-(sd.score_fish/sqrt(n.score_fish)),
            #                  ymax=mn.score_fish+(sd.score_fish/sqrt(n.score_fish))))+
            #geom_abline()+ 
              geom_smooth(method='lm', se=F, color='grey')+
              geom_point()+
            annotate('text', x=0, y=.25, hjust=0, label= "N = 45", size=3.5) + 
              annotate('text', x=0, y=1.25, hjust=0,
                       label=expression(italic(r)*" = 0.38, "*italic(p)*" < 0.01"), size=3.5) + 
            scale_x_continuous('MIBI', limits=c(0,10))+
            scale_y_continuous('FIBI', limits=c(0,10))+
            theme_wendell,
          ibi.m %>% filter(taxa=='fish') %>%
  ggplot()+geom_point(aes(x=stand.ibi, y=`MAHI composite`))+
  annotate('text', x=0, y=9.75, hjust=0,
           label=expression(italic(r)*" = 0.18, "*italic(p)*" = 0.19"), size=3.5) + 
    annotate('text', x=0, y=8.75, hjust=0, label= "N = 50", size=3.5) + 
    scale_x_continuous('FIBI', limits = c(0,10))+
    scale_y_continuous('MAHI composite', limits = c(0,10))+
    theme_wendell,
  ibi.m %>% filter(taxa=='macroinvertebrates') %>%
    ggplot()+geom_point(aes(x=stand.ibi, y=`MAHI composite`))+
    annotate('text', x=0, y=9.75, hjust=0,
             label=expression(italic(r)*" = -0.17, "*italic(p)*" = 0.13"), size=3.5) +
    annotate('text', x=0, y=8.75, hjust=0, label= "N = 70", size=3.5) +
    scale_x_continuous('MIBI ', limits = c(0,10))+
    scale_y_continuous('MAHI composite', limits = c(0,10))+
    theme_wendell, nrow=1, labels = 'AUTO'),
  plot_grid( ggplot()+theme_void(),
  ibi.m %>% select(SiteCode, taxa, stand.ibi, starts_with('MAHI')) %>%
    pivot_longer(starts_with('MAHI')) %>% filter(!is.na(value), name != 'MAHI composite') %>%
    mutate(taxa=case_match(taxa,'macroinvertebrates'~ 'MIBI', 'fish'~'FIBI',.default=taxa),
      name = factor(name, 
                 levels=c( 'MAHI p_richness', 'MAHI density', 'MAHI recruit','MAHI pop growth'),
                 labels=c( 'Richness','Density', 'Recruitment', 'Population\ngrowth'))) %>%
    ggplot()+
    geom_smooth(data = . %>% filter(name == 'Density'& taxa == 'FIBI'), 
                aes(x=stand.ibi, y=value), method='lm', se=F, color='grey')+
    geom_point(aes(x=stand.ibi, y=value), pch=1)+
    geom_text(data=ibi.metrics %>% 
                mutate(taxa=case_match(taxa,'macroinvertebrate'~ 'MIBI', 'fish'~'FIBI',.default=taxa),
                       name = factor(name, 
                                     levels=c( 'Hist. richness', 'Density', 'Recruitment','Pop. growth'),
                                     labels=c( 'Richness','Density', 'Recruitment', 'Population\ngrowth')),
                       lbl=paste0("italic(r)*' = '*", round(rho,2)),
                       yy=case_when(name=='Recruitment' ~ 8.75, 
                                    name == 'Density' ~9.75, .default = 3.25)),
              x=0, aes(y=yy, label=lbl), parse=T, hjust=0)+
    geom_text(data=ibi.metrics %>% 
                mutate(taxa=case_match(taxa,'macroinvertebrate'~ 'MIBI', 'fish'~'FIBI',.default=taxa),
                       name = factor(name, 
                                     levels=c( 'Hist. richness', 'Density', 'Recruitment','Pop. growth'),
                                     labels=c( 'Richness','Density', 'Recruitment', 'Population\ngrowth')),
                       lbl=paste0("italic(p)*'", ifelse(p < 0.01, '< 0.01',
                                                           paste0(" = ", sprintf("%#.2f", p))), "'"),
                       yy=case_when(name=='Recruitment' ~ 7.25, 
                                    name == 'Density' ~ 8.25, .default = 1.75)),
              x=0, aes(label=lbl, y=yy), parse=T, hjust=0)+
    geom_text(data=ibi.metrics %>% 
                mutate(taxa=case_match(taxa,'macroinvertebrate'~ 'MIBI', 'fish'~'FIBI',.default=taxa),
                       name = factor(name, 
                                     levels=c( 'Hist. richness', 'Density', 'Recruitment','Pop. growth'),
                                     labels=c( 'Richness','Density', 'Recruitment', 'Population\ngrowth')),
                       n = paste0('N = ', c(rep(70,3), 38, rep(50,3), 23)),
                       yy=case_when(name=='Recruitment' ~ 5.75, 
                                    name == 'Density' ~ 6.75, .default = 0.25)),
              x=0, aes(label=n, y=yy), hjust=0)+
    facet_grid(name~taxa, switch='x')+
    scale_y_continuous('MAHI metrics')+
    scale_x_continuous('', limits=c(0,10), breaks=c(0,2,4,6,8,10))+ 
    theme_wendell+ theme(strip.text = element_text(size=10), strip.placement = 'outside', 
                         axis.title.x=element_blank()),
  ggplot()+theme_void(), nrow=1,rel_widths=c(.15, .7, .15), labels=c('','D','')),
  nrow=2, rel_heights=c(.25,.75))
ggsave('IFig2_spear_cor_samplesize.pdf', width = 6.5, height=8.5)

# alternative Fig 2
plot_grid(plot_grid(ggplot(data=f.m.comp.df, aes(x=mn.score_macroinvertebrates, y=mn.score_fish))+
                      #geom_linerange(aes(xmin=mn.score_macroinvertebrates-(sd.score_macroinvertebrates/sqrt(n.score_macroinvertebrates)),
                      #                  xmax=mn.score_macroinvertebrates+(sd.score_macroinvertebrates/sqrt(n.score_macroinvertebrates))))+
                      #geom_linerange(aes(ymin=mn.score_fish-(sd.score_fish/sqrt(n.score_fish)),
                      #                  ymax=mn.score_fish+(sd.score_fish/sqrt(n.score_fish))))+
                      #geom_abline()+ 
                      geom_smooth(method='lm', se=F, color='grey')+
                      geom_point()+
                      annotate('text', x=10*.05, y=10*.02, hjust=0,
                               label=expression(italic(r)*" = 0.38, "*italic(p)*" < 0.01"), size=3.5) + 
                      scale_x_continuous('MIBI', limits=c(0,10))+
                      scale_y_continuous('FIBI', limits=c(0,10))+
                      theme_wendell,
                    ibi.m %>% filter(taxa=='fish') %>%
                      ggplot()+geom_point(aes(x=stand.ibi, y=`MAHI composite`))+
                      annotate('text', x=10*.05, y=10*.98, hjust=0,
                               label=expression(italic(r)*" = 0.18, "*italic(p)*" = 0.19"), size=3.5) + 
                      scale_x_continuous('FIBI', limits = c(0,10))+
                      scale_y_continuous('MAHI composite', limits = c(0,10))+
                      theme_wendell,
                    ibi.m %>% filter(taxa=='macroinvertebrates') %>%
                      ggplot()+geom_point(aes(x=stand.ibi, y=`MAHI composite`))+
                      annotate('text', x=10*.05, y=10*.98, hjust=0,
                               label=expression(italic(r)*" = -0.17, "*italic(p)*" = 0.13"), size=3.5) + 
                      scale_x_continuous('MIBI ', limits = c(0,10))+
                      scale_y_continuous('MAHI composite', limits = c(0,10))+
                      theme_wendell, 
                    ggplot()+theme_void(), 
                    nrow=4, rel_heights = c(.3,.3,.3,.10), labels = c('A','B','C','')),
      ibi.m %>% select(SiteCode, taxa, stand.ibi, starts_with('MAHI')) %>%
              pivot_longer(starts_with('MAHI')) %>% filter(!is.na(value), name != 'MAHI composite') %>%
              mutate(taxa=case_match(taxa,'macroinvertebrates'~ 'MIBI', 'fish'~'FIBI',.default=taxa),
                     name = factor(name, 
                                   levels=c( 'MAHI p_richness', 'MAHI density', 'MAHI recruit','MAHI pop growth'),
                                   labels=c( 'Richness','Density', 'Recruitment', 'Pop. growth'))) %>% 
              ggplot()+
              geom_smooth(data = . %>% filter(name == 'Density'& taxa == 'FIBI'), 
                          aes(x=stand.ibi, y=value), method='lm', se=F, color='grey')+
              geom_point(aes(x=stand.ibi, y=value), pch=1)+
              geom_text(data=ibi.metrics %>% 
                          mutate(taxa=case_match(taxa,'macroinvertebrate'~ 'MIBI', 'fish'~'FIBI',.default=taxa),
                                 name = factor(name, 
                                               levels=c( 'Hist. richness', 'Density', 'Recruitment','Pop. growth'),
                                               labels=c( 'Richness','Density', 'Recruitment', 'Pop. growth')),
                                 lbl=paste0("italic(r)*' = '*", round(rho,2)),
                                 yy=case_when(name=='Recruitment' ~ 3.75, 
                                              name == 'Density' ~ 9.75, .default = 1.5)),
                        x=0, aes(y=yy, label=lbl), parse=T, hjust=0, size=3.5)+
              geom_text(data=ibi.metrics %>% 
                          mutate(taxa=case_match(taxa,'macroinvertebrate'~ 'MIBI', 'fish'~'FIBI',.default=taxa),
                                 name = factor(name, 
                                               levels=c( 'Hist. richness', 'Density', 'Recruitment','Pop. growth'),
                                               labels=c( 'Richness','Density', 'Recruitment', 'Pop. growth')),
                                 lbl=paste0("italic(p)*' = ", as.character(sprintf("%#.2f", p)), "'"),
                                 yy=case_when(name=='Recruitment' ~ 2.5, 
                                              name == 'Density' ~ 8.5, .default = 0.25)),
                        x=0, aes(label=lbl, y=yy), parse=T, hjust=0, size=3.5)+
              facet_grid(name~taxa, switch='x')+
              scale_y_continuous('MAHI metrics')+
              scale_x_continuous('', limits=c(0,10), breaks=c(0,2,4,6,8,10))+ 
              theme_wendell+ theme(strip.text = element_text(size=11), strip.placement = 'outside', 
                                   axis.title.x=element_blank(), strip.switch.pad.grid = margin(t=-3)),
          nrow=1, labels=c('','D'), rel_widths = c(.35, .65),  align = 'v', axis='top')
ggsave('IFig2_spear_cor_alternate.pdf', width = 6.5, height=7.25)

# MAHI metrics and IBIS ------
mr<-cor.test(x=ibi.m[ibi.m$taxa == 'macroinvertebrates',]$stand.ibi, y=ibi.m[ibi.m$taxa == 'macroinvertebrates',]$`MAHI p_richness`, method='pearson')
md<-cor.test(x=ibi.m[ibi.m$taxa == 'macroinvertebrates',]$stand.ibi, y=ibi.m[ibi.m$taxa == 'macroinvertebrates',]$`MAHI density`, method='pearson')
ma<-cor.test(x=ibi.m[ibi.m$taxa == 'macroinvertebrates',]$stand.ibi, y=ibi.m[ibi.m$taxa == 'macroinvertebrates',]$`MAHI recruit`, method='pearson')
ibi.m.m.pg<-ibi.m[ibi.m$taxa == 'macroinvertebrates',] %>% filter(!is.na(`MAHI pop growth`))
mp<-cor.test(x=ibi.m.m.pg$stand.ibi, y=ibi.m.m.pg$`MAHI pop growth`, method='pearson')

fr<-cor.test(x=ibi.m[ibi.m$taxa == 'fish',]$stand.ibi, y=ibi.m[ibi.m$taxa == 'fish',]$`MAHI p_richness`, method='pearson')
fd<-cor.test(x=ibi.m[ibi.m$taxa == 'fish',]$stand.ibi, y=ibi.m[ibi.m$taxa == 'fish',]$`MAHI density`, method='pearson')
fa<-cor.test(x=ibi.m[ibi.m$taxa == 'fish',]$stand.ibi, y=ibi.m[ibi.m$taxa == 'fish',]$`MAHI recruit`,  method='pearson')
ibi.m.f.pg<-ibi.m[ibi.m$taxa == 'fish',] %>% filter(!is.na(`MAHI pop growth`))
fp<-cor.test(x=ibi.m.f.pg$stand.ibi, y=ibi.m.f.pg$`MAHI pop growth`, method='pearson')
str(fr)
ibi.metrics<-data.frame(taxa=rep(c('macroinvertebrate', 'fish'), each=4),
           name=factor(rep(c('MAHI p_richness', 'MAHI density', 'MAHI recruit', 'MAHI pop growth'),2), 
                       levels=c('MAHI density', 'MAHI p_richness', 'MAHI pop growth', 'MAHI recruit'),
                       labels=c('Density', 'Hist. richness', 'Pop. growth', 'Recruitment')), 
           S=c(mr$statistic, md$statistic, ma$statistic, mp$statistic,
               fr$statistic, fd$statistic, fa$statistic, fp$statistic),
           p=c(mr$p.value, md$p.value, ma$p.value, mp$p.value,
               fr$p.value, fd$p.value, fa$p.value, fp$p.value),
           rho=c(mr$estimate, md$estimate, ma$estimate, mp$estimate,
                 fr$estimate, fd$estimate, fa$estimate, fp$estimate)) %>%
  mutate(across(c(S,p,rho), ~round(.x, 4)))
ibi.metrics %>% filter(p < 0.0125)
ibi.metrics %>% filter(p > 0.0125, p<0.06)
ibi.metrics %>% arrange(desc(rho))
ibi.metrics %>% arrange(desc(p))
nrow(ibi.m.f.pg); nrow(ibi.m.m.pg)
ibi.metrics

# impairment category and MAHI scores ----
lat303<-read_csv("C:/Users/tdubose/OneDrive - DOI/tpd/pre-appointment/IBI manuscript/site_EPA_impairment_codes_250109.csv")%>%
  # adding mahi scores
  left_join(mahi.scores, by='SiteCode') %>%
  filter(SiteCode != 'KYSLT',
         !is.na(`MAHI composite`))

lat303 %>% count(latest) %>% mutate(n/sum(n)*100) # how many sites in each category
lat303 %>% pivot_longer(c(`2008`:`2022`)) %>% filter(value != "NA") %>% # what is the last assessment year
  group_by(SiteCode) %>% summarize(yr=max(name)) %>% count(yr)
lat303 %>% filter(latest==3) %>% select(SiteCode, latest, `MAHI composite`) %>% summarize(median(`MAHI composite`, na.rm=T), IQR(`MAHI composite`, na.rm=T))
lat303 %>% filter(latest=='4C')

sites %>% filter(grepl('au', Notes), SiteCode %in% mahi.scores$SiteCode)

ir.ord<-c(5, '4','2','1')
ir.lab<-c('Impaired\nwithout\nTMDL','Impaired\nwith\nTMDL', 'Meeting\nstandards\nbut not all\nuses assessed', 'Meeting\nstandards')
df.303<-lat303 %>% dplyr::select(SiteCode, starts_with('MAHI'), latest) %>%
  mutate(IRf=factor(substr(latest,1,1), levels=ir.ord, 
                    labels = ir.lab)) %>%
  filter(latest != 3, latest != '4C') %>%
  mutate(lab.grp=case_when(latest == 5 & `MAHI composite` > 8~'over', 
                           (latest == 1|latest==2) & `MAHI composite` < 2 ~'under'),
         sts=substr(SiteCode, 1,2),
         MAHIc=`MAHI composite`)

kruskal.test(df.303$`MAHI composite`, df.303$IRf)
# median and IQR estimates of each IR category
df.303 %>% filter(!is.na(`MAHI composite`)) %>% group_by(latest) %>% summarize(median.sc=median(`MAHI composite`), IQR(`MAHI composite`), n=n())
# differences among IR categories for composite MAHI and metrics
df.303 %>% group_by(IRf) %>% 
  summarize(across(starts_with('MAHI'), list(~round(median(., na.rm=T),1), ~round(IQR(.,na.rm=T),1)))) %>% View()

# mahi metric score CV for different IR categories
df.303 %>% rowwise() %>% mutate(cv=sd(c(`MAHI p_richness`, `MAHI density`, `MAHI recruit`, `MAHI pop growth`), na.rm=T)/mean(c(`MAHI p_richness`, `MAHI density`, `MAHI recruit`, `MAHI pop growth`), na.rm=T)) %>%
  group_by(IRf) %>% summarize(median(cv, na.rm=T))

df.303 %>% filter(latest==1) %>% arrange(`MAHI composite`)
df.303 %>% filter((`MAHI composite` > 8 & IRf == 'Impaired without TMDL')|(`MAHI composite` < 2 & !grepl('Impaired', IRf))) %>% 
  arrange(latest) %>% View()

df.303 %>% pivot_longer(starts_with("MAHI")) %>% 
  filter(!is.na(value), name != 'MAHIc') %>%
  mutate(nameF=factor(name, 
                      levels=c('MAHI composite','MAHI p_richness','MAHI density', 'MAHI recruit', 'MAHI pop growth'), 
                      labels=c('Composite\nMAHI score', 'Richness','Density','Recruitment','Pop. growth'))) %>%
  ggplot()+
 
  #geom_text(data = . %>% filter(!is.na(lab.grp)), 
   #          aes(x=IRf, y=`MAHI composite`, label=SiteCode))+
  geom_point(aes(x=IRf, y=value), position=position_jitter(width=.25), pch=1, color='darkgrey')+
   geom_boxplot(aes(x=IRf, y=value), outlier.colour = NA, fill=NA)+
  #annotate('text', x=)
  scale_x_discrete('Integrated report category')+
  scale_y_continuous('Composite MAHI score', breaks=seq(0,10,2))+
  facet_grid(nameF~., switch='y') +
  theme_wendell+
  theme(axis.title.y=element_blank(), strip.placement='outside')
ggsave('IFig3_boxplot303.pdf', width=3.5, height=7)

library(ggrepel)
df.303 %>%
  ggplot()+
  geom_point(alpha=0.5, aes(x=IRf, y=`MAHI composite`))+
  geom_text_repel(aes(x=IRf, y=`MAHI composite`, label=SiteCode), 
            size=2.3, min.segment.length = 0)+
  stat_summary(aes(x=IRf, y=`MAHI composite`))+
  scale_x_discrete('Integrated report category')+
  scale_y_continuous('Composite MAHI score', breaks=seq(0,10,2))+
  theme_wendell
met.mods<-NULL
for(var in c('MAHI density', 'MAHI pop growth', 'MAHI p_richness', 'MAHI recruit')){
  df.303.m<-df.303 %>% rename(MahiMet=all_of(var)) %>%
    dplyr::select(SiteCode, IRf, MahiMet) %>%
    filter(!is.na(MahiMet))
  
  mod<-kruskal.test(MahiMet~IRf, data=df.303.m)
  met.mods<-bind_rows(met.mods,
            data.frame(dep='IR factor', variable=var, 
             df1=mod$parameter, ngrps=length(unique(df.303.m$IRf)), n=nrow(df.303.m),
             Xval=mod$statistic, pval=mod$p.value))
}
met.mods

# use designations =======
use.ass<-readRDS('C:/Users/tdubose/OneDrive - DOI/tpd/pre-appointment/IBI manuscript/303d_Use_Assessments_250109.rds')
lat.use<-use.ass %>% group_by(assessmentUnitIdentifier) %>% 
  filter(max(as.numeric(reportingCycleText))==as.numeric(reportingCycleText)) %>%
filter(assessmentUnitIdentifier %in% 
         (lat303 %>% filter(!(latest %in% c('3','4C'))) %>% 
            pull(assessmentUnitIdentifier)))
lat.use %>% count(assessmentUnitIdentifier) %>% filter(n>1)

lat.use1<-lat.use %>% 
  filter(assessmentUnitIdentifier %in% 
                      (lat303 %>% filter(!(latest %in% c('3','4C'))) %>% 
                         pull(assessmentUnitIdentifier)))
lat303$SiteCode[(!(lat303$assessmentUnitIdentifier %in% lat.use1$assessmentUnitIdentifier))]

use.sum<-NULL
for(i in 1:nrow(lat.use)){
  tst<-lat.use[i,9][[1]][[1]]
  if(!is.null(tst)){
  use.sum<-bind_rows(use.sum, tst[,c(1:5,7)] %>% as_tibble() %>%
                       mutate(i=i, assessmentUnitIdentifier=as.character(unique(lat.use[i,6])), 
                              reportingCycleText=as.character(unique(lat.use[i,4]))))
  }
}
use.sum 
length(unique(use.sum$assessmentUnitIdentifier)) # 76 sites
use.sum<-use.sum %>%
  left_join(lat303 %>% select(SiteCode, assessmentUnitIdentifier), by='assessmentUnitIdentifier') %>%
  left_join(mahi.scores %>% select(SiteCode, `MAHI composite`), by='SiteCode') %>%
  filter(!is.na(`MAHI composite`),
         !(SiteCode %in% c('KYLIC','NCLTN','OHWAL'))) # removing those with IR insufficient or 4C
use.sum %>% filter(is.na(SiteCode)) 
use.sum %>% count(useName)
length(unique(use.sum$assessmentUnitIdentifier)); length(unique(use.sum$SiteCode))
View(use.sum)
# categorize designated uses and create table 2
use.sum<-use.sum %>% 
 mutate(use.category=case_when(  grepl('human heal', tolower(useName)) |
                                    grepl('consumption', tolower(useName)) ~ 'human health use',
                                  grepl('aquatic l', tolower(useName))|
                                  useName == 'Outstanding State Resource Water' ~ 'aquatic life', 
                                grepl('contact', tolower(useName))|
                                  grepl('recreation', tolower(useName))|
                                  useName %in% c('Boating and Canoeing', 'Fishing') ~ 'contact recreation',
                                grepl('water supply', tolower(useName))|
                                  useName == 'Drinking Water' ~ 'water supply',
                                grepl('watering', tolower(useName))|
                                  useName %in% c('Agriculture and Wildlife', 'Wildlife') ~ 'livestock and wildlife use',
                                grepl('fishe', tolower(useName))|
                                  useName %in% c('Propagation of Fish and Wildlife','Warm Water Aquatic Habitat') ~ 'fisheries',
                                grepl('industrial', tolower(useName))|
                                  grepl('irrigation', tolower(useName)) ~'industrial and agricultural use',
                                T~useName)) 
use.sum  %>% mutate(un=tolower(useName)) %>%
  group_by(use.category, un) %>% count() %>% arrange(desc(n)) %>%
  group_by(use.category) %>%
  summarize(use.names=paste(paste0(un, ' (', n, ')'), collapse='; ')) %>% #View()
  write.csv('IBI man data/ITable2_usecats.csv', row.names = F)
use.sum %>% distinct(use.category, SiteCode) %>% count(use.category)
# within aquatic life and fisheries, which are supporting, etc
use.sum %>% distinct(assessmentUnitIdentifier, use.category, .keep_all=T) %>%
  filter(use.category %in% c('fisheries', 'aquatic life')) %>%
  group_by(use.category, useAttainmentCodeName) %>%
  summarize(n=n(), sites=paste(unique(SiteCode), collapse='; '))
# almost all sites include aquatic life in their usage
use.sum %>% distinct(assessmentUnitIdentifier, use.category) %>%
  filter(use.category %in% c('fisheries', 'aquatic life')) %>% 
  count(use.category)
mahi.scores %>% 
  filter(!(SiteCode %in% (use.sum %>% 
             filter(use.category %in% c('fisheries', 'aquatic life')) %>% 
             pull(SiteCode) %>% unique())),
         !is.na(`MAHI composite`))
use.sum %>% filter(SiteCode %in% c('GAARM','GACON')) %>% View()

us.F.df<-use.sum %>% 
  filter(use.category %in% c('fisheries', 'aquatic life', 'human health use', 'contact recreation', 'water supply')) %>%
  dplyr::select(SiteCode, use.category, useAttainmentCodeName) %>%
  mutate(useAF=factor(ifelse(useAttainmentCodeName =='Not Assessed',
                             'Insufficient Information', useAttainmentCodeName), 
                      levels=c('Not Supporting','Fully Supporting'),
                      labels=c('Not\nsupporting','Supporting'))) %>%
  filter(!is.na(useAF)) %>% # removing insufficient info or not assessed
  left_join(mahi.scores, by='SiteCode') %>%
  filter(!is.na(`MAHI composite`)) 
us.F.df %>% 
  filter(use.category %in% c('fisheries', 'aquatic life', 'human health use', 'contact recreation')) %>%
  pivot_longer(starts_with('MAHI')) %>%
  filter(!is.na(value)) %>%
  mutate(nameF=factor(name, 
                      levels=c('MAHI composite','MAHI p_richness','MAHI density', 'MAHI recruit', 'MAHI pop growth'), 
                      labels=c('Composite\nMAHI score', 'Richness','Density','Recruitment','Pop. growth')),
         usC=factor(use.category, levels=c('aquatic life', 'fisheries', 'human health use', 'contact recreation'),
                    labels=c('Aquatic life', 'Fisheries', 'Human health', 'Human contact'))) %>%
  ggplot(aes(x=useAF, y=value))+
  geom_boxplot(fill=NA, outlier.colour = NA)+
  geom_point(position=position_jitter(width=.25), pch=1, color='darkgrey')+
  # adding in sig for human contact
  geom_text(data=data.frame(usC=factor('contact recreation', levels=c('aquatic life', 'fisheries', 'human health use', 'contact recreation'),
                                       labels=c('Aquatic life', 'Fisheries', 'Human health', 'Human contact')), 
                            nameF= factor('MAHI p_richness', 
                                          levels=c('MAHI composite','MAHI p_richness','MAHI density', 'MAHI recruit', 'MAHI pop growth'), 
                                          labels=c('Composite\nMAHI score', 'Richness','Density','Recruitment','Pop. growth'))),
            label=expression(italic(p)*" < 0.01"), 
            x=2, y=1, size=3)+
  # adding in sig for human contact
  geom_text(data=data.frame(usC=factor('contact recreation', levels=c('aquatic life', 'fisheries', 'human health use', 'contact recreation'),
                                       labels=c('Aquatic life', 'Fisheries', 'Human health', 'Human contact')), 
                            nameF= factor('MAHI composite', 
                                          levels=c('MAHI composite','MAHI p_richness','MAHI density', 'MAHI recruit', 'MAHI pop growth'), 
                                          labels=c('Composite\nMAHI score', 'Richness','Density','Recruitment','Pop. growth'))),
            label=expression(italic(p)*" = 0.04"), 
            x=2, y=9.75, size=3)+
  scale_x_discrete('Use attainment')+
  scale_y_continuous('MAHI score', breaks=seq(0,10,2))+
  facet_grid(nameF~usC, switch='y')+
  theme_wendell+
  theme(strip.placement='outside', axis.title.y=element_blank())
ggsave('IFig4_use_attain.pdf', width=6.5, height=6)

# investigate mahi metrics among use attainment groups
library(rcompanion)
met.mods.use<-NULL
for(useee in c('aquatic life', 'fisheries', 'human health use', 'contact recreation', 'water supply')){
for(var in c('MAHI composite', 'MAHI density', 'MAHI pop growth', 'MAHI p_richness', 'MAHI recruit')){
  usFdf.m<-us.F.df %>% rename(MahiMet=all_of(var)) %>%
    dplyr::select(SiteCode, use.category, useAF, MahiMet) %>%
    filter(use.category == useee, !is.na(MahiMet))
  mod<-wilcox.test(MahiMet~useAF, data=usFdf.m)
  met.mods.use<-bind_rows(met.mods.use,
                      data.frame(use.category=useee, dep='Use attainment factor', 
                                 variable=var, 
                                 ngrps=length(unique(usFdf.m$useAF)), n=nrow(usFdf.m),
                                 U=mod$statistic, pval=mod$p.value, A = vda(MahiMet~useAF, data=usFdf.m)[[1]]))
}
}

met.mods.use %>% filter(pval < 0.05)
met.mods.use %>% filter(variable == 'MAHI composite') %>% arrange(desc(pval))
View(met.mods.use)
us.F.df %>% filter(use.category == 'contact recreation') %>%
  group_by(useAF) %>%
  summarize(across(c('MAHI composite', 'MAHI p_richness'), median), n())


us.F.df %>% group_by(use.category, useAF) %>%
  summarize(across(c(`MAHI p_richness`,`MAHI density`,`MAHI recruit`),
                   list(median=median, IQR=IQR))) %>% arrange(use.category)
us.F.df %>% group_by(use.category, useAF) %>% filter(!is.na(`MAHI pop growth`)) %>%
  summarize(across(c(`MAHI pop growth`),
                   list(median=median, IQR=IQR)))
us.F.df %>% group_by(use.category, useAF) %>% 
  summarize(across(c(`MAHI composite`),
                   list(median=median, IQR=IQR)))

df.303 %>% filter(!is.na(`MAHI composite`)) %>%
  group_by(IRf) %>%
  summarize(across(starts_with('MAHI'), 
                   list(~median(.x,na.rm=T), ~IQR(.x, na.rm=T)))) %>% View()
df.303 %>% summarize(across(starts_with('MAHI'), ~sum(!is.na(.x)))) %>% select(-`MAHI composite`)

# mean and SD at each use category
us.F.df %>% group_by(use.category, useAF) %>% 
  summarize(across(starts_with('MAHI'), 
                   list(~round(median(., na.rm=T),1), ~round(IQR(.,na.rm=T),1)))) %>% View()
# mahi metric score CV for different use attaining groups
us.F.df %>% rowwise() %>% mutate(cv=sd(c(`MAHI p_richness`, `MAHI density`, `MAHI recruit`, `MAHI pop growth`), na.rm=T)/mean(c(`MAHI p_richness`, `MAHI density`, `MAHI recruit`, `MAHI pop growth`), na.rm=T)) %>%
  group_by(use.category, useAF) %>% summarize(median(cv, na.rm=T))
us.F.df %>% group_by(use.category, useAF) %>% summarize(median(`MAHI composite`, na.rm=T))
us.F.df %>% group_by(use.category, useAF) %>% summarize(IQR(`MAHI composite`, na.rm=T))

# causes of impairments ------
cause303<-read.csv('C:/Users/tdubose/OneDrive - DOI/tpd/pre-appointment/IBI manuscript/303d_source_cause_groups_latest_250109.csv') %>% 
  left_join(mahi.scores %>% select(SiteCode, `MAHI composite`), by='SiteCode') %>%
  filter(!is.na(`MAHI composite`)) %>%
  filter(assessmentUnitIdentifier %in% (lat303 %>% filter(latest %in% c('4A', '5')) %>% pull(assessmentUnitIdentifier))) %>%
  distinct(grp, assessmentUnitIdentifier, reportingCycleText, .keep_all = T) %>%
  group_by(assessmentUnitIdentifier) %>% 
  filter(as.numeric(reportingCycleText)==max(as.numeric(reportingCycleText))) %>% ungroup() %>%
  mutate(grp=recode(grp, 'high fecal coliform' = 'Fecal coliform/unknown source', 
                    'mercury'='Mercury', 'source unknown-pesticides or herbicides' = 'Pesticides, herbicides',
                    'fecal coliform from livestock'='Fecal coliform/livestock',
                    'unknown cause' = 'Unknown cause', 'PCBs' = 'Polychlorinated biphenyls',
                    'mining-associated solutes'='Mining-associated solutes',
                    'muddy water'='Siltation, turbidity',
                    'nutrient enrichment'='Nutrient enrichment',
                    'human sewage-fecal coliform'='Human sewage',
                    .default=grp),
         grp = ifelse(pair == "agriculture-fecal coliform","Fecal coliform/livestock", grp)) # fixing INTPP misclassification
cause303 %>% count(grp) %>%arrange(desc(n))
cause303 %>% group_by(SiteCode) %>% 
  summarize(causes=paste(grp, collapse='; ')) %>%
  left_join(mahi.scores, by='SiteCode') %>%
  left_join(lat303 %>% select(SiteCode, latest), by='SiteCode') %>% View()
lat303[lat303$latest %in% c('4A','5'), ]$SiteCode[!lat303[lat303$latest %in% c('4A','5'), ]$SiteCode %in% cause303$SiteCode]

# ALSO NEED TO ADD TNEFS to bioassessments!!! based on water quality report on hows my watershed
cause303 %>% distinct(grp, pair, assessmentUnitIdentifier, reportingCycleText, .keep_all = T) %>%
    group_by(assessmentUnitIdentifier) %>% 
  filter(as.numeric(reportingCycleText)==max(as.numeric(reportingCycleText))) %>% ungroup() %>%
  group_by(pair) %>% mutate(n_p=n(), newpair=paste0(pair, ' (', n_p, ')')) %>%
  group_by(grp) %>% arrange(desc(n_p)) %>%
  summarize(n_sites=length(unique(assessmentUnitIdentifier)), 
            pairsss=paste(unique(newpair), collapse='; ')) %>% 
  arrange(desc(n_sites))  %>% #View() #%>%
write.csv('IBI man data/ITable3_pairs_causes.csv', row.names = F)
cause303 %>% distinct(grp, pair, assessmentUnitIdentifier, reportingCycleText, .keep_all = T) %>%
  group_by(assessmentUnitIdentifier) %>% 
  filter(as.numeric(reportingCycleText)==max(as.numeric(reportingCycleText))) %>% ungroup() %>%
  filter(grp == 'Pesticides, herbicides')

cause303 %>% filter(grp=='unknown cause') %>% select(SiteCode, pair, `MAHI composite`, grp)
cause303 %>% filter(grepl('fecal', grp)) %>% arrange(grp) %>% View()

c303.bin<-cause303 %>% as_tibble() %>% select(-pair, -starts_with('MAHI')) %>%
  right_join(mahi.scores %>% filter(!is.na(SiteCode), !is.na(`MAHI composite`)), by='SiteCode') %>%
  filter(!(SiteCode %in% c('OHWAL','KYLIC','NCLTN'))) %>% # removing insufficient info and non pollutant
  mutate(MAHI.cat=factor(ifelse(`MAHI composite` <=5, 'degraded','healthy'), levels=c('degraded','healthy'))) %>%
  # set up contrasts for lhs
  mutate(pres=1) %>% pivot_wider(names_from = grp, values_from=pres, values_fill = 0) %>%
  mutate(across(`Siltation, turbidity`:`Unknown cause`, ~factor(.x, levels=c(0,1))))

fisher.test(c303.bin$`Fecal coliform/livestock`, c303.bin$MAHI.cat, alternative = 'less')

cause303 %>% ggplot()+geom_histogram(aes(x=`MAHI composite`, fill=grp))+facet_wrap(~grp) # nonnormal

c303.met.mods<-NULL
for(j in unique(cause303$grp)[-c(5,8)]){
grp.df <- cause303 %>% select(-`MAHI composite`, -State) %>%
  filter(grp == j, 
         !(SiteCode %in% c('OHWAL','KYLIC','NCLTN'))) %>%  # removing insufficient info and non pollutant
  left_join(mahi.scores, by=c('SiteCode')) %>%
  select(-State, -Year, -`Watershed area (km2)`)
for(var in c('MAHI composite', 'MAHI density', 'MAHI pop growth', 'MAHI p_richness', 'MAHI recruit')){
  c303.grp.m<-grp.df %>% rename(MahiMet=all_of(var)) %>%
    dplyr::select(SiteCode, MahiMet) %>%
    filter(!is.na(MahiMet))
  mod<-wilcox.test(c303.grp.m$MahiMet, mu=5)
  c303.met.mods<-bind_rows(c303.met.mods,
                      data.frame(variable=var, group=j, n=nrow(c303.grp.m), median = median(c303.grp.m$MahiMet),
                                 WilcoxStat=mod$statistic, pval=mod$p.value, r=wilcoxonOneSampleRC(c303.grp.m$MahiMet, mu=5)[[1]]))
}
}
# when only one is used, tests the null that X is symmetrically distributed around the mean (mu)
# so alternative is that X is NOT symettically distributed around mu -- more >5 or < 5 than expected
c303.met.mods %>% filter(pval < 0.05)
cause303 %>% filter(grp %in% c('Mercury', 'Fecal coliform/unknown source')) %>%
  left_join(mahi.scores, by=c('SiteCode', 'MAHI composite')) %>%
  group_by(grp) %>%
  summarize(across(c('MAHI composite', 'MAHI p_richness'), list(median=median, min=min, max=max)))

c303.met.mods %>% select(-median) %>%
  mutate(txt=paste0('W = ',round(WilcoxStat,1), '; p = ', round(pval,2), '; n = ',n )) %>% select(-n, -WilcoxStat, -pval) %>%
  pivot_wider(names_from=variable, values_from=txt) %>%
  arrange(group, `MAHI composite`, `MAHI p_richness`,`MAHI density`, `MAHI recruit`, `MAHI pop growth`)
write.csv(c303.met.mods, 'IBI man data/ITs8_wilcox_sym.csv', row.names = F)

caus.ord<-rev(c('Fecal coliform/unknown source','Fecal coliform/livestock','Human sewage', 'Mercury','Polychlorinated biphenyls','Pesticides, herbicides',
            'Mining-associated solutes','Siltation, turbidity', 'Nutrient enrichment', 'Unknown cause'))
caus.lab<-rev(c('Fecal coliform/\nunknown source','Fecal coliform/\nlivestock','Fecal coliform/\nhuman sewage', 'Mercury','Polychlorinated\nbiphenyls','Pesticides,\nherbicides',
            'Mining-associated\nsolutes','Siltation, turbidity', 'Nutrient\nenrichment', 'Unknown cause'))
cplot.df<-cause303 %>% left_join(mahi.scores, by=c('SiteCode','MAHI composite')) %>%
  distinct(SiteCode, grp, .keep_all = T) %>%
  mutate(grpF=factor(grp, levels=rev(caus.ord), labels=rev(caus.lab))) %>%
  pivot_longer(starts_with('MAHI')) %>% 
  filter(!is.na(value)) %>%
  mutate(name=recode(name, .default=name, 'MAHI composite'='Composite score', 'MAHI density'='Density', 'MAHI p_richness'='Richness','MAHI pop growth'='Population growth', 'MAHI recruit'='Recruitment')) %>%
  mutate(name=factor(name, levels=c('Composite score','Richness','Density', 'Recruitment','Population growth')))
ggplot(data=cplot.df, aes(x=grpF))+
  geom_hline(yintercept = 5, color='lightgrey', linetype='dashed')+
  geom_errorbar(data=cplot.df %>% group_by(grpF, name) %>% 
                  summarize(x25=quantile(value,0.25), x75=quantile(value,0.75), grpp='sum', .groups='drop'), 
                aes(ymin=x25, ymax=x75, x=as.numeric(grpF)+.125, group='sum'), width=0)+ 
  geom_point(data=cplot.df %>% group_by(grpF, name) %>% 
                  summarize(value=median(value), grpp='sum', .groups='drop') %>%
               bind_rows(cplot.df %>% mutate(grpp='pts')), 
                aes(y=value, group=grpp, color=grpp, shape=grpp), position = position_dodge(width=.5), size=1.7)+ 
  #geom_point(pch=1, size=1, aes(group='points', x=value), color='grey')+
  geom_text(data=. %>% mutate(labelll=case_when(grp == 'Mercury' & name == 'Composite score' ~ "italic(p)*' = 0.01'", 
                                                grp == 'Mercury' & name == 'Richness' ~  "italic(p)*' = 0.01'", 
                                                grp == 'Fecal coliform/unknown source' & name == 'Richness' ~ "italic(p)*' = 0.01'")) %>%
              filter(!is.na(labelll)) %>% group_by(grpF, name) %>%
              summarize(value=median(value), grpp='sum', .groups='drop') %>%mutate(ast="*"),
            aes(y=value-0.25, x=as.numeric(grpF)+0.25, label=ast), size=5, hjust=0, color='black')+
  scale_x_discrete('Pollutant/source category')+
  scale_color_manual(guide='none', values=c('grey','black'))+scale_shape_manual(values=c(1,19), guide='none')+
  scale_y_continuous('Metric value', limits=c(0,10), breaks=seq(0,10,2))+
  facet_grid(name~., switch='y')+
  theme_wendell+ theme(axis.text.x=element_text(angle=30, hjust=.85, vjust=.90),
                       strip.placement = 'outside')
ggsave('IFig5_MAHI_cause_boxplots.pdf', width=6.5, height=6.25)

# discussion causes ====
cause303 %>% left_join(sites %>% select(SiteCode, Address)) %>%
  filter(grp == 'unknown cause') %>% 
  select(assessmentUnitIdentifier,reportingCycleText, pair, `MAHI composite`, Address) %>%
  arrange(desc(`MAHI composite`)) %>% View()

# Figure S6: supplement box plot figure =====
plot_grid(df.303 %>% dplyr::select(-`MAHI composite`) %>%
            pivot_longer(starts_with('MAHI')) %>%
            ggplot()+
            geom_point(aes(y=IRf, x=value), alpha=0.2, position=position_jitter(height=0.3, width=0))+
            geom_boxplot(aes(y=IRf, x=value), outlier.color = NA, fill=NA)+
            
            facet_grid(name~.)+theme_wendell+
            ggtitle('EPA IR\ncategory')+
            theme(strip.text.y = element_blank() , 
                  strip.background = element_blank(),
                  plot.margin = unit( c(0,0,0,0) , units = "lines" ,),
                  plot.title=element_text(size=10),
                  axis.title=element_blank()),
          usAL %>%dplyr::select(-`MAHI composite`) %>%
            pivot_longer(starts_with('MAHI')) %>%
            ggplot()+
            geom_point(aes(y=useAF, x=value), alpha=0.2, position=position_jitter(height=0.3, width=0))+
            geom_boxplot(aes(y=useAF, x=value), outlier.color = NA, fill=NA)+
            facet_grid(name~.)+theme_wendell+
            scale_x_continuous('MAHI metric score')+
              ggtitle('Aquatic life\ndesignated use')+
            theme(strip.text.y = element_blank() , 
                  strip.background = element_blank(),
                  plot.margin = unit( c(0,0,0,0) , units = "lines" ,),
                  plot.title=element_text(size=10),
                  axis.title.y=element_blank()),
          usF %>%dplyr::select(-`MAHI composite`) %>%
            pivot_longer(starts_with('MAHI')) %>%
            mutate(name = factor(name, levels=c( 'MAHI p_richness','MAHI density', 'MAHI recruit', 'MAHI pop growth'),
                                 labels=c( 'Richness','Density',  'Recruitment','Pop. growth'))) %>%
            ggplot()+
            geom_point(aes(y=useAF, x=value), alpha=0.2, position=position_jitter(height=0.3, width=0))+
            geom_boxplot(aes(y=useAF, x=value), outlier.color = NA, fill=NA)+
            facet_grid(name~.)+theme_wendell+
            ggtitle('Fisheries\ndesignated use')+
            theme(plot.title =element_text(size=10),
                  axis.text.y = element_blank(), 
                  axis.title=element_blank()),
          align = 'h', axis='bottom', nrow=1, rel_widths = c(.40,.30,.20), labels='AUTO')
ggsave('results/IFigS6_metric_diffs.jpg', width=7, height=6)

# Figure 1 map ======
# using code from EPATADA to query attains units
mahi.scores.sf<- mahi.scores %>% filter(!is.na(SiteCode), !is.na(`MAHI composite`)) %>%
  st_as_sf(coords=c('Longitude','Latitude'), crs = st_crs(sites))

attains.sf<-NULL
for(i in c(1:24, 26:nrow(mahi.scores.sf))){ # skipped 25
  bbox_raw <- mahi.scores.sf[i,] %>% st_buffer(100) %>% st_bbox() 
  bbox <- bbox_raw %>%
    # convert bounding box to characters
    toString(.) %>%
    # encode for use within the API URL
    urltools::url_encode(.)
  query<-urltools::param_set("https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1/query?", key = "geometry",
                             value = bbox) %>%
    urltools::param_set(key = "inSR", value = 4326) %>%
    urltools::param_set(key = "resultRecordCount", value = 2000) %>%
    # ... starting at the "offset":
    urltools::param_set(key = "resultOffset", value = 0) %>%
    urltools::param_set(key = "spatialRel", value = "esriSpatialRelIntersects") %>%
    urltools::param_set(key = "f", value = "geojson") %>%
    urltools::param_set(key = "outFields", value = "*") %>%
    urltools::param_set(key = "geometryType", value = "esriGeometryEnvelope") %>%
    urltools::param_set(key = "returnGeometry", value = "true") %>%
    urltools::param_set(key = "returnTrueCurves", value = "false") %>%
    urltools::param_set(key = "returnIdsOnly", value = "false") %>%
    urltools::param_set(key = "returnCountOnly", value = "false") %>%
    urltools::param_set(key = "returnZ", value = "false") %>%
    urltools::param_set(key = "returnM", value = "false") %>%
    urltools::param_set(key = "returnDistinctValues", value = "false") %>%
    urltools::param_set(key = "returnExtentOnly", value = "false") %>%
    urltools::param_set(key = "featureEncoding", value = "esriDefault")
  ddd<-geojsonsf::geojson_sf(query)
  attains.sf<-bind_rows(attains.sf,
                        ddd %>% select(assessmentunitname, assessmentunitidentifier, region, ecological_use, recreation_use) %>%
                          bind_cols(SiteCode = mahi.scores.sf[i,]$SiteCode))
}

attains.sf$SiteCode %>% unique() %>% length() # all in here
nrow(attains.sf) # but picked up extra lines
missing.sites<-mahi.scores.sf$SiteCode[!(mahi.scores.sf$SiteCode %in% (attains.sf %>% 
                                                                         filter(assessmentunitidentifier %in% c(sites$ATTAINSau, 'MN07040002-509',
                                                                                                                'MN07020004-747','MN07020002-501',
                                                                                                                'MN07030004-525', 'MN07040002-535')) %>%
                                                                         pull(SiteCode) %>% unique()))]
missing.sites<- missing.sites[!(missing.sites %in% c('KYBFK','KYSFK','OHWAL'))] # not interested in because not fully assessed for pollutants
missing.rivers <- c('TIPPE','WABASH', 'Holston','Elk','Hacker','Kanawha','West Fork')
missing.aus<-bind_rows(attains.sf %>% filter(grepl(missing.rivers[1], assessmentunitname)|grepl(missing.rivers[2], assessmentunitname)|grepl(missing.rivers[3], assessmentunitname)|
                                     grepl(missing.rivers[4], assessmentunitname)|grepl(missing.rivers[5], assessmentunitname)|grepl(missing.rivers[6], assessmentunitname)|
                                     grepl(missing.rivers[7], assessmentunitname),
                                   SiteCode %in% missing.sites),
                       attains.sf %>% filter(SiteCode %in% missing.sites[c(1,2,5,6)]))

ggplot()+geom_sf(data=mahi.scores.sf %>% filter(SiteCode == 'INTPP'))+geom_sf(data=attains.sf %>% filter(SiteCode == 'INTPP'))
peak<-st_transform(mahi.scores.sf[mahi.scores.sf$SiteCode %in% missing.sites,], crs=st_crs(attains.sf)) %>%
  st_join(missing.aus, join=st_nearest_feature)
peak$SiteCode.x == peak$SiteCode.y

ggplot()+geom_sf(data=mahi.scores.sf %>% filter(SiteCode == 'INWLD'))+
  geom_sf(data=attains.sf %>% filter(SiteCode == 'INWLD'))

ggplot()+geom_sf(data=mahi.scores.sf %>% filter(SiteCode == 'INTPP'))+
  geom_sf(data=attains.sf %>% filter(assessmentunitidentifier == peak[peak$SiteCode.x == 'INTPP',]$assessmentunitidentifier))
ggplot()+geom_sf(data=mahi.scores.sf %>% filter(SiteCode == 'INTPP'))+
  geom_sf(data=attains.sf %>% filter(assessmentunitidentifier == peak[peak$SiteCode.x == 'INTPP',]$assessmentunitidentifier))
ggplot()+geom_sf(data=mahi.scores.sf %>% filter(SiteCode == 'INWBA'))+
  geom_sf(data=attains.sf %>% filter(assessmentunitidentifier == peak[peak$SiteCode.x == 'INWBA',]$assessmentunitidentifier))
ggplot()+geom_sf(data=mahi.scores.sf %>% filter(SiteCode == 'VAMFH'))+
  geom_sf(data=attains.sf %>% filter(assessmentunitidentifier == peak[peak$SiteCode.x == 'VAMFH',]$assessmentunitidentifier))
ggplot()+geom_sf(data=mahi.scores.sf %>% filter(SiteCode == 'WVHAC'))+
  geom_sf(data=attains.sf %>% filter(assessmentunitidentifier == peak[peak$SiteCode.x == 'WVHAC',]$assessmentunitidentifier))
ggplot()+geom_sf(data=mahi.scores.sf %>% filter(SiteCode == 'WVLKW'))+
  geom_sf(data=attains.sf %>% filter(assessmentunitidentifier == peak[peak$SiteCode.x == 'WVLKW',]$assessmentunitidentifier))
ggplot()+geom_sf(data=mahi.scores.sf %>% filter(SiteCode == 'WVELK'))+
  geom_sf(data=attains.sf %>% filter(assessmentunitidentifier == peak[peak$SiteCode.x == 'WVELK',]$assessmentunitidentifier))
ggplot()+geom_sf(data=mahi.scores.sf %>% filter(SiteCode == 'WVWFK'))+
  geom_sf(data=attains.sf %>% filter(assessmentunitidentifier == peak[peak$SiteCode.x == 'WVWFK',]$assessmentunitidentifier))

au.sf<-bind_rows(attains.sf %>% 
                   filter(assessmentunitidentifier %in% c(sites$ATTAINSau, 'MN07040002-509', 'MN07020004-747','MN07020002-501',
                                                          'MN07030004-525', 'MN07040002-535'),
                          SiteCode != 'OHWAL'),
                 attains.sf %>% filter(assessmentunitidentifier %in% peak$assessmentunitidentifier))
length(unique(au.sf$SiteCode))
mahi.scores.sf$SiteCode[!(mahi.scores.sf$SiteCode %in% au.sf$SiteCode)]

library(maps)
states<-st_as_sf(map('state', fill=T, plot=F))
sf_use_s2(F)
statesc<-st_crop(states %>% st_transform(st_crs(sites)) %>% st_buffer(0),
                 st_bbox(mahi.scores.sf %>% st_buffer(.5)))
canada<-st_as_sf(map('world','canada', fill=T, plot=F)) %>%
  st_transform(st_crs(sites)) %>% st_buffer(0) %>%
  st_crop(st_bbox(mahi.scores.sf %>% st_buffer(.5)))
usa<-st_as_sf(map('world',fill=T, plot=F)) %>%
  st_crop(xmin=-178.2, xmax=-49.0, ymin=6.6, ymax=83.3) 
ins.bb<-st_bbox(sites %>% st_buffer(.5)) %>% st_as_sfc()
us_inset<-ggplot()+geom_sf(data=usa) + 
  geom_sf(data=ins.bb, fill=NA, color='red')+
  coord_sf(expand=FALSE)+
  theme_void()+
  theme(plot.background = element_rect(fill='white',color='black') )
us_inset
(big.map<-mahi.scores.sf %>%
  left_join(ibi.mm %>% distinct(SiteCode, .keep_all=T) %>% select(SiteCode, taxa, stand.ibi)) %>%
  ggplot()+
  geom_sf(data=statesc) + geom_sf(data=canada)+
  #geom_sf(data=riv7)+
  geom_sf(aes(shape='aMAHI data', fill='aMAHI data'), alpha=0.4)+
  geom_sf(data=ibi.mm[ibi.mm$taxa == 'fish',], 
          aes(shape='cIBI score', fill='cIBI score', geometry=geometry), alpha=0.4) +
  geom_sf(data=ibi.mm[ibi.mm$taxa == 'macroinvertebrates',], 
          aes(shape='cIBI score', fill='cIBI score', geometry=geometry), alpha=0.4)+
 # geom_sf(data=au.sf, aes(color='d303(d) IR unit'), linetype='solid')+
  geom_line(data=data.frame(nm='1', long=c(-95.0, -94.0), lat=c(34.0, 36.0)), 
            aes(color='303(d)\nassessment\nunit', x=long, y=lat), linewidth=1)+
    scale_shape_manual('Locations', values=c(21,22), 
                     labels=function(x){substr(x, 2, nchar(x))})+
  scale_fill_manual('Locations', values=c('black','white'), 
                    labels=function(x){substr(x, 2, nchar(x))})+
    scale_color_manual('Locations', values='darkgrey')+
  scale_x_continuous(expand=c(0,0), limits=c(-95.62640,-77.04041))+
  scale_y_continuous(expand=c(0,0), limits=c(33.33267, 46.19911))+
  theme_bw()+
    theme(legend.title=element_blank(), legend.background = element_rect(fill=NA),
          legend.key.size = unit(0.75, 'lines'),
          legend.text=element_text(size=9)
          )+
  #theme(legend.position='bottom', legend.margin = margin(t=-15), legend.title = element_blank())+
  guides(fill = guide_legend(override.aes = list(alpha = c(1,1))))+
  draw_plot(us_inset, x=-80.5, y=43.75, scale=6))
mc.mac<- mahi.scores.sf %>%
  left_join(ibi.m %>% filter(taxa == 'macroinvertebrates') %>%
              distinct(SiteCode, .keep_all=T) %>% select(SiteCode, taxa, stand.ibi), by='SiteCode') %>%
  filter(!is.na(stand.ibi)) %>%
  ggplot()+
  geom_sf(data=statesc) + geom_sf(data=canada)+
  geom_sf()+#aes(shape='aMAHI data', fill='aMAHI data'))+
   # geom_sf(data=ibi.mm[ibi.mm$taxa == 'macroinvertebrates',], 
   #         aes(#shape='cIBI', fill='cIBI', 
   #           geometry=geometry), alpha=0.5)+
  # scale_shape_manual('Locations', values=c(21,22), labels=function(x){substr(x, 2, nchar(x))})+
  # scale_fill_manual('Locations', values=c('black','white'), labels=function(x){substr(x, 2, nchar(x))})+
  scale_x_continuous(expand=c(0,0), limits=c(-96.52662,-77.04041))+
  scale_y_continuous(expand=c(0,0), limits=c(33.33267, 46.19911))+
  theme_bw()+ theme(legend.position='none', panel.grid.major = element_blank())+
  guides(colour = guide_legend(override.aes = list(alpha = c(1,1,.4))))+
  ggtitle('Macroinvertebrate IBI')

mc.fish<-mahi.scores.sf %>%
  left_join(ibi.m %>% filter(taxa == 'fish') %>%
              distinct(SiteCode, .keep_all=T) %>% select(SiteCode, taxa, stand.ibi), by='SiteCode') %>%
  filter(!is.na(stand.ibi)) %>%
  ggplot()+
  geom_sf(data=statesc) + geom_sf(data=canada)+
  geom_sf(aes(shape='aMAHI data', fill='aMAHI data'))+
  # geom_sf(data=ibi.mm[ibi.mm$taxa == 'fish',], 
  #         aes(shape='cIBI', fill='cIBI', geometry=geometry), alpha=0.5)+
  # scale_shape_manual('Locations', values=c(21,22), labels=function(x){substr(x, 2, nchar(x))})+
  # scale_fill_manual('Locations', values=c('black','white'), labels=function(x){substr(x, 2, nchar(x))})+
  scale_x_continuous(expand=c(0,0), limits=c(-96.52662,-77.04041))+
  scale_y_continuous(expand=c(0,0), limits=c(33.33267, 46.19911))+
  theme_bw()+ theme(legend.position='none', panel.grid.major = element_blank())+
  guides(colour = guide_legend(override.aes = list(alpha = c(1,1,.4))))+
  ggtitle('Fish IBI')+
  draw_plot(us_inset, x=-80.5, y=43.75, scale=6)

mc.aus<-mahi.scores.sf %>%
  filter(!(SiteCode %in% c('OHWAL','KYSFK','KYBFK'))) %>%
  ggplot()+
  geom_sf(data=statesc) + geom_sf(data=canada)+
  geom_sf(aes(shape='aMAHI data', fill='aMAHI data'))+
 #  geom_sf(data=au.sf, color='darkgrey')+
 #  scale_shape_manual('Locations', values=c(21))+
 # scale_fill_manual('Locations', values=c('black'))+
  scale_x_continuous(expand=c(0,0), limits=c(-96.52662,-77.04041))+
  scale_y_continuous(expand=c(0,0), limits=c(33.33267, 46.19911))+
  theme_bw()+ theme(legend.position='none', panel.grid.major = element_blank())+
  ggtitle('303(d) integrated reports')
get_legend_35 <- function(plot) {
  # return all legend candidates
  legends <- get_plot_component(plot, "guide-box", return_all = TRUE)
  # find non-zero legends
  nonzero <- vapply(legends, \(x) !inherits(x, "zeroGrob"), TRUE)
  idx <- which(nonzero)
  # return first non-zero legend if exists, and otherwise first element (which will be a zeroGrob) 
  if (length(idx) > 0) {
    return(legends[[idx[1]]])
  } else {
    return(legends[[1]])
  }
}
map.leg<-get_legend_35(big.map)
plot_grid(plot_grid(mc.fish+theme(plot.title = element_text(size=9), axis.text = element_text(size=8)),
                    mc.mac+theme(plot.title = element_text(size=9), axis.text = element_text(size=8)), 
                    mc.aus+theme(plot.title = element_text(size=9), axis.text = element_text(size=8)),
                    ncol=1,labels = 'AUTO'), 
          #map.leg,
          nrow=1, rel_widths = c(.74,.26), align='left')
ggsave('IFig1_map_mahi_only.pdf', width=3.5, height=7)

ibi.m[ibi.m$taxa == 'fish',] %>% distinct(geometry)
ibi.m[ibi.m$taxa == 'macroinvertebrates',] %>% distinct(geometry)

# Figure S1: example of distribution of sites
ibi.m %>% filter(dist.to.site.m==max(as.numeric(dist.to.site.m)))
gaarm<-findNLDI(location = sites[sites$SiteCode=='GAARM',], nav = c("UM", "UT", 'DM'), find = c("flowlines"), distance_km = 50)
site.buff<-sites %>% filter(SiteCode == 'GAARM') %>% st_buffer(50000)

ga.inset<-
  ggplot()+geom_sf(data=canada, fill='white', color='black')+
  geom_sf(data=statesc, fill='white', color='black')+
  geom_sf(data=site.buff, fill='lightgrey')+
  geom_sf(data=sites[sites$SiteCode=='GAARM',])+
  coord_sf(expand = 0)+ theme_bw()+
  theme(title = element_text(size=9))
ga.riv<-ibi.ranks %>% as_tibble() %>% 
  group_by(SiteCode, taxa) %>% filter(min(avgRnk)==avgRnk) %>% ungroup() %>%
  filter(SiteCode == 'GAARM') %>%
  ggplot()+
  geom_sf(data=gaarm$UT)+geom_sf(data=gaarm$DM)+
  geom_sf(aes(fill=taxa, geometry=geometry, shape=taxa), alpha=0.5, size=2)+
  geom_sf(data=sites[sites$SiteCode=='GAARM',], aes(fill='mussel', shape='mussel'), size=2)+
  scale_fill_manual('Taxa', values=c('goldenrod3', 'goldenrod3','black'))+
  scale_shape_manual('Taxa', values=c(24,25,21))+
  theme_bw()+
  theme(legend.position='bottom', legend.direction = 'vertical', 
        title = element_text(size=9),
        axis.text.x = element_text(angle=25, hjust=.85))+theme(legend.title=element_text(size=13),legend.text = element_text(size=12))+
  ggtitle('50 km upstream and downstream of\nArmuchee Creek in Georgia')
ga.map.leg<-get_legend(ga.riv)
plot_grid(ga.riv+theme(legend.position = 'none'),
          plot_grid(ga.inset+ggtitle('Location within entire study extent'), ga.map.leg, ncol=1), labels='AUTO')
ggsave('results/IFigS1_examp_map.jpg', width=6.75, height=5)

wvelk<-findNLDI(location = sites[sites$SiteCode=='WVELK',], nav = c("UM", "UT", 'DM'), find = c("flowlines"), distance_km = 50)
site.buff<-sites %>% filter(SiteCode == 'WVELK') %>% st_buffer(50000)
ibi.ranks %>% as_tibble() %>% 
  group_by(SiteCode, taxa) %>% filter(min(avgRnk)==avgRnk) %>% ungroup() %>%
  filter(SiteCode %in% c('WVELK')) %>% 
  ggplot()+
  geom_sf(data=site.buff, fill='lightgrey')+
  geom_sf(data=wvelk$UT)+geom_sf(data=wvelk$DM)+
  geom_sf(aes(color=taxa, geometry=geometry), alpha=0.5)+geom_sf(data=sites[sites$SiteCode=='WVELK',], aes(color='mussel'))+
  theme_bw()


# Figure S2: distribution of bioassessment scores ----
plot_grid(ibi.m %>%
            ggplot()+
            geom_histogram(aes(x=stand.ibi), binwidth = 1)+
            facet_wrap(~taxa)+
  scale_x_continuous('Standardized IBI score', breaks=seq(0,10,2))+
  scale_y_continuous('n Sites',limits=c(0,20), breaks=c(0,5,10,15,20))+theme_wendell,
  mahi.scores %>% 
    ggplot()+geom_histogram(aes(x=`MAHI composite`), binwidth=1)+
    ggtitle('mussels')+
    scale_x_continuous(breaks=seq(0,10,2))+
    scale_y_continuous(limits=c(0,20), breaks=c(0,5,10,15,20))+
    theme_wendell+
    theme(axis.title.y = element_blank(),
          axis.text.y=element_blank(),axis.ticks.y = element_blank(),
          plot.title = element_text(size=9, hjust=.5, vjust=0)),
  rel_widths = c(.6,.3), axis='top', align='h')
ggsave('IBI man data/IFigS2_score_histograms.jpg', width=6, height=2)
ibi.m %>% count(taxa)

# Figure X ------
plot_grid(ibi.m %>% filter(taxa =='fish') %>% 
            ggplot()+geom_point(aes(x=stand.ibi/100, y=`MAHI composite`), pch=1) +
            theme_classic()+
            scale_x_continuous('Fish IBI score', 
                               labels = scales::percent, limits=c(0,1))+
            scale_y_continuous('MAHI composite score', limits=c(0,10))+
            theme(panel.background=element_rect(fill=NA, color='black'),
                  strip.background=element_blank()),
          ibi.m %>% filter(taxa =='macroinvertebrates') %>% 
            ggplot()+geom_point(aes(x=stand.ibi/100, y=`MAHI composite`), pch=1) +
            annotate('text', x=.75, y=9.75, label=expression(italic(rho)^2*' = 0.16, '*italic(p)*' = 0.03'), size=3)+
            theme_classic()+
            scale_x_continuous('Macroinvertebrate IBI score', 
                               labels = scales::percent, limits=c(0,1))+
            scale_y_continuous('MAHI composite score', limits=c(0,10))+
            theme(panel.background=element_rect(fill=NA, color='black'),
                  strip.background=element_blank()),
ncol=1, labels='AUTO', axis = 'left', align='v')
ggsave('results/IFig2_corr_scores.jpg', width=3.5, height=5)
# check out sites with many estimates
lng.ibis<-ibis.df %>% 
  group_by(SiteCode, taxa) %>%
  filter(!is.na(stand.ibi)) %>%
  mutate(n=n()) %>% filter(n > 9)  
count(lng.ibis) 


library(Kendall)
lng.ibis %>% ggplot()+
  geom_point(aes(x=dt, y=stand.ibi, color=taxa))+
  facet_wrap(~SiteCode, scales='free')
lng.ibis %>%
  filter(SiteCode=='WVWFK', taxa == 'macroinvertebrates') %>% 
  arrange(dt) %>%
  pull(stand.ibi) %>% MannKendall(stand.ibi)
lng.ibis %>% group_by(SiteCode, taxa, dist.to.site) %>% count()


# make a map -----
library(maps); library(cowplot)
states<-st_as_sf(maps::map('state', fill=T, plot=F)) %>%
  filter(ID %in% tolower(unique(sites$State)))
sf_use_s2(F)
usa<-st_as_sf(maps::map('usa', fill=F, plot=F)) %>%
  st_buffer(0) %>%
  st_crop(st_bbox(states))
ibi.s.m<-ibi.m %>% ibis.df %>% group_by(SiteCode, taxa) %>% 
  filter(max(yr)==yr, min(dist.to.site.m)==dist.to.site) %>%
  distinct(SiteCode, taxa, dist.to.site, geometry) %>%
  group_by(SiteCode) %>% arrange(desc(taxa)) %>%
  mutate(pt_type=gsub(' mussels,', '',
              ifelse(dist.to.site<=50, 
                           paste('mussels,',unique(taxa), collapse=', '), 
                           paste(unique(taxa), collapse=', ')))) %>%
  right_join(sites %>% as_tibble() %>%select(SiteCode, Stream, State), by='SiteCode')
ibi.2.m<-ibis.df %>% group_by(SiteCode, taxa) %>% 
  filter(max(yr)==yr, min(dist.to.site)==dist.to.site) %>%
  distinct(SiteCode, taxa, dist.to.site, geometry)
ggplot()+
  geom_sf(data=usa, fill='grey90')+
  geom_sf(data=states, fill='grey80')+
  geom_sf(data=riv7)+
  geom_sf(data=sites[sites$SiteCode %in% mahi.scores$`Site code`,], 
          size=2,alpha=0.5, aes(shape='MAHI score'))+
  geom_sf(data=ibi.2.m, aes(shape='IBI score'))+
  scale_x_continuous(expand=c(0,0), limits=c(-95.62640,-77.04041))+
  scale_shape_manual('Bioassessment site',values = c(18,0))+
  scale_y_continuous(expand=c(0,0), limits=c(33.33267, 46.19911))+
  facet_wrap(~taxa)+
  theme_bw()+
  theme(legend.position='bottom', 
    legend.margin=margin(-9,0,-5,0),
    legend.text = element_text(size=8.5),
    legend.title = element_text(size=9, hjust=.5),
    strip.background = element_blank())
riv7<-read_sf('../MusselAgeGrowth/data/7thOrderR/nhdflowline_network.shp')
states<-st_as_sf(map('state', fill=T, plot=F))
sf_use_s2(F)
riv7c<-st_crop(riv7 %>% st_transform(st_crs(sites)) %>% st_buffer(0),
               st_bbox(sites %>% st_buffer(.5)))
statesc<-st_crop(states %>% st_transform(st_crs(sites)) %>% st_buffer(0),
                 st_bbox(sites %>% st_buffer(.5)))
canada<-st_as_sf(map('world','canada', fill=T, plot=F)) %>%
  st_transform(st_crs(sites)) %>% st_buffer(0) %>%
  st_crop(st_bbox(sites %>% st_buffer(.5)))
usa<-st_as_sf(map('world',fill=T, plot=F)) %>%
  st_crop(xmin=-178.2, xmax=-49.0, ymin=6.6, ymax=83.3) 
ins.bb<-st_bbox(sites %>% st_buffer(.5)) %>% st_as_sfc()
us_inset<-ggplot()+geom_sf(data=usa) + 
  geom_sf(data=ins.bb, fill=NA, color='red')+
  coord_sf(expand=FALSE)+
  theme_void()+
  theme(plot.background = element_rect(fill='white',color='black') )
us_inset
sites %>% 
  left_join(riv.summary, by='SiteCode') %>%
  filter(!is.na(meanT), SiteCode != 'ILEMB')%>%
  ggplot()+
  geom_sf(data=statesc) + geom_sf(data=canada)+
  geom_sf(data=riv7)+
  geom_sf(aes(color=meanT, geometry=geometry))+
  scale_color_gradientn('Mean stream\ntemperature', 
                        colors=met.brewer('Tam', type='continuous', direction=1), 
                        label=tmp.lab,
                        breaks=c(20, 23.5,  27), limits=c(20,27))+
  scale_x_continuous(expand=c(0,0), limits=c(-95.62640,-77.04041))+
  scale_y_continuous(expand=c(0,0), limits=c(33.33267, 46.19911))+
  theme_bw()+
  theme(legend.position='bottom', legend.margin=margin(t=-7),
        legend.title = element_text(size=11, hjust=1, vjust=1))+
  draw_plot(us_inset, x=-80.5, y=43.75, scale=6)
ggsave('results/TFig1_meantemp_map.jpg', width=3.5, height=3.7)


ibis_d %>% filter(!is.na(IBIScore), as.numeric(dist.to.site) < 100000) %>%
  mutate(dt=case_when(substr(SiteCode,1,2) %in% c('AL','MO') ~ mdy(Date), 
                      SiteCode %in% c("ILMFV","ILSFV") ~ mdy(Date),
                      T~ymd(Date))) %>%
  summarize(n=n(), nsites=n_distinct(SiteCode), mindt=min(dt), maxdt=max(dt))
ibi.df %>% group_by(taxa) %>% summarize(n=n(), nsite=n_distinct(SiteCode),
                                        mindt=min(dt), maxdt=max(dt), 
                                        mind=min(as.numeric(dist.to.site)),
                                        maxd=max(as.numeric(dist.to.site)))
mean(as.numeric(ibi.df$dist.to.site))
macro.ibi.mod<-aov(stand.ibi~mahiF, 
                   data=ibi.df[ibi.df$taxa == 'macroinvertebrates',])
summary(macro.ibi.mod)
#TukeyHSD(macro.ibi.mod)
summary(aov(stand.ibi~mahiF, 
            data=ibi.df[ibi.df$taxa == 'fish',]))


# figure out AU size ====
library(rATTAINS)
size.data<-NULL
for(i in unique(lat303$assessmentUnitIdentifier)){
  size.data<-bind_rows(size.data,
                       assessment_units(assessment_unit_identifer = i) %>%
                         select(organization_identifier:size_source_scale_text))
}
size.data %>% filter(!(assessment_unit_identifier %in% 
                         (lat303 %>% filter(latest %in% c('3','4C')) %>% 
                            pull(assessmentUnitIdentifier)))) %>%
  summarise(across('water_size_number', list(mean=mean, max=max, min=min)))*1.609344
unique(size.data$units_code); max(size.data$water_size_number)
size.data %>% filter(!(assessment_unit_identifier %in% 
                         (lat303 %>% filter(latest %in% c('3','4C')) %>% 
                            pull(assessmentUnitIdentifier)))) %>%
  distinct(assessment_unit_identifier, water_size_number, .keep_all=T) %>%
  ggplot()+geom_histogram(aes(x=water_size_number*1.609344), binwidth=5)+
  scale_y_continuous("n Assessment units", breaks=c(0,3,6,9,12), limits=c(0,12), expand=c(0,0))+
  scale_x_continuous('Unit length (kilometers)',breaks=seq(0,225, by=55), expand=c(0.01,0.01))+
  theme_wendell
ggsave('IBI man data/FigS3_assessment_length.jpg', width=3.5, height=2)

# investigating state differences
plot_grid(size.data %>% filter(!(assessment_unit_identifier %in% 
                     (lat303 %>% filter(latest %in% c('3','4C')) %>% 
                        pull(assessmentUnitIdentifier)))) %>%
  select(organization_name, assessment_unit_identifier, assessment_unit_name, state_code:size_source_scale_text) %>%
  ggplot()+
  geom_point(aes(x=state_code, y=water_size_number*1.609344), pch=1)+
  scale_y_continuous('Unit length (kilometers)',breaks=seq(0,225, by=55), expand=c(0.01,0.01))+
  scale_x_discrete('State (postal code abbreviation)')+
  theme_wendell,
  size.data %>% filter(!(assessment_unit_identifier %in% 
                           (lat303 %>% filter(latest %in% c('3','4C')) %>% 
                              pull(assessmentUnitIdentifier)))) %>%
    select(organization_name, assessment_unit_identifier, assessment_unit_name, state_code:size_estimation_method_code) %>%
    left_join(lat303, by=c('assessment_unit_identifier'='assessmentUnitIdentifier')) %>%
    ggplot()+
    geom_point(aes(x=water_size_number*1.609344, y=`MAHI composite`), pch=1)+
    scale_x_continuous('Unit length (kilometers)',breaks=seq(0,225, by=55), expand=c(0.01,0.01))+
    scale_y_continuous('MAHI composite score', limits = c(0,10), expand=c(0.01,0.01))+
    theme_wendell)
ggsave('FigS4_assessment_unit_size_concerns.jpg', width=6.5, height=3)
(size.data %>% filter(!(assessment_unit_identifier %in% 
                         (lat303 %>% filter(latest %in% c('3','4C')) %>% 
                            pull(assessmentUnitIdentifier)))) %>%
  filter(water_size_number *1.609344> 50 ) %>% nrow())  / 
  (size.data %>% filter(!(assessment_unit_identifier %in% 
                            (lat303 %>% filter(latest %in% c('3','4C')) %>% 
                               pull(assessmentUnitIdentifier)))) %>% nrow())
# investigating specific causes and MAHI ------
cause303.true %>%
  left_join(mahi.scores %>% filter(!is.na(`Site code`)), 
            by=c('SiteCode'='Site code'), relationship='many-to-many') %>%
  filter(!is.na(`MAHI composite`)) %>%
  ggplot()+geom_text(aes(y=grp, x=`MAHI composite`, label=SiteCode))

# compare water quality to mahi scores
pc_scores<-read_excel('C:/Users/Owner/Dropbox (Personal)/!Research/!MusselDeclines/finalized data/mussel_declines_water_chemistry.xlsx',
           sheet=5) %>% left_join(mahi.scores, by=c('SiteCode'='Site code'))
plot_grid(
  pc_scores %>% pivot_longer(c('PC1', 'PC2')) %>%
    ggplot(aes(x=value, y=`MAHI composite`))+
    geom_smooth(method='lm')+geom_point()+
    facet_wrap(~name,scales='free_x'), 
  pc_scores %>% pivot_longer(c('PC1', 'PC2')) %>%
    pivot_longer(starts_with("MAHI"), names_to = 'MAHI metric', values_to='MAHI score') %>%
  filter(`MAHI metric` != 'MAHI composite') %>%
    ggplot(aes(x=value, y=`MAHI score`))+
    geom_smooth(method='lm')+geom_point()+
    facet_grid(`MAHI metric`~name), ncol=1, rel_heights = c(.3, .6))



summary(lm(`MAHI composite`~PC1+PC2, data=pc_scores))
met.mods.pc<-NULL
for(var in c('MAHI density', 'MAHI pop growth', 'MAHI p_richness', 'MAHI recruit')){
  pc_sm<-pc_scores %>% rename(MahiMet=all_of(var)) %>%
    select(SiteCode, PC1, PC2, MahiMet) %>%
    filter(!is.na(MahiMet))
  mod<-summary(lm(MahiMet~PC1+PC2, data=pc_sm))
  met.mods.pc<-bind_rows(met.mods.pc,
                         bind_cols(
                      data.frame(variable=var, 
                                 df1=as.numeric(mod$fstatistic[2]), df2=as.numeric(mod$fstatistic[3]),
                                 n=nrow(pc_sm), adj.r.sq=mod$adj.r.squared,
                                 Fval=mod$fstatistic[1]),
                      bind_cols(mod$coefficients, rowname=rownames(mod$coefficients))))
}
met.mods.pc

met.mods.pc %>% distinct(variable, df1, df2, Fval, adj.r.sq) %>%
  mutate(pval=1-round(pf(Fval, df1, df2),3))

met.mods.pc %>% filter(`Pr(>|t|)` < 0.05, rowname != '(Intercept)')

# writing out the IBI data to save -----
library(writexl)
write_xlsx(list('Mean IBI scores by Site'=f.m.comp.df,
                'Closest and most recent scores'=ibi.m,
                'All scores within 50km'=read.csv('results/ibis_near_sites_240327.csv') %>%
                  filter(dist.to.site.m < 50000) %>%
                  left_join(sites %>% as_tibble() %>% dplyr::select(SiteCode, State), by='SiteCode') %>%
                  left_join(ibi.stand, by=c('State','taxa'))),  
           'results/mussel_declines_IBI_scores.xlsx')

# OLD CODE: partial correlation ======
# trying more complicated linear model
ibi.m<-ibi.m %>% mutate(MAHIc=`MAHI composite`) 
ibi.m.mac<-ibi.m %>% filter(taxa == 'macroinvertebrates') %>%
  mutate(dist.to.site.log10.km=log10(round(dist.to.site.m,0)/1000), 
         #across(c(MAHIc, stand.ibi, yr, dist.to.site.log10.km), ~scale(.x)[,1], .names = '{.col}')
         MAHI.p=MAHIc/10) %>%
  left_join(sites %>% as_tibble() %>% 
              dplyr::select(SiteCode, YearVisited), by='SiteCode') %>%
  mutate(yr.dif=YearVisited-yr)
ibi.m.fish<-ibi.m %>% filter(taxa == 'fish')%>%
  mutate(dist.to.site.log10.km=log10(round(dist.to.site.m,0)/1000), 
         #across(c(MAHIc, stand.ibi, yr, dist.to.site.log10.km), ~scale(.x)[,1], .names = '{.col}')
         MAHI.p=MAHIc/10)%>% 
  left_join(sites %>% as_tibble() %>% dplyr::select(SiteCode, YearVisited), by='SiteCode') %>%
  mutate(yr.dif=YearVisited-yr) %>%
  filter(!is.na(stand.ibi)) # missing for NCCAT for some reason?
# check for colinearity
names(ibi.m.mac[,c(3,7,24,21)])
imp.cols<-c(3,7,24,21)
pairs(ibi.m.mac[,imp.cols])
cor(ibi.m.mac[,imp.cols], method='pearson')
cor.test(ibi.m.mac$stand.ibi, ibi.m.mac$`MAHI composite`, method='pearson')
pairs(ibi.m.fish[,imp.cols])
cor(ibi.m.fish[!is.na(ibi.m.fish$stand.ibi),imp.cols], method='pearson')
ggplot(data=ibi.m.fish, aes(x=dist.to.site.log10.km, y=stand.ibi)) +
  geom_point()

# need to plot residuals
ibi.fish.rnk<-ibi.m.fish %>% 
  mutate(taxa.rank=rank(stand.ibi), mussel.rank=rank(`MAHI composite`),
         dist.rank=rank(dist.to.site.log10.km), yr.rank=rank(yr.dif)) %>%
  filter(!is.na(stand.ibi))
fish_Y <- lm(mussel.rank ~ dist.rank+yr.rank, data=ibi.fish.rnk)
fish_X <- lm(taxa.rank ~ dist.rank+yr.rank, data=ibi.fish.rnk)
cor.test(residuals(fish_X), residuals(fish_Y))
ibi.mac.rnk<-ibi.m.mac %>% 
  mutate(taxa.rank=rank(stand.ibi), mussel.rank=rank(`MAHI composite`),
         dist.rank=rank(dist.to.site.log10.km), yr.rank=rank(yr.dif))
mac_Y <- lm(mussel.rank ~ dist.rank+yr.rank, data=ibi.mac.rnk)
mac_X <- lm(taxa.rank ~ dist.rank+yr.rank, data=ibi.mac.rnk)
cor.test(residuals(mac_X), residuals(mac_Y))
plot_grid(
  bind_cols(MAHI_resid=residuals(fish_Y), ibi_resid=residuals(fish_X)) %>% 
    ggplot()+geom_point(aes(x=ibi_resid, y=MAHI_resid), pch=1)+
    annotate('text', x=20*.33, y=20*.85, hjust=0,
             label=expression("partial "*rho*" = 0.50"), size=3.5)+
    scale_x_continuous('Fish IBI score residuals ', limits = c(-20,20))+
    scale_y_continuous('MAHI score residuals', limits = c(-20,20))+
    theme_wendell,
  bind_cols(MAHI_resid=residuals(mac_Y), ibi_resid=residuals(mac_X)) %>% 
    ggplot()+geom_point(aes(x=ibi_resid, y=MAHI_resid), pch=1)+
    annotate('text', x=30*.33, y=30*.85, hjust=0,
             label=expression("partial "*rho*" = -0.30"), size=3.5)+
    scale_x_continuous('Macroinvertebrate IBI score residuals', limits = c(-30,30))+
    scale_y_continuous('MAHI score residuals', limits = c(-30,30))+
    theme_wendell,
  ncol=1, align='v',axis='left', labels='AUTO')
ggsave('results/IFig2_partial_cor.jpg', width=3.5, height=5, dpi=600)


