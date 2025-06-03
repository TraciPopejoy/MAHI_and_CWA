# build site location huc table -----
library(tidyverse); library(nhdplusTools); library(sf)
sites<-read.csv('../DeclinesMussels/data/site_loc.csv') %>% 
  bind_rows(read.csv('IBI_data/extraMNsites.csv') %>% mutate(State = 'Minnesota')) %>% 
  filter(!is.na('Latitude'), !is.na(Longitude), !is.na(Stream)) %>%
  st_as_sf(coords=c('Longitude','Latitude'), crs = 4269)
# fixing ones that won't come in correctly
sites[sites$HUC12 == '1.1e+11',4]<-c(110100060506, 110100110201, 110100080704) # in order, just incase they convert to 1.1e11

# for loop that uses get_huc8 to pull the huc8 for each site
site.huc<-NULL
for(i in which(!(sites$Stream %in% site.huc$site))){
  print(sites[i,]$Stream)
  site.huc<-bind_rows(site.huc,
                   data.frame(site=sites[i,]$Stream,
                              huc8=get_huc8(sites[i,]))) 
}

# area of the convex hull that captures all our sites in km2
st_area(st_convex_hull(st_union(sites)))/1000000

View(site.huc[,c(1,3)])

# HUC 12 303d list query --------------
# Waters within ATTAINS are listed by the state as impaired Under CWA section 303(d)
# using rATTAINS, I can get a lot of data associated with the CWA listing process that is inclued in the integrated report

# install the package
#remotes::install_github("mps9506/rATTAINS", ref = "dev-flatten")
library(tidyverse); library(rATTAINS)
site_loc<-sites %>%
  left_join(data.frame(State=state.name,
                       StAbb=state.abb), by='State')
site_loc$HUC12
site_loc[site_loc$HUC12 == 1.1e+11 & !is.na(site_loc$HUC12),]$HUC12<-c(110100060506, 110100110201, 110100080704) # in order, just incase they convert to 1.1e11

# for loop to query huc 12 associated summary information (% impaired, etc)
attain_huc12_sum<-NULL
attain_huc12_ir<-NULL
for(i in site_loc$HUC12){
  # search for a huc12 summary, which has a list as a result
  athc<-huc12_summary(huc = #ifelse(i == '1.1e+11', i,
                                   ifelse(nchar(i)==11, paste0('0',i),as.character(i)))
  # if there isn't an assessment unit summary, record just the summary
  if(nrow(athc$au_summary)==0){
    hcsum<-athc$huc_summary
  }else{ # but when an assessment unit summary is available keep it
    hcsum<-athc$huc_summary %>% mutate(au_ids=paste(athc$au_summary$assessment_unit_id, collapse=','))}
  # keep the huc12 summary
  attain_huc12_sum<-bind_rows(attain_huc12_sum, hcsum)
  # keep the epa ir summary for each huc12
  if(nrow(athc$ir_summary) != 0){
  attain_huc12_ir<-bind_rows(attain_huc12_ir,
                             athc$ir_summary %>% mutate(huc12=i))}
}
write.csv(attain_huc12_sum, 'HUC12_303d_Summary.csv', row.names=F)
write.csv(attain_huc12_ir, 'HUC12_303d_EPA_IR_cat.csv', row.names=F)

# make a graph of huc12 summaries
attain.sf<-attain_huc12_sum %>% 
  left_join(site_loc %>% 
              mutate(huc12=ifelse(nchar(HUC12)==11, paste0("0",HUC12), HUC12)),
            by='huc12') %>% 
  st_as_sf(coords=c('Longitude','Latitude'), crs = 4269) 
library(maps)
usa<-st_as_sf(map('state', fill=T, plot=F)) %>% st_make_valid() %>%
  st_transform(st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")) %>%
  filter(ID %in% c('alabama','georgia','kentucky',
                   'tennessee','north carolina',
                   'virginia','indiana','ohio',
                   'west virginia','pennsylvania', 
                   'minnesota','missouri', 'illinois')) %>%
  st_transform(st_crs(attain.sf))
attain.sf %>%
  ggplot()+
  geom_sf(data=usa)+
  geom_sf_text(aes(label=SiteCode), size=2)+
  geom_sf(aes(fill=contain_impaired_waters_catchment_area_percent/100),
          pch=21, size=5, alpha=.5)+
  scale_fill_gradient2('% HUC 12 impaired', 
                        high="#b93961", low="#4060c8", midpoint=.50, 
                        labels=scales::percent)+
  coord_sf(xlim=st_bbox(attain.sf)[c(1,3)], ylim=st_bbox(attain.sf)[c(2,4)])+
  theme_void()+
  theme(legend.position = 'bottom')

# Assessment Unit query ======
# I manually identify specific assessment unit names we are interested in 
#    https://mywaterway.epa.gov/
# I want to double check the assessment unit is in the huc identified above
attain_huc12_sum %>% select(huc12, au_ids) %>%
  mutate(HUC12=as.numeric(substr(huc12,2,12))) %>%
  left_join(site_loc %>% select(ATTAINSau, Stream, SiteCode,HUC12),
            by='HUC12')%>%
  rowwise() %>% 
  mutate(dblchk=grepl(ATTAINSau, au_ids)) %>% ungroup() %>%
  filter(dblchk==F) %>%
  select(huc12, au_ids, ATTAINSau, SiteCode) %>% View()
# KYSFK is at the edge of the HUC
# doesn't work for NC aus, so double checked manually 
# needed to correct PASND and OHBDB
# WV huc12 summary doesn't return any assessment unit ids
huc12_summary(huc = "050500070906")

# this is a big for loop that goes through 
# each state, each assessment unit and then each year
# it pulls the delisted waters into a spread sheet (dlwat)
# and the use assessment/ integrated report summary into a single spreadsheet too

# rerun 04/12/2024
# last rerun 01/09/2025
dlwat<-NULL; use.ass<-NULL; 
for(st in unique(site_loc$StAbb)){
  # identify what the state is called within the ATTAINS database
  st.dm<-domain_values(domain_name = "OrgStateCode") %>%
    filter(name == st, context != 'EPA') %>% pull(context) %>% unique()
  # two state domains can complicate the for loop; this clarifys it
  if(st == 'MN'){st.dm<-'MNPCA'}
  if(st == 'NC'){st.dm<-'21NC01WQ'}
for(aus in site_loc[site_loc$StAbb==st,]$ATTAINSau){ # for each ass unit
for(yr in as.character(seq(2008,2022,2))){ # and each year since 2008
  # get the assessment
 assess.yr <- assessments(assessment_unit_id=aus, 
            organization_id=st.dm, reporting_cycle=yr)
 dlwat<-bind_rows(dlwat, assess.yr$delisted_waters) # save the delisted waters information
 use.ass<-bind_rows(use.ass, assess.yr$use_assessment) # and use assessments
 print(paste('done with', aus, yr))
}
}
}
saveRDS(dlwat, '303d/303d_delisted_waters_250109.rds')
saveRDS(use.ass, '303d/303d_Use_Assessments_250109.rds')

# analyze 303d data =====
# missing EPA IR & assessments for ILKIS or IL_PQ-02
library(MetBrewer); library(tidyverse)
dlwat<-readRDS('303d/303d_delisted_waters_250109.rds')
use.ass<- readRDS('303d/303d_Use_Assessments_250109.rds')
use.ass.au<-filter(use.ass, !is.na(assessmentUnitIdentifier))
sites %>% select(SiteCode, State, Stream, ATTAINSau) %>%
  filter(!(ATTAINSau %in% unique(use.ass.au$assessmentUnitIdentifier)))

# this for loop breaks up the data hidden in the use.ass file to pull out 
# parameters (reasons for listing), sources (sources of reason for listing) and 
# uses each unit is supposed to fulfil
param<-NULL; srcs<-NULL; use.attain<- NULL
for(i in 1:nrow(use.ass.au)){
  # pull out the parameters table
  Pcause.info<-bind_rows(use.ass.au[i,10][[1]])$associatedUses
  if(nrow(bind_rows(Pcause.info)) >= 1){
  pid<-bind_rows(use.ass.au[i,10][[1]])[,1:2] %>%
    mutate(id=as.character(1:n()))
  param <- bind_rows(param,
  bind_rows(Pcause.info, .id='id') %>% left_join(pid, by='id') %>% 
    select(-id, -seasons) %>%
    bind_cols(use.ass.au[i,1:7]))
  }
  # pull out probable sources table
    srrc <- use.ass.au[i,11][[1]][[1]]
    if(nrow(srrc)>0){
    srcs <-bind_rows(srcs,
                     bind_cols(use.ass.au[i,c(1,4,6)],
                               srrc[,-3],
                               cause=apply(srrc[,3], 1,
                                           function(x){paste(unlist(x), collapse='; ')})))}
  use.attain<-bind_rows(use.attain,
                        bind_rows(use.ass.au[i,9][[1]])[,1:3] %>%
                          bind_cols(use.ass.au[i,1:7]))
}
View(param); View(srcs); View(use.ass)

# report EPA IR categories -----
epair<-use.ass[,-c(8:12)] %>%
  select(assessmentUnitIdentifier, reportingCycleText, 
         overallStatus, epaIRCategory) %>%
  left_join(sites, by=c('assessmentUnitIdentifier'='ATTAINSau')) %>%
  filter(!is.na(SiteCode)) %>%
  mutate(IRf=factor(epaIRCategory, 
                    levels=c(5,'4C','4B','4A',3,2,1),
                    labels=c('impaired &\nneeds a TMDL',
                             'impaired by a\nnon-pollutant',
                             'impaired by a\npollutant, without TMDL', 
                             'impaired by pollutant\nwith TMDL',
                             'insufficient information',
                             'not all uses assessed\nbut meeting standards',
                             'supporting standards')),
         yy=as.numeric(reportingCycleText))
# investigating NAs in IRf
epair %>% filter(is.na(IRf)) %>% pull(assessmentUnitIdentifier) %>% unique()
use.ass %>%
  filter(assessmentUnitIdentifier %in% 
           (epair %>% filter(is.na(IRf)) %>% 
              pull(assessmentUnitIdentifier) %>% unique())) %>%
  arrange(assessmentUnitIdentifier, reportingCycleText) %>% 
  select(assessmentUnitIdentifier, reportingCycleText, reportStatusCode, 
         epaIRCategory, overallStatus, cycleLastAssessedText) %>%View()
# IL, IN, and OH assessments unknown when missing, should keep as NA
# MN missing 2010, but has 2008, should replace 2010 with 2008 assessment (assuming no change)
# VA has 2008 but missing 2010,2012, and 2014, should replace missing with 2008 assessment (assuming no change)
epair %>%
  select(SiteCode, assessmentUnitIdentifier, reportingCycleText, overallStatus, epaIRCategory) %>%
  arrange(assessmentUnitIdentifier, reportingCycleText) %>%
  group_by(assessmentUnitIdentifier) %>%
  mutate(adj.epaIR=case_when(substr(assessmentUnitIdentifier, 1,2) =='MN' &
                               is.na(epaIRCategory) ~ lag(epaIRCategory),
                             substr(assessmentUnitIdentifier, 1,2) =='VA' & 
                               is.na(epaIRCategory) ~ epaIRCategory[which.min(reportingCycleText)],
                             T~epaIRCategory)) %>%
  #filter(is.na(adj.epaIR), as.numeric(reportingCycleText)>2018)
  #filter(assessmentUnitIdentifier == 'IL_BE-09')
  select(-overallStatus, -epaIRCategory) %>%
  pivot_wider(names_from=reportingCycleText, values_from = adj.epaIR) %>%
  select(SiteCode, assessmentUnitIdentifier, `2008`,`2010`,`2012`,`2014`,`2016`,`2018`,`2020`,`2022`) %>%
  write.csv('results/site_EPA_impairment_codes_250109.csv', row.names = F)
  
epair %>%
  filter(reportingCycleText %in% seq(2012, 2022,2),
         !(SiteCode %in% c('PASND','NCCHE','NCCTG', 'KYSLT'))) %>%
  mutate(MAHIfac=factor(MAHIcat, levels=c('degraded','unknown','healthy'))) %>%
  ggplot()+
  geom_tile(aes(y=reportingCycleText, x=SiteCode, 
                fill=IRf, color=IRf))+
  scale_color_manual('EPA IR\ncategory', values=met.brewer('Homer1'), 
                     aesthetics=c('color','fill'))+
  scale_y_discrete('Reporting Cycle')+
  scale_x_discrete('Site')+
  facet_wrap(MAHIfac~., scales='free', ncol=1)+
  theme_classic()+
  theme(panel.background=element_rect(fill=NA, color='black'),
        legend.position='right', legend.margin = margin(l=-7),
        strip.background = element_blank(),
        axis.text.x = element_text(size=7, angle=90, hjust=1, vjust=0.3))
#ggsave('results/303dEPAstatus.jpg', width=6.5, height=4.5)
(mis.irs<-epair %>% filter(reportingCycleText %in% seq(2012, 2022,2),
                 SiteCode != 'PASND', is.na(IRf)) %>%
  pull(assessmentUnitIdentifier) %>% unique())
use.ass %>% filter(assessmentUnitIdentifier %in% mis.irs,
                   is.na(epaIRCategory)) %>% View()

# Impairment cause & source table ------
# this code builds a table that identifies the specific causes and sources of that cause of impairment
srcs.raw<-bind_cols(srcs, str_split_fixed(srcs$cause, '; ', 3)) %>% # split character string into causes
  pivot_longer(starts_with("...")) %>%
  select(-organizationIdentifier) %>%
  filter(value != '') 

# srcs misses causes when no probable source is assigned by the state
# gathering those missing assessment units here
invest.au<-epair %>% select(assessmentUnitIdentifier, reportingCycleText, epaIRCategory) %>%
  filter(!(epaIRCategory %in% c(1,2)),# only for 3, 4, and 5 epa IR category
         reportingCycleText > 2020) %>%  # and more recent ones to avoid historical data issues
  left_join(srcs.raw %>% select(assessmentUnitIdentifier, reportingCycleText, sourceName),
            by=c('assessmentUnitIdentifier', 'reportingCycleText')) %>%
  filter(is.na(sourceName)) %>% pull(assessmentUnitIdentifier)
srcs.raw %>% filter(assessmentUnitIdentifier %in% invest.au)
invest.au # code above misses these AUs
# example of available cause but missing source
use.ass.au[use.ass.au$assessmentUnitIdentifier==invest.au[1] & 
             use.ass.au$reportingCycleText == '2022',10][[1]]
use.ass.au[use.ass.au$assessmentUnitIdentifier==invest.au[1] & use.ass.au$reportingCycleText == '2022',11]

all.srcs <- param %>% filter(assessmentUnitIdentifier %in% invest.au) %>% # first find the missing causes
  group_by(assessmentUnitIdentifier) %>% filter(reportingCycleText == max(reportingCycleText))%>%
  filter(parameterStatusName == 'Cause') %>% #pull(parameterAttainmentCode) %>% unique()
  select(assessmentUnitIdentifier, reportingCycleText, parameterName) %>% 
  # making sure columns will align with srcs.raw
  rename(value = parameterName) %>% mutate(sourceName='source unknown') %>%
  bind_rows(srcs.raw) %>% # bringing in the other causes with sources
  # join specific causes or sources together for ease of interpretation
  mutate(Cause = case_when(value == 'ESCHERICHIA COLI (E. COLI)' ~ 'fecal coliform',
                           value == 'MERCURY IN FISH TISSUE' ~ 'mercury',
                           value %in% c('TOXAPHENE','MIREX', 'HEPTACHLOR', 'ENDRIN','DIELDRIN','ATRAZINE','ALDRIN') ~ 'pesticides or herbicides',
                           value %in% c('BENTHIC MACROINVERTEBRATES BIOASSESSMENTS',
                                        'FISH KILL(S)', 'FISH BIOASSESSMENTS',
                                        'NON-NATIVE FISH/SHELLFISH/ZOOPLANKTON') ~ 'biotic',
                           grepl('SILTATION', value) ~ 'siltation',
                           value %in% c('PHOSPHORUS, TOTAL','NITROGEN, NITRATE', 'NITRATE','NITRATES')|
                             grepl('NUTRIENT/EUT', value) ~ 'nutrients',
                           value == 'PCBS IN FISH TISSUE' ~ 'polychlorinated biphenyls (pcbs)',
                           value == 'TEMPERATURE, WATER' ~ 'temperature',
                           value %in% c('TOTAL DISSOLVED SOLIDS (TDS)','OSMOTIC PRESSURE') ~ 'total dissolved solids',
                           value %in% c('HABITAT ALTERATIONS','FLOW REGIME MODIFICATION', 'HABITAT ASSESSMENT') ~ 'physical changes',
                           T ~ tolower(value)),
         Source = case_when(sourceName %in% c('MANAGED PASTURE GRAZING','LIVESTOCK (GRAZING OR FEEDING OPERATIONS)') ~
                               'livestock (general)',
                             sourceName %in% c('GRAZING IN RIPARIAN OR SHORELINE ZONES','UNRESTRICTED CATTLE ACCESS') ~
                               'unrestricted livestock access',
                             grepl('NATURAL CONDI', sourceName) ~ 'natural sources',
                             grepl('WATERFOWL', sourceName) ~ 'natural sources',
                             grepl('RURAL', sourceName) ~ 'human sewage', # but the ecoli tmdl discusses pasture more than failing septic tanks
                               grepl('SEW', sourceName) | grepl('MUNICIPAL', sourceName) | grepl('SEPTIC SYSTEMS', sourceName) ~ 'human sewage',
                             grepl('MIN', sourceName) ~ 'mining',
                             grepl('ATMOSPHERIC', sourceName) ~'natural sources',
                             sourceName %in% c('CROP PRODUCTION (CROP LAND OR DRY LAND)',
                                               'NON-IRRIGATED CROP PRODUCTION', 'CROP PRODUCTION (NON-IRRIGATED)',
                                             'SOURCES OUTSIDE STATE JURISDICTION OR BORDERS') ~ 'crop agriculture',
                             sourceName %in% c('INDUSTRIAL/COMMERCIAL SITE STORMWATER DISCHARGE (PERMITTED)',
                                               'PACKAGE PLANT OR OTHER PERMITTED SMALL FLOWS DISCHARGES',
                                               "MILL TAILINGS") ~ 'industry',
                             sourceName %in% c('IMPACTS FROM LAND APPLICATION OF WASTES', 
                                               'ILLEGAL DUMPS OR OTHER INAPPROPRIATE WASTE DISPOSAL',
                                               'ANIMAL FEEDING OPERATIONS (NPS)', 'DAIRIES','MANURE RUNOFF') ~ 'animal feeding operations', 
                             T ~ tolower(sourceName)),
         # join source - causes together to get a better grouping of reason for impairments
         pair=paste(Source, Cause, sep='-'),
         # join similair groups to reduce number of groups
         grp=case_when(pair %in% c("agriculture-turbidity", "site clearance (land development or redevelopment)-turbidity",
                                   "agriculture-total suspended solids (tss)", "agriculture-siltation", 
                                   "unrestricted livestock access-siltation", "source unknown-siltation",
                                   "crop agriculture-physical substrate habitat alterations",
                                   'crop agriculture-siltation', 'source unknown-turbidity',
                                   'crop agriculture-total suspended solids (tss)',
                                   'unrestricted livestock access-alteration in stream-side or littoral vegetative covers',
                                   "unrestricted livestock access-physical substrate habitat alterations")~ 'muddy water',
                       pair %in% c("livestock (general)-fecal coliform", "animal feeding operations-fecal coliform", 
                                   "unrestricted livestock access-fecal coliform") ~
                         'fecal coliform from livestock',
                       pair %in% c("non-point source-polychlorinated biphenyls (pcbs)", 
                                   "industry-polychlorinated biphenyls (pcbs)", 
                                   "source unknown-polychlorinated biphenyls (pcbs)") ~  'PCBs',
                       pair %in% c("non-point source-fecal coliform", "source unknown-fecal coliform",
                                   'natural sources-fecal coliform', 'industry-other cause',
                                   'industry-fecal coliform') ~ # industry from Armuchee fecal coliform TMDL in 2004 (https://epd.georgia.gov/coosa-river-basin-tmdl-reports)
                         'high fecal coliform',
                       grepl('mercury', pair) ~ 'mercury',
                       pair %in% c("source unknown-cause unknown", "source unknown-biotic", 
                                   'source unknown-temperature', 'human sewage-biotic',
                                   'source unknown-physical changes','non-point source-other cause') ~ # non point & other cause from Armuchee, can't find more info
                         'unknown cause',
                       pair %in% c('industry-lead', 'mining-metals', 'source unknown-iron','source unknown-lead',
                                   'mining-ph', 
                                   "mining-total dissolved solids", "mining-biotic", 
                                   "mining-total suspended solids (tss)") ~ 'mining-associated solutes',
                       pair %in% c( 'human sewage-biotic', 'human sewage-nutrients', 'human sewage-dissolved oxygen') ~ 'human sewage',
                       pair %in% c("source unknown-nutrients", "non-point source-nutrients", 
                                   "agriculture-nutrients", 'agriculture-fecal coliform', 
                                   'crop agriculture-nutrients') ~ 'nutrient enrichment',
                       T ~ pair)) %>%
  #cleaning up the columns
  select(-cause,-name, -sourceConfirmedIndicator)
all.srcs 
table(all.srcs$pair)
table(all.srcs$grp)
# make sure I have a cause for all assessment units
epair %>% filter(!(epaIRCategory %in% c(1,2)), reportingCycleText>2020) %>%
  filter(!(assessmentUnitIdentifier %in% unique(all.srcs$assessmentUnitIdentifier)))
# only not assessed streams are missing causes

# so all.srcs has a row for each site, year, and cause-source indicated on the integrated report
all.srcs %>% left_join(sites %>% as_tibble() %>% select(SiteCode, ATTAINSau),
                       by=c('assessmentUnitIdentifier'='ATTAINSau')) %>%
  group_by(grp, pair) %>% 
  summarize(nyrs=n_distinct(reportingCycleText),
            nsites=n_distinct(assessmentUnitIdentifier),
            sites_c=paste(unique(SiteCode), collapse='; '), .groups='drop') %>%
  write.csv('results/303d_source_groups_tally.csv', row.names = F)

# making a plot of source-pairs
all.srcs %>% filter(reportingCycleText > 2011) %>%
  left_join(sites, by=c('assessmentUnitIdentifier'='ATTAINSau')) %>%
  filter(!(SiteCode %in% c('PASND','NCCHE','NCCTG')),
         !is.na(grp)) %>%
  ggplot()+geom_tile(aes(y=SiteCode, x=grp))+
  facet_wrap(~reportingCycleText, scales='free_y')+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))

all.srcs %>% 
  filter(reportingCycleText > 2011) %>%
  distinct(assessmentUnitIdentifier, reportingCycleText, grp, pair) %>%
  mutate(uid=paste(assessmentUnitIdentifier, reportingCycleText)) %>%
  left_join(sites  %>% as_tibble() %>% select(SiteCode, State, ATTAINSau),
            by=c('assessmentUnitIdentifier'='ATTAINSau')) %>%
  arrange(State, SiteCode, reportingCycleText) %>%
  left_join(epair %>% select(assessmentUnitIdentifier, reportingCycleText, epaIRCategory),
            by=c('assessmentUnitIdentifier','reportingCycleText')) %>%
  group_by(assessmentUnitIdentifier, SiteCode) %>%
  filter(reportingCycleText == max(as.numeric(reportingCycleText))) %>%
  #filter(SiteCode == 'MOMER')
  write.csv('results/303d_source_cause_groups_latest_250109.csv', row.names = F)