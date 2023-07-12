##Remove anything stored in R##
rm(list=ls())
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
## Load libraries and source pre-defined functions
# -----------------------------------------------------------------------------
library( ggplot2 )
library( reshape2 )
library( dplyr )
library( scales )
library( extrafont )
library( ggthemes )
library( checkpoint )
library( tidyr )
library( gridExtra )
#library( plyr )
library( RColorBrewer )
library( stringr )
library( devtools )
library( rgcam )
library( gcammaptools )
library( rgdal )
library( cowplot )
library( directlabels)
library( ggpmisc)
library( data.table )
library( raster )
library( scales )
library( tidyverse )
library( ggpubr )
library( gganimate )
library( magick )

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
getwd()
setwd( "/Users/grah436/Desktop/PNNL_Projects/vwt_drivers" )

get_vwt_data_gfdl <- function(db,scenario){
conn <- localDBConn('/Users/grah436/Documents/GitHub/gcam-54/output', paste0('drivers_',db))
prj <- addScenario(conn, 'vwt_drivers_gfdl.dat', scenario,
                   '/Users/grah436/Documents/GitHub/gcam-54/output/queries/vwt_queries.xml')

}
get_vwt_data_ipsl <- function(db,scenario){
  conn <- localDBConn('/Users/grah436/Documents/GitHub/gcam-54/output', paste0('drivers_',db))
  prj <- addScenario(conn, 'vwt_drivers_ipsl.dat', scenario,
                     '/Users/grah436/Documents/GitHub/gcam-54/output/queries/vwt_queries.xml')

}
get_vwt_data_miroc <- function(db,scenario){
  conn <- localDBConn('/Users/grah436/Documents/GitHub/gcam-54/output', paste0('drivers_',db))
  prj <- addScenario(conn, 'vwt_drivers_miroc.dat', scenario,
                     '/Users/grah436/Documents/GitHub/gcam-54/output/queries/vwt_queries.xml')

}
get_vwt_data_hadgem <- function(db,scenario){
  conn <- localDBConn('/Users/grah436/Documents/GitHub/gcam-54/output', paste0('drivers_',db))
  prj <- addScenario(conn, 'vwt_drivers_hadgem.dat', scenario,
                     '/Users/grah436/Documents/GitHub/gcam-54/output/queries/vwt_queries.xml')

}
get_vwt_data_noresm <- function(db,scenario){
  conn <- localDBConn('/Users/grah436/Documents/GitHub/gcam-54/output', paste0('drivers_',db))
  prj <- addScenario(conn, 'vwt_drivers_noresm.dat', scenario,
                     '/Users/grah436/Documents/GitHub/gcam-54/output/queries/vwt_queries.xml')

}

get_vwt_data_gfdl("highmi_gfdl", "High_MI")
get_vwt_data_gfdl("highmi_gfdl_2p6", "HighMI_2p6")
get_vwt_data_gfdl("ref_gfdl", "Reference")
get_vwt_data_gfdl("ref_gfdl_2p6", "Reference_2p6")
get_vwt_data_gfdl("ss_gfdl", "SS")
get_vwt_data_gfdl("ss_gfdl_2p6", "SS_2p6")
get_vwt_data_ipsl("highmi_ipsl", "High_MI")
get_vwt_data_ipsl("highmi_ipsl_2p6", "HighMI_2p6")
get_vwt_data_ipsl("ref_ipsl", "Reference")
get_vwt_data_ipsl("ref_ipsl_2p6", "Reference_2p6")
get_vwt_data_ipsl("ss_ipsl", "SS")
get_vwt_data_ipsl("ss_ipsl_2p6", "SS_2p6")
get_vwt_data_miroc("highmi_miroc", "High_MI")
get_vwt_data_miroc("highmi_miroc_2p6", "HighMI_2p6")
get_vwt_data_miroc("ref_miroc", "Reference")
get_vwt_data_miroc("ref_miroc_2p6", "Reference_2p6")
get_vwt_data_miroc("ss_miroc", "SS")
get_vwt_data_miroc("ss_miroc_2p6", "SS_2p6")
get_vwt_data_hadgem("highmi_hadgem", "High_MI")
get_vwt_data_hadgem("highmi_hadgem_2p6", "HighMI_2p6")
get_vwt_data_hadgem("ref_hadgem", "Reference")
get_vwt_data_hadgem("ref_hadgem_2p6", "Reference_2p6")
get_vwt_data_hadgem("ss_hadgem", "SS")
get_vwt_data_hadgem("ss_hadgem_2p6", "SS_2p6")
get_vwt_data_noresm("highmi_noresm", "High_MI")
get_vwt_data_noresm("highmi_noresm_2p6", "HighMI_2p6")
get_vwt_data_noresm("ref_noresm", "Reference")
get_vwt_data_noresm("ref_noresm_2p6", "Reference_2p6")
get_vwt_data_noresm("ss_noresm", "SS")
get_vwt_data_noresm("ss_noresm_2p6", "SS_2p6")


prj_gfdl <- loadProject('vwt_drivers_gfdl.dat')
prj_ipsl <- loadProject('vwt_drivers_ipsl.dat')
prj_miroc <- loadProject('vwt_drivers_miroc.dat')
prj_hadgem <- loadProject('vwt_drivers_hadgem.dat')
prj_noresm <- loadProject('vwt_drivers_noresm.dat')

##### In order to obtain water withdrawals by crop type and basin to match the agricultural productivity,
##### we must query "water withdrawals by mapping source" which specifies crop type and location but does
##### not include conveyance losses in the withdrawal calculation. Therefore we read in conveyances losses
##### which are independent of SSP and divide withdrawals by conveyance losses to obtain total irr withdrawal
detach(package:plyr)

green.water <- rbind(getQuery(prj_gfdl,'biophysical water demand by ag tech') %>% mutate(gcm="gfdl"),
                     getQuery(prj_ipsl,'biophysical water demand by ag tech')%>% mutate(gcm="ipsl"),
                     getQuery(prj_miroc,'biophysical water demand by ag tech')%>% mutate(gcm="miroc"),
                     getQuery(prj_hadgem,'biophysical water demand by ag tech')%>% mutate(gcm="hadgem"),
                     getQuery(prj_noresm,'biophysical water demand by ag tech')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(biophys=value) %>% dplyr::select(-input)



consumption <- rbind(getQuery(prj_gfdl,'irrigation water consumption by ag tech') %>% mutate(gcm="gfdl"),
                     getQuery(prj_ipsl,'irrigation water consumption by ag tech')%>% mutate(gcm="ipsl"),
                     getQuery(prj_miroc,'irrigation water consumption by ag tech')%>% mutate(gcm="miroc"),
                     getQuery(prj_hadgem,'irrigation water consumption by ag tech')%>% mutate(gcm="hadgem"),
                     getQuery(prj_noresm,'irrigation water consumption by ag tech')%>% mutate(gcm="noresm"))  %>%
  dplyr::rename(consumption=value) %>% dplyr::select(-input)

green.water <- full_join(green.water,consumption,by=c("scenario","gcm","year","region","technology","subsector","sector")) %>%
  replace_na(list(biophys=0,consumption=0))
green.water <- green.water %>% mutate(green = biophys - consumption) %>% dplyr::select(scenario,gcm,year,region,subsector,sector,technology,green)

green.water <- green.water %>%
  group_by(scenario,gcm,year,region,subsector) %>% summarise(green=sum(green)) %>% ungroup()

consumption <- consumption %>%
  group_by(scenario,gcm,year,region,subsector) %>% summarise(consumption=sum(consumption)) %>% ungroup()


field.eff <- read.csv("/Users/grah436/Desktop/PNNL_Projects/vwt_drivers/L165.ag_IrrEff_R.csv")
id_to_region <- read.csv("/Users/grah436/Desktop/PNNL_Projects/vwt_drivers/GCAM_region_names.csv")
field.eff <- left_join(field.eff,id_to_region,by=c("GCAM_region_ID"))

########## Virtual Water Analysis ############
demand.balances <- rbind(getQuery(prj_gfdl,'demand balances by crop commodity') %>% mutate(gcm="gfdl"),
                         getQuery(prj_ipsl,'demand balances by crop commodity')%>% mutate(gcm="ipsl"),
                         getQuery(prj_miroc,'demand balances by crop commodity')%>% mutate(gcm="miroc"),
                         getQuery(prj_hadgem,'demand balances by crop commodity')%>% mutate(gcm="hadgem"),
                         getQuery(prj_noresm,'demand balances by crop commodity')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(demand=value)

ag.prod <- rbind(getQuery(prj_gfdl,'ag production by tech') %>% mutate(gcm="gfdl"),
                 getQuery(prj_ipsl,'ag production by tech')%>% mutate(gcm="ipsl"),
                 getQuery(prj_miroc,'ag production by tech')%>% mutate(gcm="miroc"),
                 getQuery(prj_hadgem,'ag production by tech')%>% mutate(gcm="hadgem"),
                 getQuery(prj_noresm,'ag production by tech')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(ag.prod=value)

ag.prod.irr <- ag.prod %>% dplyr::filter(grepl("IRR",technology)) %>% dplyr::rename(ag.prod.irr=ag.prod)
aggregate.ag.prod.irr <- ag.prod.irr %>%
  group_by(scenario,gcm,sector,region,year) %>%
  summarise(ag.prod.irr = sum(ag.prod.irr)) %>%
  ungroup()

ag.prod.rfd <-ag.prod %>% dplyr::filter(grepl("RFD",technology)) %>% dplyr::rename(ag.prod.rfd=ag.prod)
aggregate.ag.prod.rfd <- ag.prod.rfd %>%
  group_by(scenario,gcm,sector,region,year) %>%
  summarise(ag.prod.rfd = sum(ag.prod.rfd)) %>%
  ungroup()


ag.prod.biomass <- dplyr::filter(ag.prod,sector=="biomass")

####Find total production of crops in a region [Mt]
aggregate.ag.prod <- ag.prod %>%
  group_by(scenario,gcm,sector,region,year) %>%
  summarise(ag.prod = sum(ag.prod)) %>%
  ungroup()

####Find total demand of crops in a region [Mt]
demand.balances <-  demand.balances %>% dplyr::rename(type=sector) %>% dplyr::rename(sector=input)
aggregate.demand.balances <- demand.balances %>%
  group_by(scenario,gcm,sector,region,year) %>%
  summarise(demand = sum(demand)) %>%
  ungroup()

purpose_biomass <- rbind(getQuery(prj_gfdl,'purpose-grown biomass production') %>% mutate(gcm="gfdl"),
                         getQuery(prj_ipsl,'purpose-grown biomass production')%>% mutate(gcm="ipsl"),
                         getQuery(prj_miroc,'purpose-grown biomass production')%>% mutate(gcm="miroc"),
                         getQuery(prj_hadgem,'purpose-grown biomass production')%>% mutate(gcm="hadgem"),
                         getQuery(prj_noresm,'purpose-grown biomass production')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(purpose.biomass=value)

msw_production <- rbind(getQuery(prj_gfdl,'MSW production') %>% mutate(gcm="gfdl"),
                        getQuery(prj_ipsl,'MSW production')%>% mutate(gcm="ipsl"),
                        getQuery(prj_miroc,'MSW production')%>% mutate(gcm="miroc"),
                        getQuery(prj_hadgem,'MSW production')%>% mutate(gcm="hadgem"),
                        getQuery(prj_noresm,'MSW production')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(msw.production=value)

biomass_consumption <- rbind(getQuery(prj_gfdl,'regional biomass consumption') %>% mutate(gcm="gfdl"),
                             getQuery(prj_ipsl,'regional biomass consumption')%>% mutate(gcm="ipsl"),
                             getQuery(prj_miroc,'regional biomass consumption')%>% mutate(gcm="miroc"),
                             getQuery(prj_hadgem,'regional biomass consumption')%>% mutate(gcm="hadgem"),
                             getQuery(prj_noresm,'regional biomass consumption')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(biomass.consumption=value)

residue_biomass <- rbind(getQuery(prj_gfdl,'residue biomass production') %>% mutate(gcm="gfdl"),
                         getQuery(prj_ipsl,'residue biomass production')%>% mutate(gcm="ipsl"),
                         getQuery(prj_miroc,'residue biomass production')%>% mutate(gcm="miroc"),
                         getQuery(prj_hadgem,'residue biomass production')%>% mutate(gcm="hadgem"),
                         getQuery(prj_noresm,'residue biomass production')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(residue.biomass=value)

outputs <- rbind(getQuery(prj_gfdl,'outputs by tech') %>% mutate(gcm="gfdl"),
                 getQuery(prj_ipsl,'outputs by tech')%>% mutate(gcm="ipsl"),
                 getQuery(prj_miroc,'outputs by tech')%>% mutate(gcm="miroc"),
                 getQuery(prj_hadgem,'outputs by tech')%>% mutate(gcm="hadgem"),
                 getQuery(prj_noresm,'outputs by tech')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(trade=value) %>%
  dplyr::filter(grepl("regional",sector)|sector=="total biomass"|grepl("Fodder",sector))

residue_biomass <- residue_biomass %>%
  group_by(scenario,gcm,region,year) %>%
  summarise(residue.biomass = sum(residue.biomass)) %>%
  ungroup()
biomass <- full_join(purpose_biomass,residue_biomass,by=c("region","scenario","gcm","year"))
biomass <- full_join(biomass,msw_production,by=c("region","scenario","gcm","year"))
biomass$purpose.biomass[is.na(biomass$purpose.biomass)] <-0
biomass$residue.biomass[is.na(biomass$residue.biomass)] <-0
biomass$msw.production[is.na(biomass$msw.production)] <-0
biomass <- biomass %>% mutate(total.biomass = purpose.biomass + msw.production + residue.biomass)

biomass_import <- full_join(biomass, biomass_consumption,by=c("scenario","gcm","year","region"))
biomass_import$biomass.consumption[is.na(biomass_import$biomass.consumption)] <-0
biomass_import$purpose.biomass[is.na(biomass_import$purpose.biomass)] <-0
biomass_import$residue.biomass[is.na(biomass_import$residue.biomass)] <-0
biomass_import$total.biomass[is.na(biomass_import$total.biomass)] <-0
biomass_trade <- biomass_import %>% mutate(trade = total.biomass - biomass.consumption) %>% dplyr::select(year,scenario,gcm,region,total.biomass,biomass.consumption,trade) %>%
  dplyr::rename(net=trade) %>% dplyr::rename(production=total.biomass, demand=biomass.consumption)
biomass_trade <- data.frame(sector=c("biomass"),biomass_trade) %>% dplyr::rename(ag.prod=production)


imports <- full_join(aggregate.ag.prod,aggregate.demand.balances, by=c("scenario","gcm","sector","year","region"))
imports$ag.prod[is.na(imports$ag.prod)] <-0
imports$demand[is.na(imports$demand)] <-0
imports <- imports %>% mutate(net = ag.prod - demand)
imports <- dplyr::filter(imports,sector!="biomass")
imports <- rbind(imports,biomass_trade)

##Find the proportion of production that is irrigated so we can infer the amount of exports are irrigated
new.imports <- outputs %>%
  dplyr::filter(grepl("imported",subsector)) %>%
  dplyr::select(scenario,gcm,region,year,sector,subsector,trade) %>%
  left_join(aggregate.demand.balances %>%
              mutate(sector=tolower(sector),
                     sector=gsub("roottuber","root_tuber",sector)), by=c("scenario","gcm","region","year","sector")) %>%
  dplyr::rename(imports=trade)

biomass_consumption <- rbind(getQuery(prj_gfdl,'regional biomass consumption') %>% mutate(gcm="gfdl"),
                             getQuery(prj_ipsl,'regional biomass consumption')%>% mutate(gcm="ipsl"),
                             getQuery(prj_miroc,'regional biomass consumption')%>% mutate(gcm="miroc"),
                             getQuery(prj_hadgem,'regional biomass consumption')%>% mutate(gcm="hadgem"),
                             getQuery(prj_noresm,'regional biomass consumption')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(biomass.consumption=value) %>%
  dplyr::select(scenario,gcm,year,region,biomass.consumption)

new.imports %>% dplyr::filter(grepl("biomass",subsector)) %>%
  left_join(biomass_consumption, by = c("region","year","scenario","gcm")) %>%
  dplyr::select(-demand) %>% dplyr::rename(demand=biomass.consumption) %>%
  bind_rows(
    new.imports %>% dplyr::filter(!grepl("biomass",subsector))
    ) -> new.imports


aggregate.ag.prod.irr %>%
  mutate(sector=tolower(sector),
         sector=gsub("roottuber","root_tuber",sector)) %>%
  full_join(new.imports %>%
            mutate(sector = gsub("regional ","",sector),
                   sector = gsub("total ","",sector)), by=c("scenario","gcm","region","year","sector"))  %>%
  left_join(aggregate.ag.prod %>%
              mutate(sector=tolower(sector),
                     sector=gsub("roottuber","root_tuber",sector)), by=c("scenario","gcm","region","year","sector")) %>%
  replace_na(list(imports=0,demand=0,ag.prod=0)) %>%
  na.omit() %>%
  mutate(irr.imports =(ag.prod.irr/ag.prod)*imports,
         exports =  (ag.prod + imports) - demand,
         exports = if_else(exports<0,0,exports),
         irr.exports = (ag.prod.irr/ag.prod)*exports) %>%
  replace_na(list(irr.imports=0,irr.exports=0))->
  irr.trade

aggregate.ag.prod.rfd %>%
  mutate(sector=tolower(sector),
         sector=gsub("roottuber","root_tuber",sector)) %>%
  full_join(new.imports %>%
              mutate(sector = gsub("regional ","",sector),
                     sector = gsub("total ","",sector)), by=c("scenario","gcm","region","year","sector"))  %>%
  left_join(aggregate.ag.prod %>%
              mutate(sector=tolower(sector),
                     sector=gsub("roottuber","root_tuber",sector)), by=c("scenario","gcm","region","year","sector")) %>%
  replace_na(list(imports=0,demand=0,ag.prod=0)) %>%
  na.omit() %>%
  mutate(rfd.imports =(ag.prod.rfd/ag.prod)*imports,
         exports =  (ag.prod + imports) - demand,
         exports = if_else(exports<0,0,exports),
         rfd.exports = (ag.prod.rfd/ag.prod)*exports) %>%
  replace_na(list(rfd.imports=0,rfd.exports=0))->
  rfd.trade

vw.importers.region <- irr.trade %>% group_by(scenario,gcm,year,region,sector) %>% summarise(regional.export = sum(irr.exports),regional.import=sum(irr.imports))
vw.importers <- irr.trade %>% group_by(scenario,gcm,year,sector) %>% summarise(total.export = sum(irr.exports),total.import=sum(irr.imports))
vw.irr.importers <- left_join(vw.importers,vw.importers.region,by=c("scenario","gcm","year","sector")) %>% mutate(percent.of.exports = regional.export/total.export,
                                                                                                            percent.of.imports = regional.import/total.import)


vw.importers.region <- rfd.trade %>% group_by(scenario,gcm,year,region,sector) %>% summarise(regional.export = sum(rfd.exports),regional.import=sum(rfd.imports))
vw.importers <- rfd.trade %>% group_by(scenario,gcm,year,sector) %>% summarise(total.export = sum(rfd.exports),total.import=sum(rfd.imports))
vw.rfd.importers <- left_join(vw.importers,vw.importers.region,by=c("scenario","gcm","year","sector")) %>% mutate(percent.of.exports = regional.export/total.export,
                                                                                                            percent.of.imports = regional.import/total.import)



##Combine hi and lo
ag.prod.irr <- ag.prod.irr %>% group_by(scenario,gcm,year,region,sector,subsector) %>% summarise(ag.prod.irr = sum(ag.prod.irr)) %>% ungroup()
ag.prod.rfd <- ag.prod.rfd %>% group_by(scenario,gcm,year,region,sector,subsector) %>% summarise(ag.prod.rfd = sum(ag.prod.rfd)) %>% ungroup()


irr.ag.trade <- irr.trade %>%  dplyr::rename(regional.irr.production=ag.prod.irr) %>% dplyr::select(-subsector)
rfd.ag.trade <- rfd.trade %>%  dplyr::rename(regional.rfd.production=ag.prod.rfd)%>% dplyr::select(-subsector)


irr.ag.trade <- left_join(irr.ag.trade,ag.prod.irr%>% mutate(sector=tolower(sector),
                                                             sector=gsub("roottuber","root_tuber",sector)),by=c("year","scenario","gcm","region","sector"))
percent.irr.trade <- irr.ag.trade %>% mutate(percent.trade.ex = (irr.exports*(ag.prod.irr/regional.irr.production)),
                                             percent.trade.im = (irr.imports*(ag.prod.irr/regional.irr.production)))
percent.irr.trade$percent.trade.ex[percent.irr.trade$percent.trade.ex < 0] <- 0
percent.irr.trade$percent.trade.im[percent.irr.trade$percent.trade.im < 0] <- 0

rfd.ag.trade <- left_join(rfd.ag.trade,ag.prod.rfd%>% mutate(sector=tolower(sector),
                                                             sector=gsub("roottuber","root_tuber",sector)),by=c("year","scenario","gcm","region","sector"))
percent.rfd.trade <- rfd.ag.trade %>% mutate(percent.trade.ex = (rfd.exports*(ag.prod.rfd/regional.rfd.production)),
                                             percent.trade.im = (rfd.imports*(ag.prod.rfd/regional.rfd.production)))
percent.rfd.trade$percent.trade.ex[percent.rfd.trade$percent.trade.ex < 0] <- 0
percent.rfd.trade$percent.trade.im[percent.rfd.trade$percent.trade.im < 0] <- 0

withdrawal <- rbind(getQuery(prj_gfdl,'water withdrawals by water mapping source') %>% mutate(gcm="gfdl"),
                    getQuery(prj_ipsl,'water withdrawals by water mapping source')%>% mutate(gcm="ipsl"),
                    getQuery(prj_miroc,'water withdrawals by water mapping source')%>% mutate(gcm="miroc"),
                    getQuery(prj_hadgem,'water withdrawals by water mapping source')%>% mutate(gcm="hadgem"),
                    getQuery(prj_noresm,'water withdrawals by water mapping source')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(withdrawal=value)

irr.withdrawal <- withdrawal[grep("irr", withdrawal$input), ]
irr.withdrawal <- irr.withdrawal %>%
  left_join(field.eff,by=c("region")) %>%
  mutate (withdrawal = withdrawal/conveyance.eff) %>%
  dplyr::select(-GCAM_region_ID, -field.eff, -conveyance.eff)
nonirr.withdrawal <- withdrawal[!grepl("irr", withdrawal$input), ]
withdrawal <- bind_rows(irr.withdrawal,nonirr.withdrawal)

region.withdrawal <- withdrawal %>% group_by(scenario,gcm,year,region) %>% summarise(regional.withdrawal = sum(withdrawal)) ##Blue water withdrawal Regionally
irr.withdrawal <- withdrawal[grep("irr", withdrawal$input), ]
irr.withdrawal <- irr.withdrawal %>% group_by(scenario,gcm,year,region) %>% summarise(irr.withdrawal = sum(withdrawal))

irr.percent <- left_join(region.withdrawal,irr.withdrawal,by=c("year","scenario","gcm","region")) %>% mutate(irr.percent = irr.withdrawal/regional.withdrawal)

withdrawal <- withdrawal %>% group_by(scenario,gcm,year,region) %>% mutate(regional.withdrawal = sum(withdrawal)) ##Blue water withdrawal Regionally
withdrawal <- withdrawal[grep("irr", withdrawal$input), ]
withdrawal <- separate(withdrawal, input, c('water','id','irr','basin','W'), sep="_") %>% group_by(scenario,gcm,year,region) %>% mutate(irr.percent = (sum(withdrawal)/regional.withdrawal))

rss.prod <- rbind(getQuery(prj_gfdl,'Water withdrawals by water source (runoff vs. groundwater)') %>% mutate(gcm="gfdl"),
                  getQuery(prj_ipsl,'Water withdrawals by water source (runoff vs. groundwater)')%>% mutate(gcm="ipsl"),
                  getQuery(prj_miroc,'Water withdrawals by water source (runoff vs. groundwater)')%>% mutate(gcm="miroc"),
                  getQuery(prj_hadgem,'Water withdrawals by water source (runoff vs. groundwater)')%>% mutate(gcm="hadgem"),
                  getQuery(prj_noresm,'Water withdrawals by water source (runoff vs. groundwater)')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(water.source=value) %>%
  mutate(subresource = if_else(grepl("groundwater",subresource),"groundwater","runoff"))

rss.prod.total <- rss.prod %>% dplyr::rename(basin.name.old=resource) %>% group_by(year,scenario,gcm, basin.name.old)%>% summarise(total.water = sum(water.source))
rss.prod.ground <- rss.prod %>% dplyr::rename(basin.name.old=resource) %>% dplyr::filter(subresource=="groundwater") %>%group_by(year,scenario,gcm, basin.name.old)%>% summarise(water.source = sum(water.source))
rss.prod <- full_join(rss.prod.total,rss.prod.ground)
rss.prod$basin.name.old <- gsub("*.water withdrawals","",rss.prod$basin.name.old)
basin.id.swap <- read.csv("/Users/grah436/Desktop/UMD Computer/SSP_Climate/basin_ID_swap.csv",sep=",")
rss.prod <- left_join(rss.prod,basin.id.swap,by=c("basin.name.old"))
rss.prod <- rss.prod %>% mutate(ground.to.total = water.source/total.water)

#### Virtual Blue Eater Trade (VBT)
virtual.blue <- left_join(percent.irr.trade,consumption,by=c("scenario","gcm","year","subsector","region"))
virtual.blue$consumption[is.na(virtual.blue$consumption)] <-0
virtual.blue.trade <- virtual.blue %>% separate(subsector, c('subsector',"basin.name"), sep="_")%>% left_join(rss.prod,by=c("basin.name","year","scenario","gcm"))  %>%
mutate(vwe =(consumption/ag.prod.irr)*percent.trade.ex,vwi =((consumption/ag.prod.irr)*percent.trade.im)*-1)
virtual.blue.trade %>% dplyr::select(scenario,gcm,region,year,subsector,basin.name,vwe,vwi) %>%
  replace_na(list(vwe=0,vwi=0))->
  VBT

#### Virtual Groundwater Trade (VGWT)
virtual.ground <- left_join(percent.irr.trade,consumption,by=c("scenario","gcm","year","subsector","region"))
virtual.ground$consumption[is.na(virtual.ground$consumption)] <-0
virtual.ground.trade <- virtual.ground %>% separate(subsector, c('subsector',"basin.name.old"), sep="_")%>% left_join(rss.prod,by=c("basin.name.old","year","scenario","gcm"))  %>% mutate(vwe =(consumption/ag.prod.irr)*percent.trade.ex*ground.to.total,vwi =((consumption/ag.prod.irr)*percent.trade.im*ground.to.total)*-1)
virtual.ground.trade %>% dplyr::select(scenario,gcm,region,year,subsector,basin.name.old,vwe,vwi) %>%
  replace_na(list(vwe=0,vwi=0))->
  VGWT

#### Virtual Green Trade (VGT)
virtual.green <- left_join(percent.rfd.trade,green.water,by=c("scenario","gcm","year","subsector","region"))
virtual.green$green[is.na(virtual.green$green)] <- 0
virtual.green.trade <- virtual.green %>% separate(subsector, c('subsector',"basin.name"), sep="_")%>% mutate(vwe =(green/ag.prod.rfd)*percent.trade.ex,vwi =((green/ag.prod.rfd)*percent.trade.im)*-1)
virtual.green.trade %>% dplyr::select(scenario,gcm,region,year,subsector,basin.name,vwe,vwi) %>%
  replace_na(list(vwe=0,vwi=0))->
  VGT





### Figure 1 ###
library(rmap)
virtual.green.water.trade %>% left_join(basin.id.swap, by=c("basin.name"))%>% mutate(basin.name.old = gsub(" ","_",basin.name.old)) %>% replace_na(list(basin.name.old="Madasgacar")) %>% na.omit() %>% rename(subRegion=basin.name.old) %>%
  group_by(scenario,gcm,year,subRegion) %>% summarise(vwe=sum(vwe),vwi=sum(vwi)) %>% ungroup() %>%
  group_by(scenario,year,subRegion) %>% summarise(vwe=mean(vwe)) %>% ungroup() %>%
  dplyr::filter(year==2100) %>% rename(value=vwe,class=scenario) %>% ungroup() %>%
  mutate(class = gsub("High_MI","High",class),
         class = gsub("HighMI","High",class),
         class = gsub("Reference","Medium",class),
         class = gsub("SS","Low",class),
         class = gsub("_2p6"," 2.6",class)) %>%
  dplyr::filter(!grepl("2.6",class)) %>%
  mutate(scenario=factor(class,levels=c("High","High 2.6", "Medium","Medium 2.6", "Low","Low 2.6"))) %>%
  mutate(subRegion= gsub("-","_",subRegion)) %>%
  dplyr::select(-class) %>% mutate(value=if_else(value>=100,100,value))-> x
rmap::map(x,
          #scaleRange = c(0,100),
          underLayer = mapGCAMBasins,
          palette = "Greens",
          legendType = "continuous",
          legendTitle = ,
          #legendBreaksn = 25,
          width=21,
          height=24,
          ncol=1,
          background = T,
          show=F) -> b
b[[1]] + ggplot2::theme(legend.key.height= unit(2, 'cm'),legend.text = element_text(size=15), strip.text.x = element_blank(), strip.background.x = element_blank(),strip.text.y = element_text(face="bold",size=15,color="black"), strip.background.y = element_blank()) ->
  vge


virtual.blue.water.trade %>% left_join(basin.id.swap, by=c("basin.name"))%>% mutate(basin.name.old = gsub(" ","_",basin.name.old)) %>% replace_na(list(basin.name.old="Madasgacar")) %>% na.omit() %>% rename(subRegion=basin.name.old) %>%
  group_by(scenario,gcm,year,subRegion) %>% summarise(vwe=sum(vwe),vwi=sum(vwi)) %>% ungroup() %>%
  group_by(scenario,year,subRegion) %>% summarise(vwe=mean(vwe)) %>% ungroup() %>%
  dplyr::filter(year==2100) %>% rename(value=vwe,class=scenario) %>% ungroup() %>%
  mutate(class = gsub("High_MI","High",class),
         class = gsub("HighMI","High",class),
         class = gsub("Reference","Medium",class),
         class = gsub("SS","Low",class),
         class = gsub("_2p6"," 2.6",class)) %>%
  dplyr::filter(!grepl("2.6",class)) %>%
  mutate(scenario=factor(class,levels=c("High","High 2.6", "Medium","Medium 2.6", "Low","Low 2.6"))) %>%
  mutate(subRegion= gsub("-","_",subRegion)) %>%
  dplyr::select(-class) %>% mutate(value=if_else(value>=10,10,value))-> x
rmap::map(x,
          #scaleRange = c(0,10),
          underLayer = mapGCAMBasins,
          palette = "pal_wet",
          legendType = "continuous",
          legendTitle = ,
          #legendBreaksn = 25,
          width=21,
          height=24,
          ncol=1,
          background = T,
          show=F) -> b
b[[1]]  + ggplot2::theme(legend.key.height= unit(2, 'cm'),legend.text = element_text(size=15), strip.text.x = element_blank(), strip.background.x = element_blank(),strip.text.y = element_text(face="bold",size=15,color="black"), strip.background.y = element_blank()) ->
  vbe

virtual.ground.water.trade%>% dplyr::filter(scenario!="MI"&scenario!="MI_2p6") %>% left_join(basin.id.swap %>% dplyr::rename(basin.name.new=basin.name.old), by=c("basin.name.old"="basin.name"))%>% mutate(basin.name.new = gsub(" ","_",basin.name.new)) %>% replace_na(list(basin.name.new="Madasgacar")) %>% na.omit() %>% rename(subRegion=basin.name.new) %>%
  group_by(scenario,gcm,year,subRegion) %>% summarise(vwe=sum(vwe),vwi=sum(vwi)) %>% ungroup() %>%
  group_by(scenario,year,subRegion) %>% summarise(vwe=mean(vwe)) %>% ungroup() %>%
  dplyr::filter(year==2100) %>% rename(value=vwe,class=scenario) %>% ungroup() %>%
  mutate(class = gsub("High_MI","High",class),
         class = gsub("HighMI","High",class),
         class = gsub("Reference","Medium",class),
         class = gsub("SS","Low",class),
         class = gsub("_2p6"," 2.6",class)) %>%
  dplyr::filter(!grepl("2.6",class)) %>%
  mutate(scenario=factor(class,levels=c("High","High 2.6", "Medium","Medium 2.6", "Low","Low 2.6"))) %>%
  mutate(subRegion= gsub("-","_",subRegion)) %>%
  dplyr::select(-class)%>% mutate(value=if_else(value>=1,1,value))-> x
rmap::map(x,
          #scaleRange = c(0,1),
          underLayer = mapGCAMBasins,
          palette = "Purples",
          legendType = "continuous",
          legendTitle = ,
          #legendBreaksn = 25,
          width=21,
          height=24,
          ncol=1,
          background = T,
          show=F) -> b
b[[1]]  + ggplot2::theme(legend.key.height= unit(2, 'cm'),legend.text = element_text(size=15), strip.text.x = element_blank(), strip.background.x = element_blank(),strip.text.y = element_text(face="bold",size=15,color="black"), strip.background.y = element_blank()) ->
  vgwe
+ ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, title=expression(VGWE~km^{3})))

virtual.water.maps <- ggarrange(vge,vbe,vgwe, ncol=3,labels = c("A", "B", "C"), common.legend = F)
ggsave(virtual.water.maps,filename="test.png",width=10.0,height=8.0)

VGT %>%
  group_by(scenario,gcm,year) %>% summarise(vwe=sum(vwe)) %>% ungroup() %>%
  group_by(scenario,year) %>% summarise(mean=mean(vwe),min=min(vwe),max=max(vwe)) %>% ungroup() %>%
  dplyr::filter(!grepl("2p6",scenario)) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High", "Medium", "Low"))) %>%
  dplyr::filter(year>=2015) %>% mutate(type="green")  ->
  vg
VBT %>%
  group_by(scenario,gcm,year) %>% summarise(vwe=sum(vwe)) %>% ungroup() %>%
  group_by(scenario,year) %>% summarise(mean=mean(vwe),min=min(vwe),max=max(vwe)) %>% ungroup() %>%
  dplyr::filter(!grepl("2p6",scenario)) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High", "Medium", "Low"))) %>%
  dplyr::filter(year>=2015) %>% mutate(type="blue")  ->
  vb
VGWT %>%
  group_by(scenario,gcm,year) %>% summarise(vwe=sum(vwe)) %>% ungroup() %>%
  group_by(scenario,year) %>% summarise(mean=mean(vwe),min=min(vwe),max=max(vwe)) %>% ungroup() %>%
  dplyr::filter(!grepl("2p6",scenario)) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High", "Medium", "Low"))) %>%
  dplyr::filter(year>=2015) %>% mutate(type="ground") ->
  vgw
rbind(vg,vb,vgw) -> virtual.ribbon

ggplot() +
  geom_ribbon(data=vg %>% dplyr::filter(scenario=="High"), aes(ymin=min,ymax=max,x=as.numeric(year)), fill="forestgreen",alpha=0.2) +
  geom_ribbon(data=vg %>% dplyr::filter(scenario=="Medium"), aes(ymin=min,ymax=max,x=as.numeric(year)), fill="forestgreen",alpha=0.2) +
  geom_ribbon(data=vg %>% dplyr::filter(scenario=="Low"), aes(ymin=min,ymax=max,x=as.numeric(year)), fill="forestgreen",alpha=0.2) +
  geom_ribbon(data=vb %>% dplyr::filter(scenario=="High"), aes(ymin=min*10,ymax=max*10,x=as.numeric(year)), fill="mediumblue",alpha=0.2) +
  geom_ribbon(data=vb %>% dplyr::filter(scenario=="Medium"), aes(ymin=min*10,ymax=max*10,x=as.numeric(year)), fill="mediumblue",alpha=0.2) +
  geom_ribbon(data=vb %>% dplyr::filter(scenario=="Low"), aes(ymin=min*10,ymax=max*10,x=as.numeric(year)), fill="mediumblue",alpha=0.2) +
  geom_ribbon(data=vgw %>% dplyr::filter(scenario=="High"), aes(ymin=min*10,ymax=max*10,x=as.numeric(year)), fill="purple4",alpha=0.2) +
  geom_ribbon(data=vgw %>% dplyr::filter(scenario=="Medium"), aes(ymin=min*10,ymax=max*10,x=as.numeric(year)), fill="purple4",alpha=0.2) +
  geom_ribbon(data=vgw %>% dplyr::filter(scenario=="Low"), aes(ymin=min*10,ymax=max*10,x=as.numeric(year)), fill="purple4",alpha=0.2) +
  geom_line(data = vg,
            aes(x=year,y=mean,linetype=scenario),col="forestgreen",size=1.1) +
  geom_line(data = vb,
            aes(x=year,y=mean*10,linetype=scenario),col="mediumblue",size=1.1) +
  geom_line(data = vgw,
            aes(x=year,y=mean*10,linetype=scenario),col="purple4",size=1.1) +
  scale_x_continuous(breaks=seq(2020,2100,20), expand = c(0.01,0)) +
  scale_y_continuous(expand = c(0,0),sec.axis = sec_axis(~./10, breaks =c(0,100,200,300,400,500,600),name=expression(paste("Annual Blue Water Flows (",km^3,")")))) +
  scale_color_manual(values = c("forestgreen","mediumblue","purple1"))+
  labs(title="",x="",y=expression(paste("Annual Green Water Flows (",km^3,")")))+
  theme_bw() + theme(legend.text = element_text(size=15,face="bold"), legend.key.size = unit(1,"cm"), axis.text = element_text(size=18),strip.text.x = element_text(size=18,face="bold"),axis.title = element_text(size=21,face="bold"), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "none", plot.title = element_text(hjust=0.5)) +
  guides(guide_legend(col=T)) -> line.plot
ggsave(filename="VWE_line_spread.png",width=24.0,height=8.0)


fig.1 <- ggarrange(virtual.water.maps, line.plot, nrow=2,labels = c("","D"), common.legend = F,heights = c(5,1))
ggsave(fig.1,filename="test.png",width=10.0,height=15.0)



### Figure 2 ###

withdrawal <- rbind(getQuery(prj_gfdl,'water withdrawals by water mapping source') %>% mutate(gcm="gfdl"),
                    getQuery(prj_ipsl,'water withdrawals by water mapping source')%>% mutate(gcm="ipsl"),
                    getQuery(prj_miroc,'water withdrawals by water mapping source')%>% mutate(gcm="miroc"),
                    getQuery(prj_hadgem,'water withdrawals by water mapping source')%>% mutate(gcm="hadgem"),
                    getQuery(prj_noresm,'water withdrawals by water mapping source')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(withdrawal=value)

irr.withdrawal <- withdrawal[grep("irr", withdrawal$input), ]
irr.withdrawal <- irr.withdrawal %>%
  left_join(field.eff,by=c("region")) %>%
  mutate (withdrawal = withdrawal/conveyance.eff) %>%
  dplyr::select(-GCAM_region_ID, -field.eff, -conveyance.eff)

irr.withdrawal <- separate(irr.withdrawal, input, c('water','id','irr','basin','W'), sep="_") %>% group_by(scenario,gcm,year,region,basin) %>% summarise(withdrawal = sum(withdrawal)) %>% ungroup()

irr.withdrawal %>% dplyr::filter(year==2020) %>%
  rename(withdrawal.y0=withdrawal) %>%
  dplyr::select(-year) %>%
  left_join(irr.withdrawal %>% dplyr::filter(year==2100),
            by =c("scenario","gcm","region", "basin")) %>%
  replace_na(list(withdrawal=0)) %>%
  rename(withdrawal.yn=withdrawal) %>%
  dplyr::select(-year) %>%
  left_join(irr.withdrawal %>% dplyr::filter(year>2020&year<2100) %>%
              replace_na(list(withdrawal=0)) %>%
              group_by(scenario,gcm,region, basin) %>%
              summarise(withdrawal=sum(withdrawal)) %>%
              ungroup(), by=c("scenario","gcm","region", "basin")) %>%
  group_by(scenario,gcm,basin) %>%
  summarise(withdrawal.y0 = sum(withdrawal.y0),
            withdrawal.yn = sum(withdrawal.yn),
            withdrawal = sum(withdrawal)) %>%
  ungroup() %>%
  mutate(cumsum = 5 * (((withdrawal.y0 + withdrawal.yn)/2) + withdrawal)) %>%
  #mutate(cumsum = (5/2) * (withdrawal.y0 + withdrawal.yn) * withdrawal) %>%
  left_join(basin.id.swap, by=c("basin"="basin.name"))%>% mutate(basin.name.old = gsub(" ","_",basin.name.old)) %>% replace_na(list(basin.name.old="Madasgacar")) %>% na.omit() %>% rename(subRegion=basin.name.old) %>%
  dplyr::select(-basin)->
  y


y %>% ungroup()%>% dplyr::filter(scenario=="Reference") %>% dplyr::select(-scenario) %>% dplyr::rename(ref=cumsum) %>%
  right_join(y,by=c("gcm", "subRegion")) %>% mutate(diff=cumsum-ref) %>% rename(value=diff) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("Medium","Medium 2.6","High","High 2.6", "Low","Low 2.6"))) %>%
  mutate(subRegion= gsub("-","_",subRegion)) %>%
  group_by(scenario, subRegion) %>% summarise(value = mean(value)) %>% ungroup()-> z

rmap::map(z,
          underLayer = mapGCAMBasins,
          scaleRange = c(-1000,1000),
          palette = "pal_div_BluRd",
          legendType = "pretty",
          legendTitle = ,
          legendBreaksn = 25,
          width=21,
          height=24,
          ncol=2,
          background = T,
          show=F) -> b
b[[1]] + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, title=expression(Water~Savings~km^{3}))) + ggplot2::theme(legend.text = element_text(size=15), strip.text.x = element_text(face="bold",size=15,color="black"), strip.background.x = element_blank(),strip.text.y = element_text(face="bold",size=15,color="black"), strip.background.y = element_blank())

y %>% ungroup()%>% dplyr::filter(scenario=="Reference") %>%  dplyr::rename(value=cumsum) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("Medium","Medium 2.6","High","High 2.6", "Low","Low 2.6"))) %>%
  mutate(subRegion= gsub("-","_",subRegion)) %>%
  group_by(scenario, subRegion) %>% summarise(value = mean(value)) %>% ungroup()-> z

rmap::map(z,
          underLayer = mapGCAMBasins,
          scaleRange = c(0,5000),
          palette = "pal_wet",
          legendType = "pretty",
          legendTitle = ,
          legendBreaksn = 15,
          width=21,
          height=24,
          ncol=2,
          background = T,
          show=F) -> b
b[[1]] + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, title=expression(Withdrawals~km^{3}))) + ggplot2::theme(legend.text = element_text(size=15), legend.position = "left", strip.text.x = element_text(face="bold",size=15,color="black"), strip.background.x = element_blank(),strip.text.y = element_text(face="bold",size=15,color="black"), strip.background.y = element_blank())

irr.withdrawal %>% dplyr::filter(year>2015) %>%
  ungroup() %>%
  group_by(scenario,gcm,year) %>%
  summarise(withdrawal = sum(withdrawal)) ->y
  y %>% ungroup() %>%
  dplyr::filter(scenario=="Reference") %>% dplyr::select(-scenario) %>% dplyr::rename(ref=withdrawal) %>%
  right_join(y,by=c("gcm", "year")) %>% mutate(diff=withdrawal-ref) %>% rename(value=diff) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High","High 2.6", "Medium","Medium 2.6", "Low","Low 2.6"))) %>%
  group_by(scenario, gcm,year) %>% mutate(cumsum = sum(value)) %>% ungroup()-> z


z %>% dplyr::filter(scenario!="Medium") %>% group_by(scenario,year) %>% summarise(mean=mean(cumsum),min=min(cumsum),max=max(cumsum)) %>%
  ggplot() +
  geom_hline(yintercept = 0,color="black",size=0.9) +
  geom_ribbon(aes(ymin=min,ymax=max,x=as.numeric(year),fill=scenario), alpha=0.15) +
  geom_line(aes(x=year,y=mean,color=scenario),size=1) +
  scale_color_brewer(palette = "Set1",expand=c(0,0)) +
  scale_fill_brewer(palette = "Set1",expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  #facet_wrap(~scenario) +
  labs(x = "",y=expression(paste("Change in Irrigation Withdrawals (",km^3,")"))) +
  theme_bw() +theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14), legend.key.size = unit(1,"cm"),legend.text = element_text(size=15), axis.text = element_text(size=12),strip.text.x = element_text(size=15),axis.title = element_text(size=16), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))


rss.prod <- rbind(getQuery(prj_gfdl,'Water withdrawals by water source (runoff vs. groundwater)') %>% mutate(gcm="gfdl"),
                  getQuery(prj_ipsl,'Water withdrawals by water source (runoff vs. groundwater)')%>% mutate(gcm="ipsl"),
                  getQuery(prj_miroc,'Water withdrawals by water source (runoff vs. groundwater)')%>% mutate(gcm="miroc"),
                  getQuery(prj_hadgem,'Water withdrawals by water source (runoff vs. groundwater)')%>% mutate(gcm="hadgem"),
                  getQuery(prj_noresm,'Water withdrawals by water source (runoff vs. groundwater)')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(water.source=value) %>%
  mutate(subresource = if_else(grepl("groundwater",subresource),"groundwater","runoff"))


rss.prod  %>% dplyr::filter(year>2015&grepl("groundwater",subresource)) %>%
  mutate(resource =  gsub("*.water withdrawals","",resource)) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
mutate(scenario=factor(scenario,levels=c("High","High 2.6", "Medium","Medium 2.6", "Low","Low 2.6"))) %>%
  group_by(scenario, gcm,year) %>% summarise(cumsum = sum(water.source)) %>% ungroup()-> z

z %>% ungroup() %>%
  dplyr::filter(scenario=="Medium") %>% dplyr::select(-scenario) %>% dplyr::rename(ref=cumsum) %>%
  right_join(z,by=c("gcm", "year")) %>% mutate(diff=cumsum-ref) %>% rename(value=diff) %>%
  group_by(scenario, gcm,year) %>% mutate(cumsum = sum(value)) %>% ungroup()-> z

z %>% dplyr::filter(scenario!="Medium") %>% group_by(scenario,year) %>% summarise(mean=mean(cumsum),min=min(cumsum),max=max(cumsum)) %>%
  ggplot() +
  geom_hline(yintercept = 0,color="black",size=0.9) +
  geom_ribbon(aes(ymin=min,ymax=max,x=as.numeric(year),fill=scenario), alpha=0.15) +
  geom_line(aes(x=year,y=mean,color=scenario),size=1) +
  scale_color_brewer(palette = "Set1",expand=c(0,0)) +
  scale_fill_brewer(palette = "Set1",expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  #facet_wrap(~scenario) +
  labs(x = "",y=expression(paste("Change in Nonrenewable Groundwater Withdrawals (",km^3,")"))) +
  theme_bw() +theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14), legend.key.size = unit(1,"cm"),legend.text = element_text(size=15), axis.text = element_text(size=12),strip.text.x = element_text(size=15),axis.title = element_text(size=16), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))


rss.prod  %>%
  dplyr::filter(grepl("groundwater",subresource)) %>%
  mutate(resource =  gsub("*.water withdrawals","",resource)) %>%
  group_by(scenario,gcm,resource,year) %>% summarise(sum=sum(water.source)) %>% ungroup() %>%
  dplyr::filter(year==2020) %>%
  rename(withdrawal.y0=sum) %>%
  dplyr::select(-year) %>%
  left_join(rss.prod  %>%
              dplyr::filter(grepl("groundwater",subresource)) %>%
              mutate(resource =  gsub("*.water withdrawals","",resource)) %>%
              group_by(scenario,gcm,resource,year) %>% summarise(sum=sum(water.source)) %>% ungroup() %>%
              dplyr::filter(year==2100),
            by =c("scenario","gcm","resource")) %>%
  replace_na(list(sum=0)) %>%
  rename(withdrawal.yn=sum) %>%
  dplyr::select(-year) %>%
  left_join(rss.prod  %>%
              dplyr::filter(grepl("groundwater",subresource)) %>%
              mutate(resource =  gsub("*.water withdrawals","",resource)) %>%
              group_by(scenario,gcm,resource,year) %>% summarise(sum=sum(water.source)) %>% ungroup() %>%
              dplyr::filter(year>2020&year<2100) %>%
              ungroup(), by=c("scenario","gcm","resource")) %>%
  group_by(scenario,gcm,resource) %>%
  summarise(withdrawal.y0 = sum(withdrawal.y0),
            withdrawal.yn = sum(withdrawal.yn),
            withdrawal = sum(sum)) %>%
  ungroup() %>%
  mutate(cumsum = 5 * (((withdrawal.y0 + withdrawal.yn)/2) + withdrawal)) %>%
  ungroup() %>%
  dplyr::rename(basin.name.old=resource) %>%
  left_join(basin.id.swap %>%
              dplyr::rename(basin.name.new=basin.name.old),
            by=c("basin.name.old"="basin.name"))%>%
  mutate(basin.name.new = gsub(" ","_",basin.name.new)) %>%
  replace_na(list(basin.name.new="Madasgacar")) %>%
  na.omit() %>% rename(subRegion=basin.name.new)-> y


y %>% ungroup()%>% dplyr::filter(scenario=="Reference") %>% dplyr::select(-scenario) %>% dplyr::rename(ref=cumsum) %>%
  right_join(y,by="subRegion","gcm") %>% mutate(diff=cumsum-ref) %>% rename(value=diff) %>%
  replace_na(list(diff=0)) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("Medium","Medium 2.6","High","High 2.6",  "Low","Low 2.6"))) %>%
  mutate(subRegion= gsub("-","_",subRegion))%>%
  group_by(scenario, subRegion) %>% summarise(value = mean(value)) %>% ungroup()-> z

rmap::map(z,
          underLayer = mapGCAMBasins,
          scaleRange = c(-200,200),
          palette = "pal_div_BluRd",
          legendType = "pretty",
          legendTitle = ,
          legendBreaksn = 15,
          width=21,
          height=24,
          ncol=2,
          background = T,
          show=F) -> b
b[[1]] + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, title=expression(Groundwater~Savings~km^{3}))) + ggplot2::theme(legend.text = element_text(size=15), legend.position = "right", strip.text.x = element_text(face="bold",size=15,color="black"), strip.background.x = element_blank(),strip.text.y = element_text(face="bold",size=15,color="black"), strip.background.y = element_blank())

y %>% ungroup()%>% dplyr::filter(scenario=="Reference") %>%  dplyr::rename(value=cumsum) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("Medium","Medium 2.6","High","High 2.6", "Low","Low 2.6"))) %>%
  mutate(subRegion= gsub("-","_",subRegion)) %>%
  group_by(scenario, subRegion) %>% summarise(value = mean(value)) %>% ungroup()-> z

rmap::map(z,
          underLayer = mapGCAMBasins,
          scaleRange = c(0,2000),
          palette = "pal_wet",
          legendType = "pretty",
          legendTitle = ,
          legendBreaksn = 15,
          width=21,
          height=24,
          ncol=2,
          background = T,
          show=F) -> b
b[[1]] + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, title=expression(Withdrawals~km^{3}))) + ggplot2::theme(legend.text = element_text(size=15), legend.position = "left", strip.text.x = element_text(face="bold",size=15,color="black"), strip.background.x = element_blank(),strip.text.y = element_text(face="bold",size=15,color="black"), strip.background.y = element_blank())


exports <- rbind(getQuery(prj_gfdl,'outputs by tech') %>% mutate(gcm="gfdl"),
                 getQuery(prj_ipsl,'outputs by tech')%>% mutate(gcm="ipsl"),
                 getQuery(prj_miroc,'outputs by tech')%>% mutate(gcm="miroc"),
                 getQuery(prj_hadgem,'outputs by tech')%>% mutate(gcm="hadgem"),
                 getQuery(prj_noresm,'outputs by tech')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(trade=value) %>%
  dplyr::filter(grepl("traded",subsector))


new.imports <- outputs %>%
  dplyr::filter(grepl("imported",subsector)) %>%
  mutate(sector = gsub("total ","",sector)) %>%
  dplyr::select(scenario,gcm,region,year,sector,subsector,trade) %>%
  left_join(aggregate.demand.balances %>%
              mutate(sector=tolower(sector),
                     sector=gsub("roottuber","root_tuber",sector)) %>%
              bind_rows(
                biomass_consumption %>% mutate(sector="biomass") %>%
                  dplyr::rename(demand=biomass.consumption)
              ), by=c("scenario","gcm","region","year","sector")) %>%
  dplyr::rename(imports=trade) %>% na.omit()

new.imports %>% mutate(sector = gsub("regional ","",sector)) %>% left_join(exports %>%
                                                                             mutate(crop = gsub(".*traded ","",subsector),
                                                                                    region2 = gsub(" traded.*","",subsector)) %>%
                                                                             dplyr::select(region2, crop, year, scenario, trade, gcm),
                                                                           by=c("region"="region2","sector"="crop","year","scenario","gcm")) %>%
  mutate(exports=imports-trade) %>% dplyr::select(-imports, -trade, -demand, -subsector) %>%
  replace_na(list(exports=0)) %>%
  mutate(exports = if_else(sector=="biomass", exports*(1000/17.5), exports))-> y

crop_mapping <- read.csv("crop_mapping.csv")

y %>% left_join(crop_mapping %>% mutate(sector=tolower(sector)),by=c("sector")) %>%
  mutate(climate = if_else(grepl("2p6",scenario),"RCP2.6","RCP6.0")) %>%
  mutate(climate=factor(climate,levels=c("RCP6.0", "RCP2.6"))) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6","",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High", "Medium", "Low"))) %>%
  dplyr::filter(exports>0&year>=2015) %>%
  group_by(crop,scenario,climate,gcm,year) %>% summarise(exports = sum(exports)) %>% ungroup()-> y

y %>% group_by(scenario,climate,year,crop) %>% summarise(mean=mean(exports),min=min(exports),max=max(exports)) %>%
  ggplot() +
  #geom_ribbon(aes(ymin=min,ymax=max,x=as.numeric(year),fill=interaction(scenario,climate)), alpha=0.15) +
  geom_line(aes(x=year,y=mean,color=scenario,linetype=climate),size=1) +
  scale_color_brewer(palette = "Set1") +
  #scale_fill_manual(values= getPalette(colourCount)) +
  facet_wrap(~crop,ncol=3,scales="free_y") +
  labs(x="",y="Total Trade Quantities (Mt)") +
  theme_bw() +theme( legend.text = element_text(size=15), legend.key.size = unit(0.75,"cm"), axis.text = element_text(size=9),strip.text.x = element_text(size=10),axis.title = element_text(size=14), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))

exports %>%
  dplyr::filter(!grepl("beef",output)&!grepl("dairy",output)&!grepl("pork",output)&!grepl("poultry",output)&!grepl("sheepgoat",output)&!grepl("forest",output)&!grepl("gas",output)&!grepl("coal",output)) %>%
  mutate(output = gsub("traded ","",output)) %>% dplyr::filter(year>2015) %>% mutate(technology = gsub(" traded.*","",technology)) %>% dplyr::filter(output!="oil") -> x

x %>% group_by(scenario,technology,year,output,gcm) %>% summarise(trade=mean(trade)) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High","High 2.6", "Medium","Medium 2.6", "Low","Low 2.6"))) %>%
  mutate(trade = if_else(output=="biomass", trade*(1000/17.5), trade)) %>% dplyr::filter(output!="biomass")-> y

colourCount = length(unique(y$output))
getPalette = colorRampPalette(brewer.pal(9,"Set1"))

y %>% filter(technology=="India")%>%
  ggplot() +
  geom_bar(aes(x=year,y=trade,fill=output),size=0.25,color="black", stat="identity") +
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values= getPalette(colourCount)) +
  facet_grid(gcm~scenario, scales = "free") +
  labs(x="",y="Total Exports (Mt)") +
  theme_bw() +theme(axis.text.x = element_text(angle = 90), legend.text = element_text(size=15), legend.key.size = unit(0.75,"cm"), axis.text = element_text(size=9),strip.text.x = element_text(size=10),axis.title = element_text(size=14), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))

elec_gen <- read.csv("elec_gen.csv") %>% gather(key=year,value=elec_gen,`X1990`:`X2100`) %>%
  mutate(year = gsub("X","",year), year=as.numeric(year),
         scenario = if_else(grepl("2p6",scenario),"RCP 2.6", "RCP 6.0"))

elec_tech_mapping <- read.csv("/Users/grah436/Desktop/PNNL_Projects/Elec VWT/elec_vwt_mats/elec_tech_mapping_new.csv")

elec_gen %>% full_join(elec_tech_mapping, by=c("technology")) %>%
  group_by(scenario,year,aggregate_tech)%>% summarise(elec_gen = sum(elec_gen)) %>% na.omit() %>% ungroup() -> gen

# Color scheme for electricity generation by aggregate fuel
elec_tech_colors <- c("Coal" = "#a0237c",
                      "Coal CCS" = "#dab4c7",
                      "Gas" = "#25a9e0",
                      "Gas CCS" = "#84e7f9",
                      "Refined liquids" = "#d01c2a",
                      "Refined liquids CCS" = "#f7988f",
                      "Biomass" = "#00931d",
                      "Biomass CCS" = "#88c892",
                      "Nuclear" = "#ef8e27",
                      "Geothermal" = "#ad440c",
                      "Hydro" = "#fdfa28",
                      "Wind" = "#3d86f9",
                      "Onshore Wind" = "#8064a2",
                      "Offshore Wind" = "#0000FF",
                      "Solar PV" = "#fdd67b",
                      "Solar CSP" = "#507fab",
                      "CHP" = "grey89")

gen %>% na.omit() %>% dplyr::filter(year>=2015) %>%
  ggplot() +
  geom_bar(aes(x=year,y=elec_gen*2.778e2,fill=aggregate_tech),color="black",size = 0.1,stat = "identity") +
  facet_wrap(~scenario)+
  scale_fill_manual(values=elec_tech_colors,name="Fuel Type") +
  labs(x="",y="Global Electricity Generation (TWh)",title="") +
  theme_bw() +theme(axis.text = element_text(size=12),strip.text.x = element_text(size=15),axis.title = element_text(size=14), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))


basin_mapping <- read.csv("basin_to_country_mapping.csv") %>% dplyr::select(GCAM_basin_ID,Basin_long_name)
get_runoff <- function(model,extra,rcp){
  runoff <- read.csv(paste("L100.runoff_max_bm3_",model,extra,"_",rcp,".csv",sep="")) %>%
    mutate(gcm=model,climate=rcp) %>%
    left_join(basin_mapping, by="GCAM_basin_ID")
}

rbind(
get_runoff("gfdl", "-esm2m", "2p6"),
get_runoff("gfdl", "-esm2m", "6p0"),
get_runoff("hadgem", "2-es", "2p6"),
get_runoff("hadgem", "2-es", "6p0"),
get_runoff("ipsl", "-cm5a-lr", "2p6"),
get_runoff("ipsl", "-cm5a-lr", "6p0"),
get_runoff("miroc", "-esm-chem", "2p6"),
get_runoff("miroc", "-esm-chem", "6p0"),
get_runoff("noresm", "1-m", "2p6"),
get_runoff("noresm", "1-m", "6p0")
) -> gcm_runoff





















y %>% ungroup()%>% dplyr::filter(scenario=="Reference") %>% dplyr::select(-scenario) %>% dplyr::rename(ref=withdrawal) %>%
  right_join(y,by="subRegion") %>% dplyr::filter(scenario!="Reference") %>% mutate(diff=((withdrawal-ref)/ref)*100) %>% rename(value=diff) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High","High 2.6", "Medium","Medium 2.6", "Low","Low 2.6"))) %>%
  mutate(subRegion= gsub("-","_",subRegion)) %>%
  dplyr::filter(grepl("2.6",scenario))-> z

rmap::map(z,
          underLayer = mapGCAMBasins,
          scaleRange = c(-100,200),
          palette = "pal_div_BluRd",
          legendType = "pretty",
          legendTitle = "Percent Change",
          legendBreaksn = 14,
          height=12,
          ncol=1,
          background = T,
          theme = theme(legend.text = element_text(size=15), strip.background.x = element_rect(fill="white",color = "white"), strip.text.x = element_text(face="bold",size=15,colour = "black"))) -> b






colourCount = length(unique(y$sector))
getPalette = colorRampPalette(brewer.pal(9,"Set1"))
region_names <- read.csv("/Users/grah436/Desktop/PNNL_Projects/vwt_drivers/new_region_names.csv")

y %>% left_join(region_names, by = c("region")) %>% dplyr::filter(scenario=="High"|scenario=="Medium"|scenario=="Low") %>%
  mutate(scenario=factor(scenario,levels=c("High", "Medium", "Low")))%>%
  ggplot() +
  geom_hline(yintercept=0,color="black") +
  geom_bar(aes(x=reorder(new_region, -exports),y=exports,fill=crop),size=0.25,color="black", stat="identity") +
  scale_fill_brewer(palette = "Set1") +
  #scale_fill_manual(values= getPalette(colourCount)) +
  facet_wrap(~scenario,ncol=1) +
  labs(x="",y="Net Trade Quantities (Mt)") +
  theme_bw() +theme(axis.text.x = element_text(angle = 90), legend.text = element_text(size=15), legend.key.size = unit(0.75,"cm"), axis.text = element_text(size=9),strip.text.x = element_text(size=10),axis.title = element_text(size=14), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))

new.prod <- outputs %>%
  dplyr::filter(grepl("domestic",subsector)) %>%
  dplyr::select(scenario,region,year,sector,subsector,trade) %>%
  left_join(aggregate.demand.balances %>%
              mutate(sector=tolower(sector),
                     sector=gsub("roottuber","root_tuber",sector)), by=c("scenario","region","year","sector")) %>%
  dplyr::rename(prod=trade) %>% na.omit() %>%mutate(sector = gsub("regional ","",sector))  %>% dplyr::filter(scenario!="Old Fixed"&year==2100) -> y

y %>% left_join(crop_mapping %>% mutate(sector=tolower(sector)),by=c("sector")) %>%
  mutate(scenario = if_else(scenario=="Reference","Medium",
                            if_else(scenario=="High_MI","High",
                                    if_else(scenario=="SS","Low",scenario)))) %>%
  group_by(crop,scenario,year,region) %>% summarise(prod = sum(prod)) %>% ungroup()-> y

colourCount = length(unique(y$sector))
getPalette = colorRampPalette(brewer.pal(9,"Set1"))
region_names <- read.csv("/Users/grah436/Desktop/PNNL_Projects/vwt_drivers/new_region_names.csv")

y%>% left_join(region_names, by = c("region")) %>%
  mutate(scenario=factor(scenario,levels=c("High", "Medium", "Low")))%>%
  ggplot() +
  geom_hline(yintercept=0,color="black") +
  geom_bar(aes(x=reorder(new_region, -prod),y=prod,fill=crop),size=0.25,color="black", stat="identity") +
  #scale_fill_manual(values= getPalette(colourCount)) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~scenario,ncol=1,scales = "free_y") +
  labs(x="",y="Change in Domestic Production (Mt)") +
  theme_bw() +theme(axis.text.x = element_text(angle = 90), legend.text = element_text(size=15), legend.key.size = unit(0.75,"cm"), axis.text = element_text(size=9),strip.text.x = element_text(size=10),axis.title = element_text(size=14), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))


exports <- getQuery(prj, 'outputs by tech') %>% dplyr::rename(trade=value) %>%
  dplyr::filter(grepl("traded",subsector))


colourCount = length(unique(exports$output))
getPalette = colorRampPalette(brewer.pal(9,"Set1"))
exports %>% group_by(scenario,year,output) %>% summarise(sum=sum(trade)) %>% ungroup() %>%
  dplyr::filter(scenario!="Old Fixed") %>%
  mutate(scenario=factor(scenario,levels=c("Reference","Fixed Trade", "High Trade","M.I.", "High M.I.","Self-Sufficient")))%>%
  ggplot() +
  geom_bar(aes(x=year,y=sum,fill=output),color="black",size=0.5,stat="identity") +
  scale_fill_manual(values= getPalette(colourCount)) +
  facet_wrap(~scenario) +
  theme_bw()

exports %>% group_by(scenario,year,output) %>% summarise(sum=sum(trade)) %>% ungroup() %>%
  dplyr::filter(scenario=="Reference") %>% dplyr::select(-scenario) %>% dplyr::rename(ref=sum) %>% left_join(exports %>% group_by(scenario,year,output) %>%
                                                       summarise(sum=sum(trade)) %>%
                                                       ungroup() %>%dplyr::filter(scenario!="Reference"&scenario!="Old Fixed"),
                                                     by=c("year","output")) %>%
  mutate(diff=(sum-ref)/ref) %>% dplyr::filter(year>=2015) -> crop.diff

crop.diff %>% dplyr::filter(!grepl("beef",output)&!grepl("dairy",output)&!grepl("pork",output)&!grepl("poultry",output)&!grepl("sheepgoat",output)&!grepl("biomass",output)) %>%
  mutate(scenario=factor(scenario,levels=c("Fixed Trade", "High Trade","M.I.", "High M.I.","Self-Sufficient")),
         output = gsub("traded ","",output))%>%
  ggplot() +
  geom_hline(yintercept=0,color="black") +
  geom_line(aes(x=year,y=diff*100,color=scenario),size=1) +
  scale_color_brewer(palette = "Set1",name="") +
  facet_wrap(~output,ncol=3,scales = "free_y") +
  labs(x="",y="Change in Global Crop Market Quantities (%)") +
  theme_bw() +theme(legend.text = element_text(size=15), legend.key.size = unit(1,"cm"), axis.text = element_text(size=12),strip.text.x = element_text(size=15),axis.title = element_text(size=14), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))

exports %>% mutate(subsector = gsub(" traded.*","",subsector)) %>% group_by(scenario,year,subsector) %>% summarise(sum=sum(trade)) %>% ungroup() %>%
  dplyr::filter(scenario=="Reference") %>% dplyr::select(-scenario) %>% dplyr::rename(ref=sum) %>% left_join(exports %>% mutate(subsector = gsub(" traded.*","",subsector)) %>%group_by(scenario,year,subsector) %>%
                                                                                                               summarise(sum=sum(trade)) %>%
                                                                                                               ungroup() %>%dplyr::filter(scenario!="Reference"&scenario!="Old Fixed"),
                                                                                                             by=c("year","subsector")) %>%
  mutate(diff=(sum-ref)) %>% dplyr::filter(year>=2015) -> region.diff

region.diff %>%
  mutate(scenario=factor(scenario,levels=c("Fixed Trade", "High Trade","M.I.", "High M.I.","Self-Sufficient")))%>%
  ggplot() +
  geom_hline(yintercept=0,color="black") +
  geom_line(aes(x=year,y=diff,color=scenario),size=1) +
  scale_color_brewer(palette = "Set1",name="") +
  facet_wrap(~subsector,ncol=8,scales = "free_y") +
  labs(x="",y="Change in Regional Export Quantities (Mt)") +
  theme_bw() +theme(legend.text = element_text(size=15), legend.key.size = unit(1,"cm"), axis.text = element_text(size=9),strip.text.x = element_text(size=10),axis.title = element_text(size=14), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))


aggregate.ag.prod.irr %>% group_by(scenario,year,region) %>% summarise(sum=sum(ag.prod.irr)) %>% ungroup() %>%
  dplyr::filter(scenario=="Reference") %>% dplyr::select(-scenario) %>% dplyr::rename(ref=sum) %>% left_join(aggregate.ag.prod.irr %>%
                                                                                                               group_by(scenario,year,region) %>%
                                                                                                               summarise(sum=sum(ag.prod.irr)) %>%
                                                                                                               ungroup() %>%dplyr::filter(scenario!="Reference"&scenario!="Old Fixed"),
                                                                                                             by=c("year","region")) %>%
  mutate(diff=(sum-ref)) %>% dplyr::filter(year>=2015) -> prod.diff

prod.diff %>%
  mutate(scenario=factor(scenario,levels=c("Fixed Trade", "High Trade","M.I.", "High M.I.","Self-Sufficient")))%>%
  ggplot() +
  geom_hline(yintercept=0,color="black") +
  geom_line(aes(x=year,y=diff,color=scenario),size=1) +
  scale_color_brewer(palette = "Set1",name="") +
  facet_wrap(~region,ncol=8,scales = "free_y") +
  labs(x="",y="Change in Irrigated Ag Prod Quantities (Mt)") +
  theme_bw() +theme(legend.text = element_text(size=15), legend.key.size = unit(1,"cm"), axis.text = element_text(size=9),strip.text.x = element_text(size=10),axis.title = element_text(size=14), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))

aggregate.ag.prod.irr %>% dplyr::filter(region=="Middle East") %>% group_by(scenario,year,sector) %>% summarise(sum=sum(ag.prod.irr)) %>% ungroup() %>%
  dplyr::filter(scenario=="Reference") %>% dplyr::select(-scenario) %>% dplyr::rename(ref=sum) %>% left_join(aggregate.ag.prod.irr%>%
                                                                                                               dplyr::filter(region=="Middle East") %>%
                                                                                                               group_by(scenario,year,sector) %>%
                                                                                                               summarise(sum=sum(ag.prod.irr)) %>%
                                                                                                               ungroup() %>%dplyr::filter(scenario!="Reference"&scenario!="Old Fixed"),
                                                                                                             by=c("year","sector")) %>%
  mutate(diff=(sum-ref)) %>% dplyr::filter(year>=2015) -> prod.diff

prod.diff %>%
  mutate(scenario=factor(scenario,levels=c("Fixed Trade", "High Trade","M.I.", "High M.I.","Self-Sufficient")))%>%
  ggplot() +
  geom_hline(yintercept=0,color="black") +
  geom_line(aes(x=year,y=diff,color=scenario),size=1) +
  scale_color_brewer(palette = "Set1",name="") +
  facet_wrap(~sector,ncol=3,scales = "free_y") +
  labs(x="",y="Change in Irrigated Ag Prod Quantities (Mt)") +
  theme_bw() +theme(legend.text = element_text(size=15), legend.key.size = unit(1,"cm"), axis.text = element_text(size=9),strip.text.x = element_text(size=10),axis.title = element_text(size=14), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))


aggregate.ag.prod.rfd %>% group_by(scenario,year,region) %>% summarise(sum=sum(ag.prod.rfd)) %>% ungroup() %>%
  dplyr::filter(scenario=="Reference") %>% dplyr::select(-scenario) %>% dplyr::rename(ref=sum) %>% left_join(aggregate.ag.prod.rfd %>%
                                                                                                               group_by(scenario,year,region) %>%
                                                                                                               summarise(sum=sum(ag.prod.rfd)) %>%
                                                                                                               ungroup() %>%dplyr::filter(scenario!="Reference"&scenario!="Old Fixed"),
                                                                                                             by=c("year","region")) %>%
  mutate(diff=(sum-ref)) %>% dplyr::filter(year>=2015) -> prod.diff

prod.diff %>%
  mutate(scenario=factor(scenario,levels=c("Fixed Trade", "High Trade","M.I.", "High M.I.","Self-Sufficient")))%>%
  ggplot() +
  geom_hline(yintercept=0,color="black") +
  geom_line(aes(x=year,y=diff,color=scenario),size=1) +
  scale_color_brewer(palette = "Set1",name="") +
  facet_wrap(~region,ncol=8,scales = "free_y") +
  labs(x="",y="Change in Rainfed Ag Prod Quantities (Mt)") +
  theme_bw() +theme(legend.text = element_text(size=15), legend.key.size = unit(1,"cm"), axis.text = element_text(size=9),strip.text.x = element_text(size=10),axis.title = element_text(size=14), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))


exports %>% mutate(subsector = gsub(" traded.*","",subsector)) %>% group_by(scenario,year,subsector) %>% summarise(sum=sum(trade)) %>% ungroup() %>%
  dplyr::filter(scenario!="Old Fixed") %>%
  mutate(scenario=factor(scenario,levels=c("Reference","Fixed Trade", "High Trade","M.I.", "High M.I.","Self-Sufficient")))%>%
  ggplot() +
  geom_bar(aes(x=year,y=sum,fill=subsector),color="black",size=0.5,stat="identity") +
  facet_wrap(~scenario) +
  theme_bw()




x<- irr.withdrawal %>% rename(subRegion=region,value=withdrawal) %>%
  dplyr::filter(year>=2020&scenario!="Higher Trade")
metis.mapsProcess(polygonTable =x,subRegShape = metis::mapGCAMReg32 ,mapTitleOn = F,                  scenRef="Reference",
                  scenDiff=c("Fixed Trade"),folderName ="multiYear_irrWregion",legendOutsideSingle = F,legendPosition = c('LEFT','bottom'),
                  legendTitleSizeI = 0.001, fillshowNA = F, legendTextSizeI=0.4,legendFixedBreaks = 25,extension = T,facetLabelColor = "white",facetLabelSize = 0.5)

rss.prod <- getQuery(prj, 'Water withdrawals by water source (runoff vs. groundwater)') %>% dplyr::rename(water.source=value)


rss.prod  %>% dplyr::filter(scenario!="MI"&scenario!="MI_2p6") %>% dplyr::filter(year>=2015&grepl("groundwater",subresource)) %>%
  mutate(resource =  gsub("*.water withdrawals","",resource)) %>%
  group_by(scenario,year) %>% summarise(sum=sum(water.source)) %>% ungroup() -> x

x %>% dplyr::filter(scenario!="Reference") %>%
  left_join(x %>% dplyr::filter(year>=2015&scenario=="Reference")  %>% dplyr::select(-scenario)%>% rename(ref=sum),
            by=c("year")) %>% mutate(diff=sum-ref)  %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High","High 2.6", "Medium","Medium 2.6", "Low","Low 2.6"))) %>%
  ggplot() +
  geom_hline(yintercept = 0,color="black",size=0.75) +
  geom_line(aes(x=year,y=diff,color=scenario),size=1) +
  scale_color_brewer(palette = "Set1",expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  labs(x = "",y="Change in Groundwater Extraction from Reference (km^3)") +
  theme_bw() +theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14), legend.key.size = unit(1,"cm"),legend.text = element_text(size=15), axis.text = element_text(size=12),strip.text.x = element_text(size=15),axis.title = element_text(size=16), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))



rss.prod  %>% dplyr::filter(scenario!="MI"&scenario!="MI_2p6")  %>% dplyr::filter(year>2015) %>%
  dplyr::filter(grepl("groundwater",subresource)) %>%
  mutate(resource =  gsub("*.water withdrawals","",resource)) %>%
  group_by(scenario,resource,year) %>% summarise(sum=sum(water.source)) %>% ungroup() %>%
  mutate(withdrawal2=if_else(year==2100,sum,sum*5)) %>%
  ungroup() %>%
  dplyr::rename(basin.name.old=resource) %>%
  group_by(scenario,basin.name.old) %>%
  summarise(withdrawal = sum(withdrawal2)) %>%
  left_join(basin.id.swap %>% dplyr::rename(basin.name.new=basin.name.old), by=c("basin.name.old"="basin.name"))%>% mutate(basin.name.new = gsub(" ","_",basin.name.new)) %>% replace_na(list(basin.name.new="Madasgacar")) %>% na.omit() %>% rename(subRegion=basin.name.new)-> y


y %>% ungroup()%>% dplyr::filter(scenario=="Reference") %>% dplyr::select(-scenario) %>% dplyr::rename(ref=withdrawal) %>%
  right_join(y,by="subRegion") %>% dplyr::filter(scenario!="Reference") %>% mutate(diff=withdrawal-ref) %>% rename(value=diff) %>%
  replace_na(list(diff=0)) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High","High 2.6", "Medium","Medium 2.6", "Low","Low 2.6"))) %>%
  mutate(subRegion= gsub("-","_",subRegion)) %>%
  dplyr::filter(!grepl("2.6",scenario))-> z

rmap::map(z,
          underLayer = mapGCAMBasins,
          scaleRange = c(-100,200),
          palette = "pal_div_BluRd",
          legendType = "pretty",
          legendTitle = "Percent Change",
          legendBreaksn = 14,
          height=12,
          ncol=1,
          background = T,
          crop=F,
          theme = theme(legend.text = element_text(size=15), strip.background.x = element_rect(fill="white",color = "white"), strip.text.x = element_text(face="bold",size=15,colour = "black"))) -> b



library(circlize)


exports <- getQuery(prj, 'outputs by tech') %>% dplyr::rename(trade=value) %>%
  dplyr::filter(grepl("traded",subsector))


new.imports <- outputs %>%
  dplyr::filter(grepl("imported",subsector)) %>%
  mutate(sector = gsub("total ","",sector)) %>%
  dplyr::select(scenario,region,year,sector,subsector,trade) %>%
  left_join(aggregate.demand.balances %>%
              mutate(sector=tolower(sector),
                     sector=gsub("roottuber","root_tuber",sector)) %>%
              bind_rows(
                biomass_consumption %>% mutate(sector="biomass") %>%
                  dplyr::rename(demand=biomass.consumption)
              ), by=c("scenario","region","year","sector")) %>%
  dplyr::rename(imports=trade) %>% na.omit()

new.imports %>% mutate(sector = gsub("regional ","",sector)) %>% left_join(exports %>%
                                                                             mutate(crop = gsub(".*traded ","",subsector),
                                                                                    region2 = gsub(" traded.*","",subsector)) %>%
                                                                             dplyr::select(region2, crop, year, scenario, trade),
                                                                           by=c("region"="region2","sector"="crop","year","scenario")) %>%
  mutate(exports=trade) %>% dplyr::select(-imports, -trade, -demand, -subsector) %>%
  replace_na(list(exports=0)) %>%
  mutate(exports = if_else(sector=="biomass", exports*(1000/17.5), exports)) %>%
  mutate(to = "World") %>% rename(from=region)-> y




region_names <- read.csv("/Users/grah436/Desktop/PNNL_Projects/vwt_drivers/new_region_names.csv")
region_color <- c( "E. Africa" = "olivedrab2",
                   "N. Africa" = "darkgreen",
                   "S. Africa" = "green4",
                   "W. Africa" = "olivedrab3",
                   "Argentina" = "mediumpurple",
                   "Australia" = "cornflowerblue",
                   "Brazil" = "mediumpurple4",
                   "Canada" = "chocolate1",
                   "C. America" = "darkorchid4",
                   "C. Asia" = "firebrick1",
                   "China" = "firebrick4",
                   "Colombia" = "magenta4",
                   "EU-12" = "dodgerblue4",
                   "EU-15" = "dodgerblue3",
                   "E. Europe" = "deepskyblue3",
                   "Non-EU" = "cadetblue2",
                   "Euro FTA" = "cadetblue",
                   "India" = "indianred",
                   "Indonesia" = "firebrick2",
                   "Japan" = "deeppink4",
                   "Mexico" = "orchid4",
                   "Middle East" = "darkolivegreen3",
                   "Pakistan" = "indianred4",
                   "Russia" = "red4",
                   "South Africa" = "olivedrab4",
                   "N. S. America" = "purple4",
                   "S. S. America" = "purple2",
                   "S. Asia" = "indianred2",
                   "South Korea" = "deeppink3",
                   "SE Asia" = "red3",
                   "Taiwan" = "deeppink3",
                   "USA" = "chocolate3",
                   "World Exports" = "grey75",
                   "World" = "goldenrod"
                   )

order <- c("World Exports","E. Africa","N. Africa","S. Africa","W. Africa","Argentina", "Australia","Brazil","Canada","C. America",
           "C. Asia", "China",  "Colombia", "EU-12", "EU-15", "E. Europe","Non-EU", "Euro FTA","India","Indonesia","Japan", "Mexico",
           "Middle East","Pakistan","Russia","South Africa","N. S. America","S. S. America","S. Asia","South Korea","SE Asia",
           "Taiwan","USA","World")

new.imports %>% mutate(sector = gsub("regional ","",sector)) %>% dplyr::select(-demand,-subsector) %>%
  rename(to=region, value=imports) %>%
  mutate(from = "World Exports") %>%
  bind_rows(
    exports %>%
      mutate(sector = gsub(".*traded ","",subsector),
             region2 = gsub(" traded.*","",subsector)) %>%
      dplyr::rename(from = region2,value=trade) %>%
      dplyr::select(from, sector, year, scenario, value) %>% mutate(to="World")
  ) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High","High 2.6", "Medium","Medium 2.6", "Low","Low 2.6"))) %>% na.omit()-> x

scenarios = unique(x$scenario)

png(file = paste0("test.png"), width = 5000, height = 5000, res = 500)
x %>% dplyr::filter(year==2100&sector=="corn"&scenario=="High") %>% dplyr::select(-scenario,-year,-sector) %>%
  left_join(region_names %>% dplyr::select(region,new_region), by=c("to"="region")) %>% dplyr::select(-to) %>%
  mutate(to=new_region) %>% replace_na(list(to="World")) %>%dplyr::select(-new_region) %>%
  left_join(region_names %>% dplyr::select(region,new_region), by=c("from"="region")) %>% dplyr::select(-from) %>%
  mutate(from=new_region)%>%dplyr::select(-new_region) %>% replace_na(list(from="World Exports")) -> scen
col_order <- c("from","to","value")
scen <- scen[, col_order]

chordDiagram(scen, directional = 1,grid.col = region_color, order=order, direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow", annotationTrack = c("grid","axis") ,
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(scen))))))
# we go back to the first track and customize sector labels
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1] + mm_y(2), CELL_META$sector.index,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  }, bg.border = NA)
title("High")
dev.off()


virtual.ground.water.trade %>% group_by(scenario,year,region) %>% summarise(value=sum(vwe)) %>% ungroup() %>%
  dplyr::filter(scenario=="SS"&year==2100) %>%
  dplyr::select(-scenario,-year) %>%
  rename(from=region) %>%
  mutate(to="World") %>%
  left_join(region_names %>% dplyr::select(region,new_region), by=c("from"="region")) %>% dplyr::select(-from) %>%
  rename(from=new_region) ->
  x
col_order <- c("from","to","value")
x <- x[, col_order]

region_color_vwt <- c( "E. Africa" = "olivedrab2",
                   "N. Africa" = "darkgreen",
                   "S. Africa" = "green4",
                   "W. Africa" = "olivedrab3",
                   "Argentina" = "mediumpurple",
                   "Australia" = "cornflowerblue",
                   "Brazil" = "mediumpurple4",
                   "Canada" = "chocolate1",
                   "C. America" = "darkorchid4",
                   "C. Asia" = "firebrick1",
                   "China" = "firebrick4",
                   "Colombia" = "magenta4",
                   "EU-12" = "dodgerblue4",
                   "EU-15" = "dodgerblue3",
                   "E. Europe" = "deepskyblue3",
                   "Non-EU" = "cadetblue2",
                   "Euro FTA" = "cadetblue",
                   "India" = "indianred",
                   "Indonesia" = "firebrick2",
                   "Japan" = "deeppink4",
                   "Mexico" = "orchid4",
                   "Middle East" = "darkolivegreen3",
                   "Pakistan" = "indianred4",
                   "Russia" = "red4",
                   "South Africa" = "olivedrab4",
                   "N. S. America" = "purple4",
                   "S. S. America" = "purple2",
                   "S. Asia" = "indianred2",
                   "South Korea" = "deeppink3",
                   "SE Asia" = "red3",
                   "Taiwan" = "deeppink3",
                   "USA" = "chocolate3",
                   "World" = "goldenrod"
)

order_vwt <- c("E. Africa","N. Africa","S. Africa","W. Africa","Argentina", "Australia","Brazil","Canada","C. America",
           "C. Asia", "China",  "Colombia", "EU-12", "EU-15", "E. Europe","Non-EU", "Euro FTA","India","Indonesia","Japan", "Mexico",
           "Middle East","Pakistan","Russia","South Africa","N. S. America","S. S. America","S. Asia","South Korea","SE Asia",
           "Taiwan","USA","World")

#circos.par(gap.after = c(rep(5, nrow(x)-1), 5, rep(5, ncol(x)-1), 5))
chordDiagram(x, directional = 1,small.gap = 2,grid.col = region_color_vwt, order=order_vwt, direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow", annotationTrack = c("grid","axis") ,
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(scen))))))
# we go back to the first track and customize sector labels
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1] + mm_y(2), CELL_META$sector.index,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
#title("High")
circos.clear()
dev.off()















































aggregate.ag.prod.rfd  %>%
  mutate(climate = if_else(grepl("2p6",scenario),"RCP2.6","RCP6.0")) %>%
  mutate(climate=factor(climate,levels=c("RCP6.0", "RCP2.6"))) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6","",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High", "Medium", "Low"))) %>%
  group_by(sector,scenario,climate,gcm,year) %>% summarise(ag.prod.rfd = sum(ag.prod.rfd)) %>% ungroup()-> y

colourCount = length(unique(y$sector))
getPalette = colorRampPalette(brewer.pal(9,"Set1"))

y %>% dplyr::filter(gcm=="gfdl"&year>2015) %>%
  ggplot() +
  geom_bar(aes(x=year,y=ag.prod.rfd,fill=sector), stat = "identity", color="black",size=0.25) +
  scale_fill_manual(values= getPalette(colourCount)) +
  facet_wrap(climate~scenario) +
  theme_bw()



y  %>% group_by(scenario,year) %>% summarise(mean=mean(withdrawal),min=min(withdrawal),max=max(withdrawal)) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High","High 2.6", "Medium","Medium 2.6", "Low","Low 2.6"))) %>%
  ggplot() +
  geom_ribbon(aes(ymin=min,ymax=max,x=as.numeric(year),fill=scenario), alpha=0.15) +
  geom_line(aes(x=year,y=mean,color=scenario),size=1) +
  scale_color_brewer(palette = "Set1",expand=c(0,0)) +
  scale_fill_brewer(palette = "Set1",expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  #facet_wrap(~scenario) +
  labs(x = "",y=expression(paste("Total Irrigation Withdrawals in India (",km^3,")"))) +
  theme_bw() +theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14), legend.key.size = unit(1,"cm"),legend.text = element_text(size=15), axis.text = element_text(size=12),strip.text.x = element_text(size=15),axis.title = element_text(size=16), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))


z  %>% group_by(scenario,year) %>% summarise(mean=mean(cumsum),min=min(cumsum),max=max(cumsum)) %>%
  ggplot() +
  geom_ribbon(aes(ymin=min,ymax=max,x=as.numeric(year),fill=scenario), alpha=0.15) +
  geom_line(aes(x=year,y=mean,color=scenario),size=1) +
  scale_color_brewer(palette = "Set1",expand=c(0,0)) +
  scale_fill_brewer(palette = "Set1",expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  #facet_wrap(~scenario) +
  labs(x = "",y=expression(paste("Total Nonrenewable Groundwater Withdrawals (",km^3,")"))) +
  theme_bw() +theme(axis.text.y = element_text(size=14),axis.text.x = element_text(size=14), legend.key.size = unit(1,"cm"),legend.text = element_text(size=15), axis.text = element_text(size=12),strip.text.x = element_text(size=15),axis.title = element_text(size=16), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))

















ag.prod <- rbind(getQuery(prj_gfdl,'ag production by tech') %>% mutate(gcm="gfdl"),
                 getQuery(prj_ipsl,'ag production by tech')%>% mutate(gcm="ipsl"),
                 getQuery(prj_miroc,'ag production by tech')%>% mutate(gcm="miroc"),
                 getQuery(prj_hadgem,'ag production by tech')%>% mutate(gcm="hadgem"),
                 getQuery(prj_noresm,'ag production by tech')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(ag.prod=value)

ag.prod.irr <- ag.prod %>% dplyr::filter(grepl("IRR",technology)) %>% dplyr::rename(ag.prod.irr=ag.prod)
aggregate.ag.prod.irr <- ag.prod.irr %>%
  group_by(scenario,gcm,sector,region,year) %>%
  summarise(ag.prod.irr = sum(ag.prod.irr)) %>%
  ungroup()

ag.prod.rfd <-ag.prod %>% dplyr::filter(grepl("RFD",technology)) %>% dplyr::rename(ag.prod.rfd=ag.prod)
aggregate.ag.prod.rfd <- ag.prod.rfd %>%
  group_by(scenario,gcm,sector,region,year) %>%
  summarise(ag.prod.rfd = sum(ag.prod.rfd)) %>%
  ungroup()


aggregate.ag.prod.irr %>%
  mutate(ag.prod.irr = if_else(sector=="biomass", ag.prod.irr*(1000/17.5), ag.prod.irr)) %>%
  group_by(scenario,gcm,year) %>% summarise(ag.prod.irr=sum(ag.prod.irr)) %>% ungroup() %>%
  group_by(scenario,year) %>% summarise(mean=mean(ag.prod.irr),min=min(ag.prod.irr),max=max(ag.prod.irr)) %>% ungroup() %>%
  mutate(climate = if_else(grepl("2p6",scenario),"RCP 2.6", "RCP 6.0"),
         scenario = gsub("_2p6","",scenario),
         scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High", "Medium", "Low")),
         climate = factor(climate, levels = c("RCP 6.0", "RCP 2.6"))) %>%
  dplyr::filter(year>=2015)  ->
global.irr.prod

ggplot() +
  geom_ribbon(data=global.irr.prod, aes(ymin=min,ymax=max,x=as.numeric(year), fill=scenario,linetype=climate), alpha=0.2) +
  geom_line(data = global.irr.prod,
            aes(x=year,y=mean,linetype=climate,color=scenario),size=1.1) +
  scale_x_continuous(breaks=seq(2020,2100,20), expand = c(0.01,0)) +
  scale_fill_brewer("",palette = "Set1")+
  scale_color_brewer("",palette = "Set1") +
  labs(title="",x="",y="Annual Global Irrigated Crop Production (Mt)")+
  theme_bw() + theme(legend.text = element_text(size=15,face="bold"), legend.key.size = unit(1,"cm"), axis.text = element_text(size=12,face="bold"),strip.text.x = element_text(size=15,face="bold"),axis.title = element_text(size=14,face="bold"), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), plot.title = element_text(hjust=0.5))+
  guides(guide_legend(col=T))
ggsave(filename="global_irrigated_prod.png",width=12,height=10)

aggregate.ag.prod.rfd %>%
  mutate(ag.prod.rfd = if_else(sector=="biomass", ag.prod.rfd*(1000/17.5), ag.prod.rfd)) %>%
  group_by(scenario,gcm,year) %>% summarise(ag.prod.rfd=sum(ag.prod.rfd)) %>% ungroup() %>%
  group_by(scenario,year) %>% summarise(mean=mean(ag.prod.rfd),min=min(ag.prod.rfd),max=max(ag.prod.rfd)) %>% ungroup() %>%
  mutate(climate = if_else(grepl("2p6",scenario),"RCP 2.6", "RCP 6.0"),
         scenario = gsub("_2p6","",scenario),
         scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High", "Medium", "Low")),
         climate = factor(climate, levels = c("RCP 6.0", "RCP 2.6"))) %>%
  dplyr::filter(year>=2015)  ->
  global.rfd.prod

ggplot() +
  geom_ribbon(data=global.rfd.prod, aes(ymin=min,ymax=max,x=as.numeric(year), fill=scenario,linetype=climate), alpha=0.2) +
  geom_line(data = global.rfd.prod,
            aes(x=year,y=mean,linetype=climate,color=scenario),size=1.1) +
  scale_x_continuous(breaks=seq(2020,2100,20), expand = c(0.01,0)) +
  scale_fill_brewer("",palette = "Set1")+
  scale_color_brewer("",palette = "Set1") +
  labs(title="",x="",y="Annual Global Rainfed Crop Production (Mt)")+
  theme_bw() + theme(legend.text = element_text(size=15,face="bold"), legend.key.size = unit(1,"cm"), axis.text = element_text(size=12,face="bold"),strip.text.x = element_text(size=15,face="bold"),axis.title = element_text(size=14,face="bold"), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), plot.title = element_text(hjust=0.5))+
  guides(guide_legend(col=T))
ggsave(filename="global_rainfed_prod.png",width=12,height=10)

aggregate.ag.prod.irr %>%
  mutate(ag.prod.irr = if_else(sector=="biomass", ag.prod.irr*(1000/17.5), ag.prod.irr)) %>%
  group_by(scenario,sector,gcm,year) %>% summarise(ag.prod.irr=sum(ag.prod.irr)) %>% ungroup() %>%
  group_by(scenario,sector,year) %>% summarise(mean=mean(ag.prod.irr),min=min(ag.prod.irr),max=max(ag.prod.irr)) %>% ungroup() %>%
  mutate(climate = if_else(grepl("2p6",scenario),"RCP 2.6", "RCP 6.0"),
         scenario = gsub("_2p6","",scenario),
         scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High", "Medium", "Low")),
         climate = factor(climate, levels = c("RCP 6.0", "RCP 2.6"))) %>%
  dplyr::filter(year>=2015)  ->
  global.irr.prod.crop

ggplot() +
  geom_ribbon(data=global.irr.prod.crop, aes(ymin=min,ymax=max,x=as.numeric(year), fill=scenario,linetype=climate), alpha=0.2) +
  geom_line(data = global.irr.prod.crop,
            aes(x=year,y=mean,linetype=climate,color=scenario),size=1.1) +
  facet_wrap(~sector,scales="free_y") +
  scale_x_continuous(breaks=seq(2020,2100,20), expand = c(0.01,0)) +
  scale_fill_brewer("",palette = "Set1")+
  scale_color_brewer("",palette = "Set1") +
  labs(title="",x="",y="Annual Global Irrigated Crop Production (Mt)")+
  theme_bw() + theme(legend.text = element_text(size=15,face="bold"), legend.key.size = unit(1,"cm"), axis.text = element_text(size=12,face="bold"),strip.text.x = element_text(size=15,face="bold"),axis.title = element_text(size=14,face="bold"), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), plot.title = element_text(hjust=0.5))+
  guides(guide_legend(col=T))
ggsave(filename="global_irrigated_prod_crop.png",width=12,height=10)

aggregate.ag.prod.rfd %>%
  mutate(ag.prod.rfd = if_else(sector=="biomass", ag.prod.rfd*(1000/17.5), ag.prod.rfd)) %>%
  group_by(scenario,sector,gcm,year) %>% summarise(ag.prod.rfd=sum(ag.prod.rfd)) %>% ungroup() %>%
  group_by(scenario,sector,year) %>% summarise(mean=mean(ag.prod.rfd),min=min(ag.prod.rfd),max=max(ag.prod.rfd)) %>% ungroup() %>%
  mutate(climate = if_else(grepl("2p6",scenario),"RCP 2.6", "RCP 6.0"),
         scenario = gsub("_2p6","",scenario),
         scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High", "Medium", "Low")),
         climate = factor(climate, levels = c("RCP 6.0", "RCP 2.6"))) %>%
  dplyr::filter(year>=2015)  ->
  global.rfd.prod.crop

ggplot() +
  geom_ribbon(data=global.rfd.prod.crop, aes(ymin=min,ymax=max,x=as.numeric(year), fill=scenario,linetype=climate), alpha=0.2) +
  geom_line(data = global.rfd.prod.crop,
            aes(x=year,y=mean,linetype=climate,color=scenario),size=1.1) +
  facet_wrap(~sector,scales="free_y") +
  scale_x_continuous(breaks=seq(2020,2100,20), expand = c(0.01,0)) +
  scale_fill_brewer("",palette = "Set1")+
  scale_color_brewer("",palette = "Set1") +
  labs(title="",x="",y="Annual Global Rainfed Crop Production (Mt)")+
  theme_bw() + theme(legend.text = element_text(size=15,face="bold"), legend.key.size = unit(1,"cm"), axis.text = element_text(size=12,face="bold"),strip.text.x = element_text(size=15,face="bold"),axis.title = element_text(size=14,face="bold"), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), plot.title = element_text(hjust=0.5))+
  guides(guide_legend(col=T))
ggsave(filename="global_rainfed_prod_crop.png",width=12,height=10)

ag.prod.irr <- ag.prod %>% dplyr::filter(grepl("IRR",technology)) %>% dplyr::rename(ag.prod.irr=ag.prod) %>%
  separate(subsector, c('subsector',"basin.name"), sep="_") %>%
  group_by(scenario,gcm,sector,basin.name, year) %>% summarise(ag.prod.irr = sum(ag.prod.irr)) ->
  basin.irr.prod

basin.id.swap <- read.csv("/Users/grah436/Desktop/UMD Computer/SSP_Climate/basin_ID_swap.csv",sep=",")

basin.irr.prod %>% ungroup() %>%
  dplyr::filter(year==2020) %>%
  rename(prod.y0=ag.prod.irr) %>%
  dplyr::select(-year) %>%
  left_join(basin.irr.prod %>% dplyr::filter(year==2100),
            by =c("scenario","gcm","sector","basin.name")) %>%
  rename(prod.yn=ag.prod.irr) %>%
  dplyr::select(-year) %>%
  left_join(basin.irr.prod  %>% dplyr::filter(year>2020&year<2100) %>%
              ungroup(), by=c("scenario","gcm","sector","basin.name")) %>%
  group_by(scenario,gcm,sector,basin.name) %>%
  summarise(prod.y0 = sum(prod.y0),
            prod.yn = sum(prod.yn),
            prod = sum(ag.prod.irr)) %>%
  ungroup() %>%
  mutate(cumsum = 5 * (((prod.y0 + prod.yn)/2) + prod)) %>%
  ungroup() %>%
  dplyr::rename(basin.name.old=basin.name) %>%
  left_join(basin.id.swap %>%
              dplyr::rename(basin.name.new=basin.name.old),
            by=c("basin.name.old"="basin.name"))%>%
  mutate(basin.name.new = gsub(" ","_",basin.name.new)) %>%
  replace_na(list(basin.name.new="Madasgacar")) %>%
  na.omit() %>% rename(subRegion=basin.name.new)->
   basin.cum.irr.prod

basin.cum.irr.prod %>%
  mutate(cum.prod = if_else(sector=="biomass", cumsum*(1000/17.5), cumsum)) %>%
  ungroup() %>%
  group_by(scenario,sector,subRegion) %>% summarise(value=mean(cum.prod)) %>% ungroup() %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High","High 2.6","Medium","Medium 2.6",  "Low","Low 2.6")))  ->
  basin.irr.cum

basin.irr.cum %>% dplyr::filter(scenario == "Medium") %>% dplyr::rename(base=value) %>% dplyr::select(-scenario) %>%
  left_join(basin.irr.cum, by=c("sector","subRegion")) %>%
  mutate(value = ((value-base)/base)*100) %>% dplyr::select(-base) ->
  basin.irr.cum.diff

sectors<-unique(basin.irr.cum.diff$sector)

for(i in seq_along(sectors)){
rmap::map(basin.irr.cum.diff %>% dplyr::filter(sector==sectors[i]),
          underLayer = mapGCAMBasins,
          scaleRange = c(-100,400),
          palette = "RdBu",
          legendType =  "pretty",
          legendTitle = ,
          legendBreaksn = 21,
          title= paste0("Irrigated ",sectors[i]),
          legendSingleValue = T,
          width=21,
          height=24,
          ncol=2,
          background = T,
          crop=F,
          show=F) -> b
b[[1]] + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, title=expression(Percent~Difference))) + ggplot2::theme(plot.title = element_text(face="bold",size=20,color="black",hjust=0.5), legend.text = element_text(size=15), legend.position = "right", strip.text.x = element_text(face="bold",size=15,color="black"), strip.background.x = element_blank(),strip.text.y = element_text(face="bold",size=15,color="black"), strip.background.y = element_blank())
  ggsave(filename=paste0("irrigated_prod_",sectors[i],".png"),width=15,height=20)
}


ag.prod.rfd <- ag.prod %>% dplyr::filter(grepl("RFD",technology)) %>% dplyr::rename(ag.prod.rfd=ag.prod) %>%
  separate(subsector, c('subsector',"basin.name"), sep="_") %>%
  group_by(scenario,gcm,sector,basin.name, year) %>% summarise(ag.prod.rfd = sum(ag.prod.rfd)) ->
  basin.rfd.prod

basin.id.swap <- read.csv("/Users/grah436/Desktop/UMD Computer/SSP_Climate/basin_ID_swap.csv",sep=",")

basin.rfd.prod %>% ungroup() %>%
  dplyr::filter(year==2020) %>%
  rename(prod.y0=ag.prod.rfd) %>%
  dplyr::select(-year) %>%
  left_join(basin.rfd.prod %>% dplyr::filter(year==2100),
            by =c("scenario","gcm","sector","basin.name")) %>%
  rename(prod.yn=ag.prod.rfd) %>%
  dplyr::select(-year) %>%
  left_join(basin.rfd.prod  %>% dplyr::filter(year>2020&year<2100) %>%
              ungroup(), by=c("scenario","gcm","sector","basin.name")) %>%
  group_by(scenario,gcm,sector,basin.name) %>%
  summarise(prod.y0 = sum(prod.y0),
            prod.yn = sum(prod.yn),
            prod = sum(ag.prod.rfd)) %>%
  ungroup() %>%
  mutate(cumsum = 5 * (((prod.y0 + prod.yn)/2) + prod)) %>%
  ungroup() %>%
  dplyr::rename(basin.name.old=basin.name) %>%
  left_join(basin.id.swap %>%
              dplyr::rename(basin.name.new=basin.name.old),
            by=c("basin.name.old"="basin.name"))%>%
  mutate(basin.name.new = gsub(" ","_",basin.name.new)) %>%
  replace_na(list(basin.name.new="Madasgacar")) %>%
  na.omit() %>% rename(subRegion=basin.name.new)->
  basin.cum.rfd.prod

basin.cum.rfd.prod %>%
  mutate(cum.prod = if_else(sector=="biomass", cumsum*(1000/17.5), cumsum)) %>%
  ungroup() %>%
  group_by(scenario,sector,subRegion) %>% summarise(value=mean(cum.prod)) %>% ungroup() %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High","High 2.6","Medium","Medium 2.6",  "Low","Low 2.6")))  ->
  basin.rfd.cum

basin.rfd.cum %>% dplyr::filter(scenario == "Medium") %>% dplyr::rename(base=value) %>% dplyr::select(-scenario) %>%
  left_join(basin.rfd.cum, by=c("sector","subRegion")) %>%
  mutate(value = ((value-base)/base)*100) %>% dplyr::select(-base) ->
  basin.rfd.cum.diff

sectors<-unique(basin.rfd.cum.diff$sector)

for(i in seq_along(sectors)){
  rmap::map(basin.rfd.cum.diff %>% dplyr::filter(sector==sectors[i]),
            underLayer = mapGCAMBasins,
            scaleRange = c(-100,400),
            palette = "RdBu",
            legendType =  "pretty",
            legendTitle = ,
            legendBreaksn = 21,
            title= paste0("Rainfed ",sectors[i]),
            legendSingleValue = T,
            width=21,
            height=24,
            ncol=2,
            background = T,
            crop=F,
            show=F) -> b
  b[[1]] + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, title=expression(Percent~Difference))) + ggplot2::theme(plot.title = element_text(face="bold",size=20,color="black",hjust=0.5), legend.text = element_text(size=15), legend.position = "right", strip.text.x = element_text(face="bold",size=15,color="black"), strip.background.x = element_blank(),strip.text.y = element_text(face="bold",size=15,color="black"), strip.background.y = element_blank())
  ggsave(filename=paste0("rainfed_prod_",sectors[i],".png"),width=15,height=20)
}

region_names <- read.csv("/Users/grah436/Desktop/PNNL_Projects/vwt_drivers/new_region_names.csv")
region_color <- c( "E. Africa" = "olivedrab2",
                   "N. Africa" = "darkgreen",
                   "S. Africa" = "green4",
                   "W. Africa" = "olivedrab3",
                   "Argentina" = "mediumpurple",
                   "Australia" = "cornflowerblue",
                   "Brazil" = "mediumpurple4",
                   "Canada" = "chocolate1",
                   "C. America" = "darkorchid4",
                   "C. Asia" = "firebrick1",
                   "China" = "firebrick4",
                   "Colombia" = "magenta4",
                   "EU-12" = "dodgerblue4",
                   "EU-15" = "dodgerblue3",
                   "E. Europe" = "deepskyblue3",
                   "Non-EU" = "cadetblue2",
                   "Euro FTA" = "cadetblue",
                   "India" = "indianred",
                   "Indonesia" = "firebrick2",
                   "Japan" = "deeppink4",
                   "Mexico" = "orchid4",
                   "Middle East" = "darkolivegreen3",
                   "Pakistan" = "indianred4",
                   "Russia" = "red4",
                   "South Africa" = "olivedrab4",
                   "N. S. America" = "purple4",
                   "S. S. America" = "purple2",
                   "S. Asia" = "indianred2",
                   "South Korea" = "deeppink3",
                   "SE Asia" = "red3",
                   "Taiwan" = "deeppink3",
                   "USA" = "chocolate3"
)

order_vwt <- c("E. Africa","N. Africa","S. Africa","W. Africa","Argentina", "Australia","Brazil","Canada","C. America",
               "C. Asia", "China",  "Colombia", "EU-12", "EU-15", "E. Europe","Non-EU", "Euro FTA","India","Indonesia","Japan", "Mexico",
               "Middle East","Pakistan","Russia","South Africa","N. S. America","S. S. America","S. Asia","South Korea","SE Asia",
               "Taiwan","USA")


virtual.blue.water.trade %>% group_by(scenario,gcm,region,year) %>% summarise(vwe=sum(vwe)) %>% filter(year==2100) %>%
  ungroup() %>% group_by(scenario,region) %>% summarise(mean=mean(vwe)) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High","High 2.6","Medium","Medium 2.6",  "Low","Low 2.6"))) %>%
  left_join(region_names %>% dplyr::select(region,new_region), by=c("region")) %>%
  dplyr::select(-region)->
  vbt_2100


vbt_2100 %>%
  ggplot() +
  geom_bar(aes(x=scenario,y=mean,fill=new_region),size=0.25,color="black", stat="identity") +
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values= region_color) +
  labs(x="",y="Total Blue Water Exports (km3)") +
  theme_bw() +theme(axis.text.x = element_text(angle = 90), legend.text = element_text(size=15), legend.key.size = unit(0.75,"cm"), axis.text = element_text(size=15),strip.text.x = element_text(size=10),axis.title = element_text(size=14), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))
ggsave(filename="vbt_barchart_2100.png",width=8,height=10)

virtual.green.water.trade %>% group_by(scenario,gcm,region,year) %>% summarise(vwe=sum(vwe)) %>% filter(year==2100) %>%
  ungroup() %>% group_by(scenario,region) %>% summarise(mean=mean(vwe)) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High","High 2.6","Medium","Medium 2.6",  "Low","Low 2.6"))) %>%
  left_join(region_names %>% dplyr::select(region,new_region), by=c("region")) %>%
  dplyr::select(-region)->
  vgt_2100


vgt_2100 %>%
  ggplot() +
  geom_bar(aes(x=scenario,y=mean,fill=new_region),size=0.25,color="black", stat="identity") +
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values= region_color) +
  labs(x="",y="Total Green Water Exports (km3)") +
  theme_bw() +theme(axis.text.x = element_text(angle = 90), legend.text = element_text(size=15), legend.key.size = unit(0.75,"cm"), axis.text = element_text(size=15),strip.text.x = element_text(size=10),axis.title = element_text(size=14), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))
ggsave(filename="vgt_barchart_2100.png",width=8,height=10)


virtual.ground.water.trade %>% group_by(scenario,gcm,region,year) %>% summarise(vwe=sum(vwe)) %>% filter(year==2100) %>%
  ungroup() %>% group_by(scenario,region) %>% summarise(mean=mean(vwe)) %>%
  mutate(scenario = gsub("High_MI","High",scenario),
         scenario = gsub("HighMI","High",scenario),
         scenario = gsub("Reference","Medium",scenario),
         scenario = gsub("SS","Low",scenario),
         scenario = gsub("_2p6"," 2.6",scenario)) %>%
  mutate(scenario=factor(scenario,levels=c("High","High 2.6","Medium","Medium 2.6",  "Low","Low 2.6"))) %>%
  left_join(region_names %>% dplyr::select(region,new_region), by=c("region")) %>%
  dplyr::select(-region)->
  vgwt_2100


vgwt_2100 %>%
  ggplot() +
  geom_bar(aes(x=scenario,y=mean,fill=new_region),size=0.25,color="black", stat="identity") +
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values= region_color) +
  labs(x="",y="Total Groundwater Exports (km3)") +
  theme_bw() +theme(axis.text.x = element_text(angle = 90), legend.text = element_text(size=15), legend.key.size = unit(0.75,"cm"), axis.text = element_text(size=15),strip.text.x = element_text(size=10),axis.title = element_text(size=14), legend.title = element_blank(), strip.text = element_text(face="bold",size=12), strip.background = element_blank(), legend.position = "right", plot.title = element_text(hjust=0.5))
ggsave(filename="vgwt_barchart_2100.png",width=8,height=10)
