#--------------------------------------------
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readr)
library(ggpubr)
library(grid)
library(gridExtra)
library(extrafont)

filesdir = ""
setwd(filesdir)

#reading technology mapping
tech_mapping <- read_csv("tech_mapping_5.4.csv",skip=1) %>% unique()
tech_mapping_nobio <- read_csv("tech_mapping_5.4_nobio.csv") %>% unique()

state_glu_mapping <- read_csv("state_glu_mapping.csv")
basin_country_mapping <- read_csv("basin_to_country_mapping.csv",skip=7)
LUC_state_mapping <- merge(state_glu_mapping,basin_country_mapping[,c("Basin_long_name","GLU_name","Country_name")],by="Basin_long_name") %>%
  filter(str_length(state_acr)==2)

datadir= ""
setwd(datadir)

#--------------------------------------------
# reading data
#--------------------------------------------
SCENARIO = "NEWdac_netzero_usa_global_ssp2_highEV"   #copy and paste from list below for the scenario you want

#NEWdac_netzero_usa_global_ssp2_highEV                  #scenario a) low LUC + low-cost CCS (main scenario)
#NEWdac_netzero_usa_global_ssp2_highEV_lowCCS           #scenario b) low LUC + high cost CCS
#NEWdac_netzero_usa_global_ssp2_highEV_luc2050          #scenario c) high LUC + low-cost CCS
#NEWdac_netzero_usa_global_ssp2_highEV_lowCCS_luc2050   #scenario d) high LUC + high cost CCS


NEWdac_highEV_co2 <- read_csv(paste0(SCENARIO,"/queryout_CO2_emissions_by_tech_nobio_states.csv"))
NEWdac_highEV_co2_seq <- read_csv(paste0(SCENARIO,"/queryout_CO2_sequestration_by_tech_states.csv"), skip = 1)
NEWdac_highEV_luc <- read_csv(paste0(SCENARIO,"/queryout_LUC_emissions_by_LUT_states.csv"), skip = 1)

NEWdac_highEV_detailed_luc <- read_csv(paste0(SCENARIO,"/queryout_detailed_land_allocation_states.csv"), skip = 1)
NEWdac_highEV_fertilizer <- read_csv(paste0(SCENARIO,"/queryout_fertilizer_consumption_by_ag_tech_states.csv"), skip = 1)
NEWdac_highEV_water <- read_csv(paste0(SCENARIO,"/queryout_water_withdrawals_by_tech_states.csv"), skip = 1)
NEWdac_highEV_energy <- read_csv(paste0(SCENARIO,"/queryout_final_energy_consumption_by_sector_and_fuel_states.csv"), skip = 1)

NEWdac_co2_price <- read_csv(paste0(SCENARIO,"/queryout_CO2_prices_global.csv"), skip = 1)

#--------------------------------------------
# cleaning CO2 emissions by tech
#--------------------------------------------
usa_tech_yr <- NEWdac_highEV_co2 %>%
  left_join(tech_mapping_nobio, by = 'sector') %>%
  group_by(scenario, Year, region, tech_map) %>% 
  summarise(netvalue = sum(co2.emiss)) %>%
  mutate(Units="MTC") %>%
  rename(year=Year) %>%
  mutate(tech_map=if_else((tech_map=="Bioliquids" & netvalue>=0),"Refining",tech_map)) %>%
  mutate(tech_map=if_else((tech_map=="Bioliquids" & netvalue<0),"BECCS liquids",tech_map))

#--------------------------------------------
# cleaning CO2 sequestration by tech
#--------------------------------------------
bio_seq <- NEWdac_highEV_co2_seq %>%
  gather(year, value, -c(scenario, region, sector, subsector,technology,Units)) %>%
  mutate(scenario = SCENARIO) %>%
  mutate(year=as.numeric(year)) %>%
  left_join(tech_mapping %>% select(sector,technology,tech_map), by = c('sector','technology')) %>%
  filter(tech_map == "BECCS electricity") %>%
  mutate(value = -value) %>%
  group_by(scenario, year, region, Units, tech_map) %>% 
  summarise(netvalue = sum(value))

bioELEC <- bio_seq %>%
  filter(tech_map=="BECCS electricity") %>%
  mutate(netvalue = -netvalue) %>%
  mutate(tech_map="Electricity")

total_ccs_states_2050 <- NEWdac_highEV_co2_seq %>%
  gather(year, value, -c(scenario, region, sector, subsector,technology,Units)) %>%
  mutate(scenario = SCENARIO) %>%
  mutate(year=as.numeric(year)) %>%
  filter(year == 2050) %>%
  filter(!str_detect(sector,"feedstock")) %>%
  group_by(scenario,region,year,Units) %>%
  summarise(totalCCS=(44/12)*sum(value)) %>%
  mutate(Units="MtCO2")

#--------------------------------------------
# cleaning LUC by LUT
#--------------------------------------------
state_basins <- paste(as.vector(unlist(unique(LUC_state_mapping$GLU_name))),collapse="|")
state_basins_list <- unique(LUC_state_mapping$GLU_name)

NEWdac_highEV_luc_1 <- NEWdac_highEV_luc %>%
  filter(region=="Global") %>%
  filter(str_detect(LandLeaf,state_basins)) %>%
  gather(year, value, -c(scenario, region, LandLeaf,Units)) %>%
  mutate(scenario = SCENARIO) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(Units="MTC") %>%
  mutate(GLU_name = "")

for (i in state_basins_list) {
  NEWdac_highEV_luc_1 <- NEWdac_highEV_luc_1 %>% mutate(GLU_name = if_else(str_detect(LandLeaf, i),i,GLU_name))
}

usa_luc <- NEWdac_highEV_luc_1 %>%
  group_by(scenario, year, GLU_name, Units) %>%
  summarise(value=sum(value))

usa_luc_yr <- merge(usa_luc,LUC_state_mapping[,c("GLU_name","state_acr","glu_proportion")],by="GLU_name") %>%
  mutate(stateLUC=value*glu_proportion) %>%
  rename(region=state_acr) %>%
  mutate(tech_map="Land-use change") %>%
  group_by(scenario, year, region, Units,tech_map) %>%
  summarise(netvalue=sum(stateLUC))

#---------------------------------------------
## cleaning detailed land allocation
#---------------------------------------------
NEWdac_highEV_detailed_luc_1 <- NEWdac_highEV_detailed_luc %>%
  filter(region=="Global") %>%
  filter(str_detect(LandLeaf,state_basins)) %>%
  gather(year, value, -c(scenario, region, LandLeaf,Units)) %>%
  mutate(scenario = SCENARIO) %>%
  mutate(year=as.numeric(year)) %>%
  filter(year>=2015, year<=2050) %>%
  mutate(Landuse=str_extract(LandLeaf,"[^_]+")) %>%
  mutate(Landtype="") %>%
  mutate(Landtype=if_else(Landuse %in% c("biomassGrass","biomassTree"),"biomass",Landtype)) %>%
  mutate(Landtype=if_else(Landuse %in% c("Corn","FiberCrop","FodderGrass","FodderHerb","MiscCrop","OilCrop",
                                         "OtherGrain","PalmFruit","Rice","RootTuber","SugarCrop","Wheat"),"crops",Landtype)) %>%
  mutate(Landtype=if_else(Landuse %in% c("Forest","ProtectedUnmanagedForest","UnmanagedForest"),"forest",Landtype)) %>%
  mutate(Landtype=if_else(Landuse %in% c("Grassland","ProtectedGrassland"),"grass",Landtype)) %>%
  mutate(Landtype=if_else(Landuse %in% c("OtherArableLand"),"other arable land",Landtype)) %>%
  mutate(Landtype=if_else(Landuse %in% c("Pasture","ProtectedUnmanagedPasture","UnmanagedPasture"),"pasture",Landtype)) %>%
  mutate(Landtype=if_else(Landuse %in% c("RockIceDesert"),"rock and desert",Landtype)) %>%
  mutate(Landtype=if_else(Landuse %in% c("ProtectedShrubland","Shrubland"),"shrubs",Landtype)) %>%
  mutate(Landtype=if_else(Landuse %in% c("Tundra"),"tundra",Landtype)) %>%
  mutate(Landtype=if_else(Landuse %in% c("UrbanLand"),"urban",Landtype)) %>%
  mutate(GLU_name = "") %>%
  mutate(GLU_name=sub("^[^_]*_", "", LandLeaf)) %>%
  mutate(GLU_name=sub("\\_.*", "", GLU_name))

usa_luc_area <- NEWdac_highEV_detailed_luc_1 %>%
  group_by(scenario, year, GLU_name, Landtype, Units) %>%
  summarise(value=sum(value))

usa_luc_state_area <- merge(usa_luc_area,LUC_state_mapping[,c("GLU_name","state_acr","glu_proportion")],by="GLU_name") %>%
  mutate(stateLUCarea=value*glu_proportion) %>%
  rename(region=state_acr) %>%
  group_by(scenario, year, region, Landtype, Units) %>%
  summarise(netvalue=sum(stateLUCarea)) %>%
  left_join(., state_glu_mapping %>% select(state_acr,state_area) %>% unique() %>% na.omit(),by=c("region"="state_acr")) %>%
  mutate(state_area=state_area/(1000^3)) %>%
  mutate(percentchange=100*(netvalue/state_area)) %>%
  filter(Landtype!="tundra")

#---------------------------------------------
## cleaning fertilizer use
#---------------------------------------------
NEWdac_highEV_fertilizer_1 <- NEWdac_highEV_fertilizer %>%
  filter(region=="Global", sector!="N fertilizer") %>%
  select(-input) %>%
  filter(str_detect(subsector,state_basins)) %>%
  gather(year, value, -c(scenario, region, sector, subsector, technology, Units)) %>%
  mutate(scenario = SCENARIO) %>%
  mutate(year=as.numeric(year)) %>%
  filter(year>=2015, year<=2050) %>%
  mutate(Landtype="") %>%
  mutate(Landtype=if_else(sector=="biomass","biomass",Landtype)) %>%
  mutate(Landtype=if_else(sector %in% c("Corn","FiberCrop","FodderGrass","FodderHerb","MiscCrop","OilCrop",
                                         "OtherGrain","PalmFruit","Rice","RootTuber","SugarCrop","Wheat"),"crops",Landtype)) %>%
  mutate(GLU_name=sub("^[^_]*_", "", subsector))

usa_fertilizer_use <- NEWdac_highEV_fertilizer_1 %>%
  group_by(scenario, year, GLU_name, Landtype, Units) %>%
  summarise(value=sum(value))

usa_state_fertilizer_use <- merge(usa_fertilizer_use,LUC_state_mapping[,c("GLU_name","state_acr","glu_proportion")],by="GLU_name") %>%
  mutate(statevalue=value*glu_proportion) %>%
  rename(region=state_acr) %>%
  group_by(scenario, year, region, Landtype, Units) %>%
  summarise(netvalue=sum(statevalue)) %>% ungroup() %>%
  group_by(year,region) %>%
  mutate(total=sum(netvalue), percent=netvalue/total)
  
#---------------------------------------------
## cleaning water use
#---------------------------------------------
NEWdac_highEV_water_ag <- NEWdac_highEV_water %>%
  gather(year, value, -c(scenario, region, sector, subsector, technology, Units)) %>%
  mutate(scenario = SCENARIO) %>%
  mutate(year=as.numeric(year)) %>%
  filter(year>=2015, year<=2050, region=="USA",grepl("IRR",technology)) %>%
  mutate(GLU_name=sub("^[^_]*_", "", subsector)) %>% 
  mutate(sector=if_else(sector=="biomass","biomass",sector)) %>%
  mutate(sector=if_else(sector %in% c("Corn","FiberCrop","FodderGrass","FodderHerb","MiscCrop","OilCrop",
                                        "OtherGrain","PalmFruit","Rice","RootTuber","SugarCrop","Wheat"),"crops",sector)) %>%
  group_by(scenario, year, GLU_name, sector, Units) %>%
  summarise(value=sum(value))

usa_water_ag <- merge(NEWdac_highEV_water_ag,LUC_state_mapping[,c("GLU_name","state_acr","glu_proportion")],by="GLU_name") %>%
  mutate(state_water=value*glu_proportion) %>%
  rename(region=state_acr) %>%
  group_by(scenario, year, region, sector, Units) %>%
  summarise(netvalue=sum(state_water))

NEWdac_highEV_water_other <- NEWdac_highEV_water %>%
  gather(year, value, -c(scenario, region, sector, subsector, technology, Units)) %>%
  select(-c(subsector,technology)) %>%
  mutate(scenario = SCENARIO) %>%
  mutate(year=as.numeric(year)) %>%
  filter(year>=2015, year<=2050, region!="USA") %>%
  rename(netvalue=value) 

usa_water <- usa_water_ag %>% bind_rows(NEWdac_highEV_water_other) %>%
  group_by(year,region) %>%
  mutate(total=sum(netvalue), percent=netvalue/total) %>%
  arrange(year,region)

#---------------------------------------------
## cleaning energy use
#---------------------------------------------
usa_dac_elec <- NEWdac_highEV_energy %>%
  gather(year, value, -c(scenario, region, sector, input, Units)) %>%
  mutate(scenario=SCENARIO,year=as.numeric(year)) %>%
  filter(year>=2015, year<=2050,input=="electricity") %>%
  mutate(sector=if_else(sector=="CO2 removal","DACCS","other")) %>%
  group_by(scenario, year, region, sector, Units) %>%
  summarise(netvalue=sum(value)) %>% ungroup() %>%
  group_by(year,region) %>%
  mutate(total=sum(netvalue), percent=netvalue/total)

usa_dac_NG <- NEWdac_highEV_energy %>%
  gather(year, value, -c(scenario, region, sector, input, Units)) %>%
  mutate(scenario=SCENARIO,year=as.numeric(year)) %>%
  filter(year>=2015, year<=2050,input %in% c("gas","process heat dac")) %>%
  mutate(sector=if_else(sector=="CO2 removal","DACCS","other")) %>%
  group_by(scenario, year, region, sector, Units) %>%
  summarise(netvalue=sum(value)) %>% ungroup() %>%
  group_by(year,region) %>%
  mutate(total=sum(netvalue), percent=netvalue/total)

#--------------------------------------------
# Combining dataframes into one & saving excel data for other figures
#--------------------------------------------
usa_co2_df <- usa_tech_yr %>% bind_rows(bio_seq,bioELEC, usa_luc_yr) %>%
  filter(year>=2005) %>%
  filter(year<=2050) %>%
  filter(str_length(region)==2) %>%
  group_by(scenario, year, region, Units, tech_map) %>% 
  summarise(netvalue = sum(netvalue)) %>%
  mutate(netvalue=(44/12)*netvalue) %>%     #converting units from MtC to MtCO2
  mutate(Units="MtCO2")

pos_emiss_state_2050 <- usa_co2_df %>% filter(year==2050, netvalue>=0) %>% group_by(scenario,year,region,Units) %>% summarise(netvalue=sum(netvalue))
neg_emiss_states_2050 <- usa_co2_df %>% filter(year==2050, netvalue<0) %>% group_by(scenario,year,region,Units) %>% summarise(netvalue=sum(netvalue))
net_emiss_states_2050 <- usa_co2_df %>% filter(year==2050) %>% group_by(scenario,year,region,Units) %>% summarise(netvalue=sum(netvalue))
net_emiss_sectors_2050 <- usa_co2_df %>% filter(year==2050) %>% group_by(scenario,year,tech_map,Units) %>% summarise(netvalue=sum(netvalue))

biomassLUC <- usa_luc_state_area %>% filter(year==2050, Landtype=="biomass")
bio_fertilizer <- usa_state_fertilizer_use %>% filter(year==2050,Landtype=="biomass")
bio_water <- usa_water %>% filter(year==2050, sector=="biomass")
usa_dac_elec_1 <- usa_dac_elec %>% filter(year==2050, sector=="DACCS")
usa_dac_NG_1 <- usa_dac_NG %>% filter(year==2050, sector=="DACCS")

resultsdir = ""
setwd(paste0(resultsdir,SCENARIO))

xlsx::write.xlsx(as.data.frame(usa_co2_df),sheetName = "states & sectors all years",file="results_usa_ssp2.xlsx",append=FALSE)
xlsx::write.xlsx(as.data.frame(net_emiss_states_2050),sheetName = "states 2050",file="results_usa_ssp2.xlsx",append=TRUE)
xlsx::write.xlsx(as.data.frame(net_emiss_sectors_2050),sheetName = "sectors 2050",file="results_usa_ssp2.xlsx",append=TRUE)
xlsx::write.xlsx(as.data.frame(pos_emiss_state_2050),sheetName = "pos states 2050",file="results_usa_ssp2.xlsx",append=TRUE)
xlsx::write.xlsx(as.data.frame(neg_emiss_states_2050),sheetName = "neg states 2050",file="results_usa_ssp2.xlsx",append=TRUE)

xlsx::write.xlsx(as.data.frame(total_ccs_states_2050),file="total_ccs_usa_ssp2.xlsx",append=FALSE)

xlsx::write.xlsx(as.data.frame(biomassLUC),file="luc_usa_ssp2.xlsx",sheetName = "biomass % change",append=FALSE)
xlsx::write.xlsx(as.data.frame(bio_fertilizer),file="fertilizer_usa_ssp2.xlsx",sheetName = "biomass fertilizer use",append=FALSE)
xlsx::write.xlsx(as.data.frame(bio_water),file="water_usa_ssp2.xlsx",sheetName = "biomass water use",append=FALSE)
xlsx::write.xlsx(as.data.frame(usa_dac_elec_1),file="dac_energy_usa_ssp2.xlsx",sheetName = "DAC electricity",append=FALSE)
xlsx::write.xlsx(as.data.frame(usa_dac_NG_1),file="dac_energy_usa_ssp2.xlsx",sheetName = "DAC NG",append=TRUE)

#-------------------------------------------------------------------------------
## Figure 1: USA + IA + CA + TX sectoral emissions breakdown
#-------------------------------------------------------------------------------
colors <- c("black", "burlywood4","burlywood3","darkorange3", "yellow3", "darkgreen",
            "darkblue", "aquamarine4",  "aquamarine3")
names(colors) <- c("Buildings","Industry", "Refining","Transportation", "Electricity",
                   "Land-use change", "DACCS", "BECCS electricity","BECCS liquids")

usa_co2_df$year <- factor(usa_co2_df$year)
usa_co2_df$tech_map <- factor(usa_co2_df$tech_map,
                              levels=c("Buildings","Industry","Refining","Transportation","Electricity",
                                       "Land-use change", "DACCS", "BECCS electricity","BECCS liquids"))

usa_co2_df_total <- usa_co2_df %>% group_by(scenario,year,tech_map,Units) %>% summarise(netvalue=sum(netvalue)/1000) %>% mutate(Units="GtCO2")
usa_co2_df_IA <- usa_co2_df %>% filter(region=="IA")
usa_co2_df_CA <- usa_co2_df %>% filter(region=="CA")
usa_co2_df_TX <- usa_co2_df %>% filter(region=="TX")

TITLE_SIZE=14
TEXT_SIZE=8
FONT="Calibri"

## USA + IA + CA + TX Plot
USA <- ggplot() +
  geom_bar(data = usa_co2_df_total[usa_co2_df_total$tech_map %in% c("Buildings","Industry", "Refining","Transportation", "Electricity"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = F), stat="identity") +
  geom_bar(data = usa_co2_df_total[usa_co2_df_total$tech_map %in% c("Land-use change", "DACCS", "BECCS electricity","BECCS liquids"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = T), stat="identity") + 
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT),plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.title = element_blank(), legend.text=element_text(size=TEXT_SIZE,family=FONT),
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  xlab("Year") + labs(tag="a") +
  ggtitle("USA") +
  scale_x_discrete(breaks = c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050)) +
  scale_fill_manual(values=colors)

y_label_USA <- grid::textGrob(expression("Positive and Negative "*CO[2]*" emissions ("*GtCO[2]~year^{-1}*")"), rot = 90,gp=gpar(fontsize=TEXT_SIZE))
legend_USA <- get_legend(USA)
USA <- USA + theme(legend.position="none")
top <- grid.arrange(USA,nrow=1,left = y_label_USA, right=legend_USA)

IA <- ggplot() +
  geom_bar(data = usa_co2_df_IA[usa_co2_df_IA$tech_map %in% c("Buildings","Industry", "Refining","Transportation", "Electricity"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = F), stat="identity") +
  geom_bar(data = usa_co2_df_IA[usa_co2_df_IA$tech_map %in% c("Land-use change", "DACCS", "BECCS electricity","BECCS liquids"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = T), stat="identity") + 
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT),plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.title = element_blank(), legend.text=element_text(size=TEXT_SIZE,family=FONT),
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  ylim(-100,100) +
  xlab("Year") +
  labs(tag="b") +
  ggtitle("IA") +
  scale_x_discrete(breaks = c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050)) +
  scale_fill_manual(values=colors)

legend <- get_legend(IA)
IA <- IA + theme(legend.position="none")

CA <- ggplot() +
  geom_bar(data = usa_co2_df_CA[usa_co2_df_CA$tech_map %in% c("Buildings","Industry", "Refining","Transportation", "Electricity"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = F), stat="identity") +
  geom_bar(data = usa_co2_df_CA[usa_co2_df_CA$tech_map %in% c("Land-use change", "DACCS", "BECCS electricity","BECCS liquids"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = T), stat="identity") + 
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT),plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.position="none",
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  ylim(-400,400) +
  xlab("Year") +
  labs(tag="c") +
  ggtitle("CA") +
  scale_x_discrete(breaks = c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050)) +
  scale_fill_manual(values=colors)

TX <- ggplot() +
  geom_bar(data = usa_co2_df_TX[usa_co2_df_TX$tech_map %in% c("Buildings","Industry", "Refining","Transportation", "Electricity"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = F), stat="identity") +
  geom_bar(data = usa_co2_df_TX[usa_co2_df_TX$tech_map %in% c("Land-use change", "DACCS", "BECCS electricity","BECCS liquids"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = T), stat="identity") + 
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT),plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.position="none",
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  ylim(-700,700) +
  xlab("Year") +
  labs(tag="d") +
  ggtitle("TX") +
  scale_x_discrete(breaks = c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050)) +
  scale_fill_manual(values=colors)

y_label_IAVATX <- grid::textGrob(expression("Positive and Negative "*CO[2]*" emissions ("*MtCO[2]~year^{-1}*")"), rot = 90, gp=gpar(fontsize=TEXT_SIZE))
bottom <- grid.arrange(IA,CA,TX,nrow=1,left = y_label_IAVATX, right=legend)

tiff(filename="NEWdac_netzero_usa_global_ssp2_highEV_USAIACATX.png",units='mm',width=180,height=220,res=300)
grid.arrange(top,bottom)
dev.off()

#-------------------------------------------------------------------------------
## Figure 3: Comparing state NETS
#-------------------------------------------------------------------------------
## Total CDR
total_cdr_states_2050 <- usa_co2_df %>% filter(year==2050, tech_map %in% c("BECCS electricity","BECCS liquids","DACCS","Land-use change")) %>% 
  group_by(scenario,year,region,Units) %>%
  mutate(totalnet=sum(netvalue))

totalCDR <- ggplot(total_cdr_states_2050) +
  geom_bar(aes(x=reorder(region,totalnet),y=netvalue,fill=factor(tech_map,levels=c("Land-use change","BECCS liquids","BECCS electricity","DACCS"))), 
           position='stack',stat='identity',show.legend = FALSE) +
  scale_fill_manual(values=colors) +
  ggtitle("Total CDR by state in 2050") +
  xlab("State") + labs(tag="a") +
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT), plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT), axis.text.x = element_text(size=TEXT_SIZE,family=FONT,angle = 90, vjust = 0.5, hjust=1),
        axis.title.y=element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT))

y_label_CDR <- grid::textGrob(expression(CO[2]*" emissions ("*MtCO[2]~year^{-1}*")"), rot = 90, gp=gpar(fontsize=TEXT_SIZE))
top <- grid.arrange(totalCDR, nrow = 1,
             left = y_label_CDR)

## NETs breakdown
percentCDR <- ggplot(total_cdr_states_2050 %>% filter(netvalue<0) %>% mutate(netvalue=-1*netvalue)) +
  geom_bar(aes(x=reorder(region,totalnet),y=netvalue,fill=factor(tech_map,levels=c("Land-use change","BECCS liquids","BECCS electricity","DACCS"))), 
           position='fill',stat='identity',show.legend = FALSE) +
  scale_fill_manual(values=colors) +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("NETs share by state in 2050") +
  xlab("State") + labs(tag="b") +
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT),plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT), axis.text.x = element_text(size=TEXT_SIZE,family=FONT,angle = 90, vjust = 0.5, hjust=1),
        axis.title.y=element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT))

y_label_CDR2 <- grid::textGrob(expression("Percent share of negative "*CO[2]*" emissions"), rot = 90, gp=gpar(fontsize=TEXT_SIZE))
middle <- grid.arrange(percentCDR, nrow = 1,
             left = y_label_CDR2)

## DAC
DAC <- ggplot(usa_co2_df %>% filter(year==2050) %>% filter(tech_map=="DACCS") %>% arrange(netvalue) %>% head(10)) +
  geom_bar(aes(x=reorder(region,netvalue),y=netvalue),fill="darkblue", stat='identity',show.legend = FALSE) +
  #ylim(-50,0) +
  ggtitle("DACCS") +
  xlab("State") + 
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT),plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT), axis.text.x = element_text(size=TEXT_SIZE,family=FONT),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  labs(tag="c")

## BECCS electricity
BECCSeleq <- ggplot(usa_co2_df %>% filter(year==2050) %>% filter(tech_map=="BECCS electricity") %>% arrange(netvalue) %>% head(10)) +
  geom_bar(aes(x=reorder(region,netvalue),y=netvalue),fill="aquamarine4", stat='identity',show.legend = FALSE) +
  ylim(-60,0) +
  ggtitle("BECCS electricity") +
  xlab("State") +
  theme(plot.title = element_text(size=TITLE_SIZE, family=FONT),plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT), axis.text.x = element_text(size=TEXT_SIZE,family=FONT),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  labs(tag="d")

## LUC
LUC <- ggplot(usa_co2_df %>% filter(year==2050) %>% filter(tech_map=="Land-use change") %>% arrange(netvalue) %>% filter(netvalue<=0) %>% head(10)) +
  geom_bar(aes(x=reorder(region,netvalue),y=netvalue), stat='identity', fill="darkgreen", show.legend = FALSE) +
  ylim(-60,0) +
  ggtitle("Land-use change") +
  xlab("State") + 
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT),plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT), axis.text.x = element_text(size=TEXT_SIZE,family=FONT),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  labs(tag="e")

## BECCS liquids
BECCSliq <- ggplot(usa_co2_df %>% filter(year==2050) %>% filter(tech_map=="BECCS liquids") %>% arrange(netvalue) %>% head(10)) +
  geom_bar(aes(x=reorder(region,netvalue),y=netvalue),fill="aquamarine3", stat='identity',show.legend = FALSE) +
  ylim(-60,0) +
  ggtitle("BECCS liquids") +
  xlab("State") + 
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT),plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT), axis.text.x = element_text(size=TEXT_SIZE,family=FONT),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  labs(tag="f")

y_label <- grid::textGrob(expression("Negative "*CO[2]*" emissions ("*MtCO[2]~year^{-1}*")"), rot = 90, gp=gpar(fontsize=TEXT_SIZE))
bottom <- grid.arrange(DAC, BECCSeleq, LUC, BECCSliq, nrow = 2,
             left = y_label)

tiff(filename="NEWdac_netzero_usa_global_ssp2_highEV_NETs.png",units='mm',width=180,height=240,res=300)
grid.arrange(top,middle,bottom)
dev.off()

#-------------------------------------------------------------------------------
## Figure 6: USA sectoral emissions, sensitivity analysis
#-------------------------------------------------------------------------------
# This one is a bit tedious. You will have to run through all the data cleaning for each scenario and create the unique 
# 'USA1', 'USA2', etc. objects below before being able to combine all 4 scenarios on one plot

colors <- c("black", "burlywood4","burlywood3","darkorange3", "yellow3", "darkgreen",
            "darkblue", "aquamarine4",  "aquamarine3")
names(colors) <- c("Buildings","Industry", "Refining","Transportation", "Electricity",
                   "Land-use change", "DACCS", "BECCS electricity","BECCS liquids")

usa_co2_df$year <- factor(usa_co2_df$year)
usa_co2_df$tech_map <- factor(usa_co2_df$tech_map,
                              levels=c("Buildings","Industry","Refining","Transportation","Electricity",
                                       "Land-use change", "DACCS", "BECCS electricity","BECCS liquids"))

usa_co2_df_total <- usa_co2_df %>% group_by(scenario,year,tech_map,Units) %>% summarise(netvalue=sum(netvalue)/1000) %>% mutate(Units="GtCO2")


TITLE_SIZE=12
TEXT_SIZE=8
FONT="Calibri"

#scenario a) low LUC + low-cost CCS (main scenario)
USA1 <- ggplot() +
  geom_bar(data = usa_co2_df_total[usa_co2_df_total$tech_map %in% c("Buildings","Industry", "Refining","Transportation", "Electricity"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = F), stat="identity") +
  geom_bar(data = usa_co2_df_total[usa_co2_df_total$tech_map %in% c("Land-use change", "DACCS", "BECCS electricity","BECCS liquids"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = T), stat="identity") + 
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT), plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.title = element_blank(), legend.text=element_text(size=TEXT_SIZE,family=FONT),
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  ylim(-3,6) +
  xlab("Year") + labs(tag="a") +
  scale_x_discrete(breaks = c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050)) +
  scale_fill_manual(values=colors)

#scenario b) low LUC + high cost CCS
USA2 <- ggplot() +
  geom_bar(data = usa_co2_df_total[usa_co2_df_total$tech_map %in% c("Buildings","Industry", "Refining","Transportation", "Electricity"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = F), stat="identity") +
  geom_bar(data = usa_co2_df_total[usa_co2_df_total$tech_map %in% c("Land-use change", "DACCS", "BECCS electricity","BECCS liquids"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = T), stat="identity") + 
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT), plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.position="none",
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  ylim(-3,6) +
  xlab("Year") + labs(tag="b") +
  scale_x_discrete(breaks = c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050)) +
  scale_fill_manual(values=colors)

y_label_USA <- grid::textGrob(expression("Positive and Negative "*CO[2]*" emissions ("*MtCO[2]~year^{-1}*")"), rot = 90,gp=gpar(fontsize=TEXT_SIZE))
legend_USA <- get_legend(USA1)
USA1 <- USA1 + theme(legend.position="none")

top <- grid.arrange(USA1, USA2, nrow=1,left = y_label_USA)


#scenario c) high LUC + low-cost CCS
USA3 <- ggplot() +
  geom_bar(data = usa_co2_df_total[usa_co2_df_total$tech_map %in% c("Buildings","Industry", "Refining","Transportation", "Electricity"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = F), stat="identity") +
  geom_bar(data = usa_co2_df_total[usa_co2_df_total$tech_map %in% c("Land-use change", "DACCS", "BECCS electricity","BECCS liquids"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = T), stat="identity") + 
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT), plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.position="none",
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  ylim(-3,6) +
  xlab("Year") + labs(tag="c") +
  scale_x_discrete(breaks = c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050)) +
  scale_fill_manual(values=colors)


#scenario d) high LUC + high-cost CCS
USA4 <- ggplot() +
  geom_bar(data = usa_co2_df_total[usa_co2_df_total$tech_map %in% c("Buildings","Industry", "Refining","Transportation", "Electricity"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = F), stat="identity") +
  geom_bar(data = usa_co2_df_total[usa_co2_df_total$tech_map %in% c("Land-use change", "DACCS", "BECCS electricity","BECCS liquids"),],
           aes(x=year,y=netvalue, fill=tech_map),
           position=position_stack(reverse = T), stat="identity") + 
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT), plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.position="none",
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  ylim(-3,6) + 
  xlab("Year") + labs(tag="d") +
  scale_x_discrete(breaks = c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050)) +
  scale_fill_manual(values=colors)

bottom <- grid.arrange(USA3, USA4, nrow=1,left = y_label_USA)

tiff(filename="NEWdac_luc_vs_ccs_sensitivity.png",units='mm',width=180,height=220,res=300)
grid.arrange(top,bottom,nrow=2,right=legend_USA)
dev.off()

#-------------------------------------------------------------------------------
# SI Figure 5: Comparing NETs between scenarios
#-------------------------------------------------------------------------------  
# Same as before. This one is a bit tedious. You will have to run through all the data cleaning for each scenario and create the unique 
# 'USA1', 'USA2', etc. objects before being able to combine all 4 scenarios on one plot.

USA_sectors_1 <- usa_co2_df1 %>% group_by(scenario,year,tech_map,Units) %>% summarise(netvalue=sum(netvalue))  #scenario a) low LUC + low-cost CCS
USA_sectors_2 <- usa_co2_df2 %>% group_by(scenario,year,tech_map,Units) %>% summarise(netvalue=sum(netvalue))  #scenario b) low LUC + high-cost CCS
USA_sectors_3 <- usa_co2_df3 %>% group_by(scenario,year,tech_map,Units) %>% summarise(netvalue=sum(netvalue))  #scenario c) high LUC + low-cost CCS
USA_sectors_4 <- usa_co2_df4 %>% group_by(scenario,year,tech_map,Units) %>% summarise(netvalue=sum(netvalue))  #scenario d) high LUC + high-cost CCS

TITLE_SIZE=12
TEXT_SIZE=8
FONT="Calibri"

#DACCS
DACCS2_data <- USA_sectors_1[USA_sectors_1$tech_map=="DACCS",] %>%
  rbind(.,USA_sectors_2[USA_sectors_2$tech_map=="DACCS",]) %>%
  rbind(.,USA_sectors_3[USA_sectors_3$tech_map=="DACCS",]) %>%
  rbind(.,USA_sectors_4[USA_sectors_4$tech_map=="DACCS",])
  
DACCS2 <- ggplot(data=DACCS2_data, aes(x=year,y=netvalue,color=scenario)) + 
  geom_line(size=1) +
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT), plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.text=element_text(size=TEXT_SIZE,family=FONT),
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  scale_colour_discrete(breaks=c("NEWdac_netzero_usa_global_ssp2_highEV",                   #scenario a) low LUC + low-cost CCS
                                 "NEWdac_netzero_usa_global_ssp2_highEV_lowCCS",            #scenario b) low LUC + high-cost CCS
                               "NEWdac_netzero_usa_global_ssp2_highEV_luc2050",             #scenario c) high LUC + low-cost CCS
                               "NEWdac_netzero_usa_global_ssp2_highEV_lowCCS_luc2050"),     #scenario d) high LUC + high-cost CCS
                      labels=c("Slow LUC/ Low-cost CCS",        #scenario a) low LUC + low-cost CCS
                               "Slow LUC/ High-cost CCS",       #scenario b) low LUC + high-cost CCS
                               "High LUC/ Low-cost CCS",        #scenario c) high LUC + low-cost CCS
                               "High LUC/ High-cost CCS")) +    #scenario d) high LUC + high-cost CCS
  xlab("Year") + labs(tag="a") +
  ggtitle("DACCS")


#BECCS elec
BECCSelec2_data <- USA_sectors_1[USA_sectors_1$tech_map=="BECCS electricity",] %>%
  rbind(.,USA_sectors_2[USA_sectors_2$tech_map=="BECCS electricity",]) %>%
  rbind(.,USA_sectors_3[USA_sectors_3$tech_map=="BECCS electricity",]) %>%
  rbind(.,USA_sectors_4[USA_sectors_4$tech_map=="BECCS electricity",])
  
BECCSeleq2 <- ggplot(data=BECCSelec2_data, aes(x=year,y=netvalue,color=scenario)) + 
  geom_line(size=1) +
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT), plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.position="none",
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  xlab("Year") + labs(tag="b") +
  ggtitle("BECCS electricity")

y_label_NETs <- grid::textGrob(expression("Negative "*CO[2]*" emissions ("*MtCO[2]~year^{-1}*")"), rot = 90,gp=gpar(fontsize=TEXT_SIZE))
legend_DACCS <- get_legend(DACCS2)
DACCS2 <- DACCS2 + theme(legend.position="none")

top <- grid.arrange(DACCS2, BECCSeleq2, nrow=1,left = y_label_NETs)


#LUC
LUC2_data <- USA_sectors_1[USA_sectors_1$tech_map=="Land-use change",] %>%
  rbind(.,USA_sectors_2[USA_sectors_2$tech_map=="Land-use change",]) %>%
  rbind(.,USA_sectors_3[USA_sectors_3$tech_map=="Land-use change",]) %>%
  rbind(.,USA_sectors_4[USA_sectors_4$tech_map=="Land-use change",])

LUC2 <- ggplot(data=LUC2_data, aes(x=year,y=netvalue,color=scenario)) + 
  geom_line(size=1) +
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT), plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.position="none",
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  xlab("Year") + labs(tag="c") +
  ggtitle("Land-use change")  


#BECCS liquid
BECCSliq2_data <- USA_sectors_1[USA_sectors_1$tech_map=="BECCS liquids",] %>%
  rbind(.,USA_sectors_2[USA_sectors_2$tech_map=="BECCS liquids",]) %>%
  rbind(.,USA_sectors_3[USA_sectors_3$tech_map=="BECCS liquids",]) %>%
  rbind(.,USA_sectors_4[USA_sectors_4$tech_map=="BECCS liquids",])

BECCSliq2 <- ggplot(data=BECCSliq2_data, aes(x=year,y=netvalue,color=scenario)) + 
  geom_line(size=1) +
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT), plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.position="none",
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  xlab("Year") + labs(tag="d") +
  ggtitle("BECCS liquids")

bottom <- grid.arrange(LUC2, BECCSliq2, nrow=1,left = y_label_NETs)

tiff(filename="NETs_scenario_comp.png",units='mm',width=180,height=220,res=300)
grid.arrange(top,bottom,nrow=2,right=legend_DACCS)
dev.off()


#-------------------------------------------------------------------------------
# SI Figure 6: Comparing sectoral CCS
#-------------------------------------------------------------------------------
#run for each scenario, changing the number at the end of CCS_sectors_data
CCS_sectors_data1 <- NEWdac_highEV_co2_seq %>%
  gather(year, value, -c(scenario, region, sector, subsector,technology,Units)) %>%
  mutate(scenario = SCENARIO) %>%
  mutate(year=as.numeric(year)) %>%
  filter(!str_detect(sector,"feedstock"), year>=2020, year<=2050) %>%
  left_join(tech_mapping %>% select(sector,technology,tech_map), by = c('sector','technology')) %>%
  group_by(tech_map, year) %>% 
  summarise(totalCCS=(44/12000)*sum(value)) %>%
  mutate(Units="GtCO2")

colors <- c("black", "burlywood4","burlywood3","darkorange3", "yellow3", "darkgreen",
            "darkblue", "aquamarine4",  "aquamarine3")
names(colors) <- c("Buildings","Industry", "Refining","Transportation", "Electricity",
                   "Land-use change", "DACCS", "BECCS electricity","BECCS liquids")

TITLE_SIZE=12
TEXT_SIZE=8
FONT="Calibri"

#scenario a) low LUC + low-cost CCS
CCS_sectors1 <- ggplot(data=CCS_sectors_data1, aes(x=year,y=totalCCS, fill=tech_map)) +
  geom_bar(position= position_stack(reverse = TRUE), stat="identity",) +
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT), plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.title = element_blank(), legend.text=element_text(size=TEXT_SIZE,family=FONT),
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  ylim(0,3.5) +
  xlab("Year") + labs(tag="a") +
  ggtitle("Slow LUC / Low-cost CCS") +
  scale_fill_manual(values=colors)

y_label_CCS <- grid::textGrob(expression("Total CCS ("*GtCO[2]~year^{-1}*")"), rot = 90,gp=gpar(fontsize=TEXT_SIZE))
legend_totalCCS <- get_legend(CCS_sectors1)
CCS_sectors1 <- CCS_sectors1 + theme(legend.position="none")


#scenario b) low LUC + high-cost CCS
CCS_sectors2 <- ggplot(data=CCS_sectors_data2, aes(x=year,y=totalCCS, fill=tech_map)) +
  geom_bar(position= position_stack(reverse = TRUE), stat="identity",) +
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT), plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.position="none",
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  ylim(0,3.5) +
  xlab("Year") + labs(tag="b") +
  ggtitle("Slow LUC / High-cost CCS") +
  scale_fill_manual(values=colors)

top <- grid.arrange(CCS_sectors1, CCS_sectors2, nrow=1,left = y_label_CCS)


#scenario c) high LUC + low-cost CCS
CCS_sectors3 <- ggplot(data=CCS_sectors_data3, aes(x=year,y=totalCCS, fill=tech_map)) +
  geom_bar(position= position_stack(reverse = TRUE), stat="identity",) +
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT), plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.position="none",
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  ylim(0,3.5) +
  xlab("Year") + labs(tag="c") +
  ggtitle("Fast LUC / Low-cost CCS") +
  scale_fill_manual(values=colors)


#scenario d) high LUC + high-cost CCS
CCS_sectors4 <- ggplot(data=CCS_sectors_data4, aes(x=year,y=totalCCS, fill=tech_map)) +
  geom_bar(position= position_stack(reverse = TRUE), stat="identity",) +
  theme(plot.title = element_text(size=TITLE_SIZE,family=FONT), plot.tag=element_text(size=TITLE_SIZE,family=FONT),
        legend.position="none",
        axis.title.x = element_text(size=TEXT_SIZE,family=FONT),axis.text.x = element_text(size=TEXT_SIZE,family=FONT, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(), axis.text.y = element_text(size=TEXT_SIZE,family=FONT)) +
  ylim(0,3.5) +
  xlab("Year") + labs(tag="d") +
  ggtitle("Fast LUC / High-cost CCS") +
  scale_fill_manual(values=colors)

bottom <- grid.arrange(CCS_sectors3, CCS_sectors4, nrow=1,left = y_label_CCS)

tiff(filename="NEWdac_total_ccs_sensitivity.png",units='mm',width=180,height=220,res=300)
grid.arrange(top,bottom,nrow=2,right=legend_totalCCS)
dev.off()

#-------------------------------------------------------------------------------
# SI Figure 7: CO2 price over time
#-------------------------------------------------------------------------------
#run for each scenario, changing the number at the end of co2_price
co2_price1 <- NEWdac_co2_price %>%
  gather(year, value, -c(scenario, region, market, Units)) %>%
  mutate(scenario = SCENARIO, year=as.numeric(year)) %>%
  filter(market == "USACO2", year>=2020, year<=2050) %>%
  mutate(value = 1.98*value) %>%   #inflating to 1990$ to 2020$
  mutate(value = (12/44)*value) %>%   #converting C to CO2
  mutate(Units = "2020$/tCO2")

co2_price_data <- co2_price1 %>%
  rbind(.,co2_price2,co2_price3,co2_price4)

co2_price <- ggplot(data=co2_price_data, aes(x=year, y=value, color=scenario)) +
  geom_line() +
  scale_colour_discrete(breaks=c("NEWdac_netzero_usa_global_ssp2_highEV",                     #scenario a) low LUC + low-cost CCS
                                 "NEWdac_netzero_usa_global_ssp2_highEV_lowCCS",              #scenario b) low LUC + high-cost CCS
                                 "NEWdac_netzero_usa_global_ssp2_highEV_luc2050",             #scenario c) high LUC + low-cost CCS
                                 "NEWdac_netzero_usa_global_ssp2_highEV_lowCCS_luc2050"),     #scenario d) high LUC + high-cost CCS
                        labels=c("Slow LUC/ Low-cost CCS",        #scenario a) low LUC + low-cost CCS
                                 "Slow LUC/ High-cost CCS",       #scenario b) low LUC + high-cost CCS
                                 "High LUC/ Low-cost CCS",        #scenario c) high LUC + low-cost CCS
                                 "High LUC/ High-cost CCS")) +    #scenario d) high LUC + high-cost CCS
  xlab("Year") + ylab(expression("Carbon price (2020$/t"*CO[2]*")"))

tiff(filename="CO2_price.png",units='mm',width=180,height=100,res=300)
co2_price
dev.off()

#-------------------------------------------------------------------------------
