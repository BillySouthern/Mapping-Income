#11/28/23, initiated by BS
#Goal: To map household income across a spatial extent  
#Script retrieve data and plots as dot density

#Variables of interest:
  #Household Income in the Past 12 Months (ACS 2016-20)

#Libraries
library(tidycensus)
library(tidyverse)
library(tigris)
library(ggplot2)
library(sf)
library(ggsn)

options(tigris_use_cache = TRUE)

#Parameters 
GEOG = "block group"
ST = "VA"
CBSA = c("Richmond")
CENTRAL_CITY = c("Richmond city")
COUNTIES<-c("075", "085", "087", "041", 
            "097", "101", "127", "145", 
            "183", "053", "007", "149", 
            "036", "109", "049", "033")
YR = 2020

#Set number of household that dot represents
DOTS = 50



#-------------------------------------------------------------------------------
#Download 2020 binned data from ACS
Income_Binned <- get_acs(
  geography = GEOG,
  table = "B19001", 
  #variables = Income_Groups,
  state = ST,
  #county = COUNTIES,
  geometry = TRUE,
  year = YR,
  output = "wide",
) 

#Tidy table
Income_Binned <- Income_Binned %>%
  rename(Below10000 = "B19001_002E",
         Btw10000_14999 = "B19001_003E",
         Btw15000_19999 = "B19001_004E",
         Btw20000_24999 = "B19001_005E",
         Btw25000_29999 = "B19001_006E",
         Btw30000_34999 = "B19001_007E",
         Btw35000_39999 = "B19001_008E",
         Btw40000_44999 = "B19001_009E",
         Btw45000_49999 = "B19001_010E",
         Btw50000_59999 = "B19001_011E",
         Btw60000_74999 = "B19001_012E",
         Btw75000_99999 = "B19001_013E",
         Btw100000_124999 = "B19001_014E",
         Btw125000_149999 = "B19001_015E",
         Btw150000_199999 = "B19001_016E",
         Above200000 = "B19001_017E") %>%
  mutate(Below24999 = Below10000 + Btw10000_14999 + Btw15000_19999 + Btw20000_24999,
         Btw25000_49999 = Btw25000_29999 + Btw30000_34999 + Btw35000_39999 + Btw40000_44999 + Btw45000_49999,
         Btw50000_99999 = Btw50000_59999 + Btw60000_74999 + Btw75000_99999,
         Btw100000_149999 = Btw100000_124999 + Btw125000_149999,
         Btw150000_199999 = Btw150000_199999,
         Above200000 = Above200000) %>%
    select(GEOID, NAME, Below24999, Btw25000_49999, Btw50000_99999, 
           Btw100000_149999, Btw150000_199999, Above200000) %>%
    pivot_longer((Below24999:Above200000))

#Add Counties as underlying geography
CBSA_2020 <- core_based_statistical_areas(resolution = "500k", year = YR) %>%
  filter(str_detect(NAME, ST)) %>%
  filter(str_detect(NAME, CBSA))

#Acquiring the cities within VA
CentralCities_2020 <- places(state = ST, year = YR) %>%
  filter(str_detect(NAMELSAD, "city")) %>%
  mutate(City = if_else(NAME == CBSA, "Central", "Independant"))

#Spatial filter the cities within the Richmond MSA
CentralCities_2020 <- CentralCities_2020[lengths(st_within(CentralCities_2020,CBSA_2020)) > 0,] 

#Spatial filter the income data to the geography of interest
Income_Binned <- Income_Binned[lengths(st_within(Income_Binned,CBSA_2020)) > 0,] 

#Create sf object and shift geometry (shift geometry is simple way to change the projection to Albers)
Income_Binned <- Income_Binned %>% 
  st_as_sf(coords = c('lonCust', 'latCust')) %>%
  shift_geometry()

#Shift CBSA geom
CBSA_2020 <- CBSA_2020 %>% 
  st_as_sf(coords = c('lonCust', 'latCust')) %>%
  shift_geometry()

#Erase water bodies
Income_Binned <- erase_water(Income_Binned)
CBSA_2020 <- erase_water(CBSA_2020)

# Convert data to project the dots
Dots <- as_dot_density(
  Income_Binned,
  value = "value",
  values_per_dot = DOTS,
  group = "name"
)

#Create the plot.  Change the title, number of dots, and scale
ggplot() +
  geom_sf(data = CBSA_2020,
          fill = "black",
          color = "black",
          alpha = 50) +
  geom_sf(data = Dots,
          aes(color = name),
          size = 0.002) +
  scale_color_manual(values=c("#bd0026","#ffffb2", "#fd8d3c", "#f03b20","#fed976", "#feb24c"),
                     labels = c("Above $200,000", "Below $24,999", "Between $100,000 and $149,999",
                                "Between $150,000 and $199,999","Between 25,000 and $49,999",
                                "Between $50,000 and $99,999")) +
  guides(color = guide_legend(override.aes = list(size = 2.0), 
                              title = "1 dot = 50 households"))

#Export the plot
ggsave("Mapping-Income.png",
       path = "~/desktop",
       width = 12,
       height = 12,
       units = "in",
       dpi = 500)