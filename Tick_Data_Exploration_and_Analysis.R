# 03/18/2019
# Tick and tick-born pathogen dataset exploration

# load R packages into R environment
require(tidyverse) # use dplyr, lubriate 
require(lubridate)
require(sf)
require(sp)
#require()


#################
### FUNCTIONS ###
#################
vegType_summary <- function(x) {
  ## combines vegetation types across plots for each site 
  values <- x
  unique_values <- values %>% unique %>% paste(collapse='_')
  return(unique_values)
}

P_A <- function(x) {
  ## looks across all the plots sampled for a given site and returns 'Y' if ticks were found at any of the plots belonging to the site
  if ('Y' %in% x) {
    answer <- 'Y'
  } else {
    answer <- 'N'
  }
  
  return(answer)
}

number_plots_sampled <- function(x) {
  ## counts the number of plots sampled per site
  number_plots <- x %>% unique %>% length
  return(number_plots)
}

mean_elev <- function(x) {
  ## returns the mean elevation of the site based on the plots sampled
  return( x %>% unique %>% mean())
}

summary_testResult_by_site <- function (x) {
  # x is a vector of the testResults for the siteID, year combo
  # returns a positve if at least one 
  if ('Positive' %in% x) {
    test_result_summary <- 'Positive'
  } else {
    test_result_summary <- 'Negative'
  }
  
  return(test_result_summary)
}

#################
### LOAD DATA ###
#################

tck_fd <- read_csv('1_tck_fielddata_concatenated.csv')
tck_tax <- read_csv('2_tck_taxonomyProcessed_concatenated.csv')
tck_path <- read_csv('3_tck_pathogen_concatenated.csv')

##############
### PART A ###
##############

## Question 1:Which field sites had ticks across different sampling years? What was the density of ticks (ticks counted/sampled area)?
########################################################################

sites_mapping <- tck_fd %>% 
  select(siteID, plotID, lon = decimalLongitude, 
         lat = decimalLatitude, elev = elevation, 
         vegType = nlcdClass, date=collectDate, 
         targetTaxaPresent=targetTaxaPresent) %>% 
  mutate(year=year(date)) %>%
  group_by(siteID, year) %>%
  summarize(countPlots=number_plots_sampled(plotID), 
            mean_elev=mean_elev(elev), lat=mean(lat), 
            lon=mean(lon), 
            ticksPresent=P_A(targetTaxaPresent))
names(tck_fd)
tck_fd %>% pull('nlcdClass') %>% unique()
# create empty data frame to fill
years <- c(2014, 2015, 2016, 2017, 2018)
col_names <- c('year', 'number_sites_sampled', 'number_sites_w_ticks', 'percent')
df <- as.data.frame(matrix(nrow=length(years), ncol = length(col_names)))
colnames(df) <- col_names


for (i in 1:length(years)) {
  num_sites_sampled <- sites_mapping %>% filter(year == years[i]) %>% nrow()
  num_sites_present <- sites_mapping %>% filter(year == years[i]) %>% filter(ticksPresent == 'Y') %>% nrow()
  
  df$year[i] <- years[i]
  df$number_sites_sampled[i] <- num_sites_sampled
  df$number_sites_w_ticks[i] <- num_sites_present
  df$percent[i] <- (num_sites_present/num_sites_sampled) * 100
}

# visualization:
sites_mapping_sf <- st_as_sf(sites_mapping, coords=c('lon', 'lat'), crs=4326)
sites_mapping_sf
names(sites_mapping_sf)

usa<- st_read("Shape_Files/cb_2017_us_state_5m/cb_2017_us_state_5m.shp") # available from us census data here: https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html

new_bbox <- st_bbox(c(xmin=-180,xmax=-50,ymax=75,ymin=15), crs=4326) # sets bbbox (the visible area of the map)

tm_shape(st_geometry(usa), bbox=new_bbox) +tm_polygons() +  tm_shape(sites_mapping_sf) + tm_symbols(col='ticksPresent', size=0.25) + tm_facets(by='year', syn=T)



## Question 2: Does vegetation type of the sampling site influence the presence of ticks? What other factors may influence the presence of ticks (i.e. latitude, elevation)?
########################################################################
# logistic regression analyis 
# model: 

## Question 3: What species of tick were identified? What is the distribution of tick species? (i.e. What field sites had which species?)
########################################################################

# Some SQL-like data processing 

# tck_fd
tck_fd$collectDate <- as_date(tck_fd$collectDate)
tck_fd_v2 <- tck_fd %>% 
  rename(fd_uid=uid, lat=decimalLatitude, lon=decimalLongitude, ) %>% 
  mutate(year=year(collectDate))
names(tck_fd_v2)

#tck_tax
tck_tax_v2 <- tck_tax %>% 
  select(tax_uid = uid, sampleID, subsampleID, scientificName, acceptedTaxonID, sexOrAge,  identifiedBy)

# join tck_fd and tck_tax
tck_fd_tax <- left_join(tck_tax_v2, tck_fd_v2, by='sampleID')
tick_sp <- tck_fd_tax %>% 
  arrange(scientificName) %>%
  pull(scientificName) %>% 
  unique() # species of tick identified
tick_sp

# get complete list of all site by year combos
sites_sampled_by_year <- tck_fd_v2 %>% 
  select(siteID, year, lat, lon) %>% 
  group_by(siteID, year) %>% 
  summarize(lat=mean(lat), lon=mean(lon)) %>% arrange(siteID) %>% 
  add_column(present=1)
ssby <- as.data.frame(sites_sampled_by_year)
head(ssby)

# prep for mapping
# usa shape file
usa<- st_read("../../Downloads/cb_2017_us_state_5m/cb_2017_us_state_5m.shp") # available from us census data
# set bbox
new_bbox <- st_bbox(c(xmin=-180,xmax=-50,ymax=75,ymin=15), crs=4326)

for (i in 1:length(tick_sp)) {
  df <- tck_fd_tax %>% select(scientificName, siteID, year) %>%  
    filter(scientificName==tick_sp[i]) %>% 
    distinct() %>% 
    arrange(siteID)

  mapping_data_i <- ssby
  
  for (z in 1:nrow(ssby)) {
    site_row_z <- ssby$siteID[z]
    year_row_z <- ssby$year[z]
    
    row1 <- which(df$siteID== site_row_z)
    row2 <- which(df$year == year_row_z)
    test_vector <-  intersect(row1, row2)
    
    if (length(test_vector) > 0) {
      mapping_data_i$present[z] <- 'Y'
    } else {
      mapping_data_i$present[z] <- 'N'
    }
  }
  
  # transform mqpping data into shape vector
  mapping_data_i_sf <- st_as_sf(mapping_data_i, coords=c('lon', 'lat'), crs=4326)

  # open pdf file
  fileName = paste('figs/', tick_sp[i], '_distribution.pdf', sep='')
  pdf(fileName)
  
  # create plot
  print(tm_shape(st_geometry(usa), bbox=new_bbox) +tm_polygons() + tm_shape(mapping_data_i_sf) + tm_symbols(col='present', size=0.25) + tm_facets(by='year', syn=T))
 
  # close pdf file
  dev.off()
  
}

##############
### PART B ###
##############

## Question 1: Which tick-born pathogens were tested for? And which were tested positive for?
########################################################################

names(tck_path)
test_results <- tck_path %>% select('testResult', 'testPathogenName')
test_results['testPathogenName'] %>% distinct() %>% arrange(testPathogenName)

test_results['testResult'] %>% unique
test_results[which(tck_path['testResult'] =='Positive'),] %>% distinct() %>% arrange(testPathogenName)
test_results[which(tck_path['testResult'] =='Negative'),] %>% distinct() %>% arrange(testPathogenName)

## Question 2: Which tick species carried each of the pathogens that were tested positive? 
########################################################################

# some SQL-like data processing
# rename uid across all three datasets to give unique id
# tck_fd
tck_fd$collectDate <- as_date(tck_fd$collectDate)
tck_fd_v2 <- tck_fd %>% 
  rename(fd_uid=uid, lat=decimalLatitude, lon=decimalLongitude) %>% 
  mutate(year=year(collectDate))
names(tck_fd_v2)

#tck_tax
tck_tax_v2 <- tck_tax %>% 
  select(tax_uid = uid, sampleID, subsampleID, scientificName, acceptedTaxonID, sexOrAge,  identifiedBy)

#tck_path
tck_path_v2 <- tck_path %>% 
  select(path_uid=uid, subsampleID, testedDate, batchID,  testResult, testPathogenName) %>% 
  group_by(subsampleID, testPathogenName) 

# join tck_path, tck_tax, and tck_fd 
tck_path_tax <- left_join(tck_path_v2, tck_tax_v2, by='subsampleID')
tck <- left_join(tck_path_tax, tck_fd_v2, by='sampleID')
tck # 78,956 x 39
names(tck)

tck_tax_v2[,'scientificName'] %>% distinct() # tick species tested for pathogens


cols <- which(names(tck) %in% c('scientificName', 'testPathogenName', 'testResult'))

tck[,cols] %>% filter(testResult == 'Positive') %>% distinct(testPathogenName) %>% arrange(testPathogenName)# pathogens tested postive for

tck[,cols] %>% filter(testResult == 'Positive') %>% distinct(scientificName) # tick speices tested positive for carrying pathogen

tck[,cols] %>% filter(testResult == 'Positive') %>% distinct(scientificName, testPathogenName) %>% arrange(scientificName) # asscoiation between pathogen and vector species
  

## Question 3: At which field sites did ticks test positive for each of the pathogens? And for the tick species which tested positive for pathogens, at which sites did they test positive and at which sites did they test negative?
##############################################################################

# 1st, for each pathogen that was tested positive for plot the field site where it was found
# generate a list of pathogens to loop through...
cols <- which(names(tck) %in% c('scientificName', 'testPathogenName', 'testResult'))

pathogens <- tck[,cols] %>% filter(testResult == 'Positive') %>% distinct(testPathogenName) %>% arrange(testPathogenName) %>% pull(testPathogenName) # pathogens tested postive for

# get complete list of all site by year combos
sites_sampled_by_year <- tck_fd_v2 %>% 
  select(siteID, year, lat, lon) %>% 
  group_by(siteID, year) %>% 
  summarize(lat=mean(lat), lon=mean(lon)) %>% arrange(siteID) %>% 
  add_column(present=1)
ssby <- as.data.frame(sites_sampled_by_year)
head(ssby)

# prep for mapping
# usa shape file
usa<- st_read("../../Downloads/cb_2017_us_state_5m/cb_2017_us_state_5m.shp") # available from us census data
# set bbox
new_bbox <- st_bbox(c(xmin=-180,xmax=-50,ymax=75,ymin=15), crs=4326)

pathogens
i <-1

for (i in 1:length(pathogens)) {
  df <- tck %>% select(scientificName, testPathogenName, testResult, siteID, year) %>%  
    filter(testPathogenName==pathogens[i]) %>% 
    distinct() %>% 
    arrange(siteID)
 
  mapping_data_i <- ssby 
  
  for (z in 1:nrow(ssby)) {
    site_row_z <- ssby$siteID[z]
    year_row_z <- ssby$year[z]
    
    row1 <- which(df$siteID== site_row_z)
    row2 <- which(df$year == year_row_z)
    test_vector <-  intersect(row1, row2)
    
    if (length(test_vector) > 0) {
      mapping_data_i$present[z] <- 'Y'
    } else {
      mapping_data_i$present[z] <- 'N'
    }
  }
  
  # transform mqpping data into shape vector
  mapping_data_i_sf <- st_as_sf(mapping_data_i, coords=c('lon', 'lat'), crs=4326)
  
  # open pdf file
  fileName = paste('figs2/', pathogens[i], '_distribution.pdf', sep='')
  pdf(fileName)
  
  # create plot
  print(tm_shape(st_geometry(usa), bbox=new_bbox) +tm_polygons() + tm_shape(mapping_data_i_sf) + tm_symbols(col='present', size=0.25) + tm_facets(by='year', syn=T))
  
  # close pdf file
  dev.off()
  
}

# for each of the two tick speices that tested postiive for having a pathogen I want to plot the sites where they were sampled from and indicate at which sites they tested positive for!


tck_sp2 <- c("Amblyomma americanum", "Ixodes scapularis")

for (i in 1:length(tck_sp2)) {
  df <- tck %>% select(scientificName, testPathogenName, testResult, siteID, year, lat, lon) %>%  
    filter(scientificName==tck_sp2[i]) %>% 
    filter(testResult == 'Negative' || testResult == "Postive") %>%
    distinct() %>% 
    group_by(siteID, year) %>%
    summarize(lat = mean(lat), lon = mean(lon), test_summary = summary_testResult_by_site(testResult)) %>%
    arrange(siteID)
  
    # transform mqpping data into shape vector
    df_sf <- st_as_sf(df, coords=c('lon', 'lat'), crs=4326)
  
  # open pdf file
  fileName = paste('figs3/', tck_sp2[i], '_postive_distribution.pdf', sep='')
  pdf(fileName)
  
  # create plot
  print(tm_shape(st_geometry(usa), bbox=new_bbox) +tm_polygons() + tm_shape(df_sf) + tm_symbols(col='test_summary', size=0.25) + tm_facets(by='year', syn=T))
  
  # close pdf file
  dev.off()
  
}


### END ###