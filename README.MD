# NEON Tick and Pathogen Occurrence Exploratory Data Analysis

## Skills used
* file management with python
* data processing and filtering in R (SQL-like)
* statistical analysis in R
* geocomputation in R with shape files and vector data

## Questions of Interest

### Part A
1. Which field sites had ticks across the different sampling years? What was the density of ticks (ticks counted/sampled area)?
2. Does vegetation type of the sampling site influence the presence of ticks? What other factors may influence the presence of ticks (i.e. latitude, elevation)?
3. What species of tick were identified? What is the distribution of tick species? (i.e. What field sites had which species?)

### Part B
1. Which pathogens were tested for? And which were tested positive for?
2. Which tick species carried each of the pathogens tested positive for?
3. At which field sites did ticks test positive for each of the pathogens? And for the two tick species which tested positive for pathogens (Amblyomma americium, and Ixodes scapularis), at which sites did they test positive and at which sites did they test negative?


## Description of the Datasets
All data is openly and freely accessible via the National Ecological Observatory Network website. The National Ecological Observatory Network (NEON) is a NSF funded project designed to  "characterize and quantify complex, rapidly changing effects of ecological processes across the US" over a 30 year time-frame. Data collected is intended to be used by "scientists, educators, planners, decision makers and public to map, understand and predict the effects of human activities on ecology and effectively address critical ecological questions and issues." (https://www.neonscience.org/about)

For my analyses, I focus on the ticks sampled using drag cloths and tick-born pathogen status datasets (data available here: https://data.neonscience.org/browse-data?showAllDates=true&showAllSites=true&showTheme=org). Datasets were downloaded on 02/13/2019 and a custom python script (Data/concatenate_csv.py) was used to compile data from individual csv files downloaded from NEON resulting in three relational datasets related by unique identifier keys (see Tables_pdf/ER_diagram_relational_datasets.pdf for Entity Relationship (ER) Diagram for dataset relations)-- tck_fieldData, tck_taxonomyProcessed, and tck_Pathogen. Taxonomic identification and pathogensis analyses are still being carried out on field collected samples and as such tc_taxonomyProcessed and tck_pathogen datasets are incomplete. Below are details of the three datasets as of 02/13/2019. Also included in this repository is an ER Diagram (Tables_pdfs/ER_diagram_relational_datasets.pdf) which diagrams the relations between datasets.

### tck_fieldData
* 6,124 rows x 27 columns
* Details the count of adult, nymph and larva of ticks collected at fieldsites using a drop cloth method
* Samples were collected from 04/2014 to 11/2018 from 45 sites and 283 plots (note that not all sites and plots were sampled over the full time period)

### tck_taxonomyProcessed
* 2,962 rows x 31 columns
* Provides the taxonomic species identification of ticks collected from tck_fieldData
* The taxonomy has been processed for 25 sites and 130 plots for samples collected from 04/2014-09/2018

### tck_pathogen
* 78,956 rows x 27 columns
* Details the results of pathogensis analyses performed on ticks collected from tck_fieldData and identified in tck_taxonomyProcessed
* Results from pathongensis analyses have been obtained for 13 sites and 62 plots.

## Summary of Results

### Part A
#### Question 1:
In 2014, 14 sites were sampled and 8 of the 14 sites (57.1%) had ticks present. By 2018, 45 sites were sampled and 29 of the 45 sites (64.4%) had ticks present. See Tables_pdf/Table_1 for other years. See also Figures/PartA_Q1. Although some sites (i.e. a site in central Florida and a site in south-eastern South Dakota) had ticks present across all sampling years, other sites were variable in the presence of ticks from year to year. At a coarse level geographically, the presence of ticks is fairly constant in the eastern and south eastern sections of the United States across years while sites in the western United States are more variability. Additionally, ticks seem to be absent from field sites at greater latitudinal extremes (e.g. Alaska) or closer to the equator (e.g. Puerto Rico).

#### Question 2:
More to come but the general game plan is....
Factors influencing the presence of ticks
I used a logistic regression
p = B0 + B1x1 + B2x2 + B3x3 + B4x4 + e

where p = the probability of a site having ticks
x1 = site vegetation type (categorical - 11 vegetation classes: evergreenForest, shrubScrub, grasslandHerbaceous, mixedForest, emergentHerbaceousWetlands, decidousForest, cultivatedCrops, pastureHay, sedgeHerbaceous, woodyWetlands, dwarfScrub)
x2 = latitude of site (continuous)
x3 = longitude of site (continuous)
x4 = elevation of site (continuous)
x5 = sampling year (categorical- 5 sampling years: 2014, 2015, 2016, 2017, 2018)


#### Question 3:
Twelve species of tick were identified to species-level (Tables_pdfs/Table_2). See figures for the distribution across field sites for each of the twelve tick species identified (Figures/PartA_Q3.pdf). In general, species of Amboyomma are distributed in the south east (Flordia, Georgia), species of Dermacentor are distributed in the midwest and east coast, and species of Ixodes are distributed mainly on the east coast but also are present in a site located in Washington. Note, that samples from only samples of 25/45 sites have been processed (as of when this dataset was downloaded) and thus the appeared distribution of species may not reflect the true distribution of species.

### Part B
#### Question 1
Thirteen tick-borne pathogens were tested for (Tables_pdfs/Table_3) to the species level while one tick-born pathogen was tested for at the genus level. Of the thirteen tick-borne pathogens tested for 9 were found.

#### Question 2
Of the 12 identified species of tick identified to species-level, the following six were targeted for tick-borne pathogen testing, according to the NEON tick-borne pathogen sampling protocol: Ixodes scapularis (black-legged tick), I. pacificus (western black-legged tick), Amblyomma americanum (lone star tick), A. maculatum (gult coast tick), Dermacentor andersoni (Rocky mountain wood tick, and D. variabilis). Only two tick species, Amblyomma americium and Ixodes scapularis, tested positive for pathogens (Tables_pdf/Table_3).

#### Question 3
For each of the pathogens tested positive for and for each year, I plotted a map indicating at which sites ticks testing positive for the pathogen was found (see Figures/PartB_Q3_1). See figures in directory (Figures/PartA_Q3). Additionally for the two ticks species that tested positive for pathogens I plotted for which sites it carried a pathogen and for which sites it did not across sampling years (see Figures/PartB_Q3_2).  


## Useful References

Geocomputation with R: https://geocompr.robinlovelace.net/index.html

US census data-- shape file of United States (including territories): https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
