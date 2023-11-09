# EDGE_thesis
These data were analyzed and presented in an accompanying paper where we observed a positive correlation between low-density population growth rates in drought and low-density population growth rates in the presence of interspecific neighbors. We also found that high leaf dry matter content and low (more negative) leaf turgor loss point were associated with higher population fitness in drought and with higher neighbor competition.
The EDGE_covers.csv dataset contains aggregated absolute cover estimates and annual population growth summarized from the Extreme Drought in Grassland Experiment (EDGE). The all_pop_data.csv contains the calculated population growth rates for each population and the trait data paired with each population. This is the primary data used for all analyses.


all_pop_data.csv contains 113 rows where each observation contains the calculated low-density population growth rates in different conditions and trait values for each unique population (species-site combination). Columns include:
"species" = Latin names for each species formatted as Genusspecies.
"site" = denotes what site in the EDGE project data is from. CHY = High Plains Grassland Research Station, WY; HYS = Hays Agricultural Research Center, KS; KNZ = Konza Prairie Biological Station, KS; SGS = Central Plains Experimental Range, CO; sev.blue = Sevilleta Wildlife Refuge, NM (shortgrass), sev.black = Sevilleta Wildlife Refuge, NM (desert grassland).
"Photosynthesis" = type of photosynthetic pathway.
"annual.Perennial" = lifespan. Includes perennial, annual, and biennial.
"height" = average maximum plant height (mm).
"rootdiam" = root diameter (mm).
"SRL" = specific root length (m g-1).
"RTD" = root tissue density (cm3 g-1).
"rootN" = root nitrogen content (%).
"SLA" = specific leaf area (cm2 g-1).
"LDMC" = leaf dry matter content (g g-1).
"LTD" = leaf tissue density (cm3 g-1).
"leafarea" = leaf area (cm2).
"TLP" = turgor loss point (Mpa).
"leafN" = leaf nitrogen content (%).
"intrinsicLDGRcon" = low density growth rate with 0 interspecific cover in ambient precipitation.
"intrinsicLDGRchr" = low density growth rate with 0 interspecific cover in drought.
"invasionLDGRcon" = low density growth rate with mean interspecific cover in ambient precipitation.
"invasionLDGRchr" = low density growth rate with mean interspecific cover in drought.
"weight" = standard error of models used to calculate low density growth rates.
"weight2" = 1/"weight" column. Used to weight certainty in our growth rates in subsequent models.
"grassland_type" = grassland community type. Includes tallgrass prairie, southern mixed-grass prairie, northern mixed-grass prairie, northern shortgrass prairie, southern shortgrass prairie, and desert grassland.
"grass.forb" = type of growth form grouped into Grass or Other.
"lifespan" = length of species lifespan. grouped into perennial or short-lived species.

Code/Software
All analyses were conducted in R version 4.1.3 (R Core Team, 2022). R scripts used to complete the analysis in the associated publication are available on a public Github repository (https://github.com/Hmount/EDGE_thesis). In the folder named "code" in this repository, there are six .R scripts containing the code for all data manipulation, analyses, and figures included in the publication and supporting information.

"Data wrangling.R" script includes code to summarize data from larger EDGE dataset, calculate lambda, and create the EDGE_covers.csv and all_pop_data.csv. It is not necessary to run this script in order to replicate the analyses, but shows how data were compiled.
"summary stats.R" contains initial summary information about the different growth rates calculated and tests for differences in growth rate related to grassland type.
"responses analysis.R" contains the analysis of the relationships between growth rate in drought and growth rate with neighbors.
"traits analysis.R" contains the analysis of the relationship between traits and population growth in different precipitation conditions and neighbor abundances.
"figures.R" contains the code to reproduce all of the figures in the manuscript.
"supplemental info.R" contains the analyses and figures for information included in the supplemental.
Research completed for my M.S. where we we observed a positive correlation between low-density population growth rates in drought and in the presence of interspecific neighbors. High leaf dry matter content and low (more negative) leaf turgor loss point were associated with higher fitness in drought and neighbor competition.
