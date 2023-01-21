## Drivers of organic seed adoption, 2001-2021 

This repository houses code for the draft of CH1: Alternative sustainability transition pathways: Assessing innovation system functions' affect on adoption in the organic seed niche. The data of focus in this repository include invidual-level data based on organic grower surveys (2011, 2016, 2021), combined with innovation system quantification across that time period (2001-2021). Systems-level data collection is housed in the [quantifying_seed_innovation repository](https://github.com/liza-wood/quantifying_seed_innovation).

### Data storage:   
Organic producer survey data from the 2011, 2016, and 2021 SOS reports are stored on the shared Dropbox, SOSInteractiveSurvey, and newly created data are stored in the shared Box folder, organicseed_adoption, or in the `data/` folder (namely codebooks).

### Code file structure:    

```
code/
  00_creating_codebook: looks at the three organic producer survey data sets
     (2011, 2016, 2021), and extracts the column titles and question text from 
     the three and compiles them together, looking for overlaps. Codebooks are 
     stored in the `data/` folder.
  01_merging: reads in `data/codebook_3panel.csv` and the RDS files from the 
     Dropbox. It uses the codebook to select only the columns that overlap 
     across all 15 years and then also identifies the region for each respondent
     based on their state.
  02_variable_creation: adds in a few new columns for accomodating modelling. 
     This is importantly where I identify the `maincrop` variable of a producer 
     based on how they report their total acreage and then acreage by crop, 
     looking for the crop that has the largest  proportion. I also experiment 
     with factor analysis of challenges.
  03_grower_level_data_summary: using cleaned data from the previous script, I
     create tables summarizing key variables of (Table 1 and 2 and Figure 3)
  03_innovation_level_data_summary: Similar to the summary in the quantifying_
     seed_innovation summary.R, I bring together data on research funding, 
     intellectual property, organic entrants, and seed company sales to 
     summarize the innovation system, separate from the organic growers. 
     I also create Figure 4. At the end of the script I generate the data
     file: df_multilevel.rds
  04_map_making: Using combined individual and systems-level data I generate
     maps to visualize how the data were sampled and how they can represented
     at the national/regional scale (Figure 2)
  05_models_DVacreage_exploration: fits a few different models using the DV
     of percent acreage. These include acreage of `maincrop` in the df 
     dataframe, identified in the variable_creation script, and of every crop 
     that a producer grows, using df.long. This dataframe, also created in the 
     variable_creation script, is a lengthened dataframe where there are 
     repeated values for some producers that grow more than one crop. For each 
     of these data structures (df and df.long) I fit two models: one more basic 
     linear model that includes year and region as fixed effects, and a second 
     mixed effects model where I use year and region as random effects, and can 
     add in the attributes of the year and region (funding and number of 
     producers per region) as attributes of those higher levels. For the 
     df.long models, I also have to include respondentID as a random effect 
     because of the repeated nature of the multiple crops. I compare these 
     models and run a couple diagnostics and *tentatively* think that the main 
     crop dataframe is more predictive than the longer dataframe where producers
     with multiple crops are repeated. The random and mixed effects models seem 
     to perform similarly, so I keep working with both of them in the following 
     scripts. 
  06_models_final_multilevel: still TBD
  07_S5_modelest_compare: Here I compare different models based on likelihood: 
     linear, binomial, and beta. I use this supplemental material to inform the 
     final model.
```

### Data flow:    

1. From the `Dropbox/SOSInteractiveSurvey/` read in `data2011_cleaned.rds` + `data2016_cleaned.rds` +  `data2021_cleaned.rds` into `01_merged.R`. Merged outputs saved into `Box/organicseed_adoption/` as `merged_df_3panel.csv`
1. `02_variable_creation.R` reads in `merged_df_3panel.csv`, cleans it and outputs: `Box/organicseed_adoption/df.rds` and `Box/organicseed/df.long.RDS`  
1. `03_innovation_level_data_summary` reads in `Box/organicseed_adoption/df.rds` and merges it with data cleaned in quantifying_seed_innovation repository and outputs `Box/organicseed_adoption/df_multilevel.rds`
1. The `df_multilevel.rds` is the main data frame used for analysis