## Drivers of organic seed adoption, 2004-2020  

### Data storage:   
Organic producer survey data from the 2011, 2016, and 2021 SOS reports are stored on the shared Dropbox, SOSInteractiveSurvey, and newly created data are stored in the shared Box folder, organicseed_adoption, or in the `data/` folder (for codebooks).

### Code file structure:    

```
code/
  00_creating_codebook: looks at the three organic producer survey data sets
     (2011, 2016, 2021), and extracts the column titles and question text from 
     the three and compiles them together, looking for overlaps. Codebooks are 
     stored in the `data/` folder.
  01_merging: reads in `data/codebook_3panel.csv` and the RDS files from the 
     Dropbox. It uses the codebook to select only the columns that overlap 
     across all 15 years and then also identified the region for each respondent
     based on their state.
  02_variable_creation: adds in funding and producer data to the organic 
     producer data, as well as adds in a few new columns. This is importantly 
     where I identify the `maincrop` variable of a producer based on how they 
     report their total acreage and then acreage by crop, looking for the crop 
     that has the largest  proportion. The funding data was added such that 
     average annual funding was calculated for time spans of 5 years prior to 
     each survey year, by location. Producer data was used to add some regional-
     level seed producer attributes like the number of seed producers, median 
     income, and total sum of the challenges they express.  
  03_innovation_level_data_summary: will draw from other repo... and combine
     with figure
  04_grower_level_data_summary: summary of individual behavior + figures  
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
  05_models_DVselfreport: adapts the better-fitting models from the acreage 
     dependent variable and uses those to fit a model for the dependent 
     variables of self reporting increasing organic seed usage. Currently this 
     script doesn't do much other than run them.  
  06_models_final` is more or less a repeat of the previous 04 scripts, but 
     focused in on the few models that matter and saving them into `stargazer` 
     tables, stored in the `tables/` folder. In this script I begin to start 
     looking at predictive probabilities based on certain specifications to 
     visualize the effect of our key variables of interest. 
```

### Data flow:    

1. From the `Dropbox/SOSInteractiveSurvey/` read in `data2011_cleaned.rds` + `data2016_cleaned.rds` +  `data2021_cleaned.rds` into `01_merged.R`. Merged outputs saved into `Box/organicseed_adoption/` as `merged_df_3panel.csv`
1. `02_variable_creation.R` reads in `merged_df_3panel.csv`, as well as funding data from `Dropbox/SOSInteractiveSurvey/local_data/`, `public-funding-Dec21.csv`, and seed producer data from `Box/osa_networks/`, `data_combined_3/surveys_combined.csv`, to combine with the producer data and create any new variables, saved as `Box/organicseed/df.RDS` and `Box/organicseed/df.long.RDS`  
1. The `df.RDS` and `df.long.RDS` are the main data frames used for analysis