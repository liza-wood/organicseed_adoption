# State of Organic Seed Data, 2011-2021  

## Data storage  
Survey data from the 2011, 2016, and 2021 SOS reports are stored on the shared Dropbox, SOSInteractiveSurvey, and newly created data are stored in the shared Box folder, organicseed, or in the `data/` folder (for codebooks).

## Script file summary  
Inside the `script/` folder are the following scripts:
* `00_creating_codebook` looks at the three survey datasets and extracts the column titles and question text from the three and compiles them together, looking for overlaps. Codebooks are stored in the `data/` folder.
* `01_merging` reads in `data/codebook_3panel.csv` and the RDS files from the Dropbox
* `02_variable_creation` adds in funding and producer data adds in a few new columns  
* `03_data_summary` takes some cursory summary looks. Nothing fancy yet  
* `04_models_DVacreage` develops fits a few different models using the dependent variable of percent acreage. These include acreage of "main crop", identified in the variable_creation script, and of every crop that a producer grows, using df.long, where there are repeated values for some producers that grow more than one crop. For each of these data structures I fit two models: one more basic linear model that includes year and region as fixed effects, and a second mixed effects model where I use year and region as random effects, and can add in the attributes of the year and region (funding and number of producers per region) as attributes of those higher levels. For the df.long models, I also have to include respondentID as a random effect because of the repeated nature of the multiple crops. I compare these models an run a couple diagnostics and **tentatively** think that the main crop dataframe is more predictive than the longer dataframe where producers with multiple crops are repeated. The random and mixed effects models seem to perform similarly, so I keep working with both of them in the following scripts. 
* `04_models_DVselfreport` adapts the better-fitting models from the acreage dependent variable and uses those to fit a model for the dependent variables of self reporting increasing organic seed usage. Currently this script doesn't do much other than run them.  
* `05_models_final` is more or less a repeat of the previous 04 scripts, but focused in on the few models that matter and saving them into `stargazer` tables, stored in the `tables/` folder. In this script I begin to start looking at predictive probabilities based on certain specifications to visualize the effect of our key variables of interest. 

## Workflow  

Read in Dropbox/SOSInteractiveSurvey/data2011_cleaned.rds, ~/Dropbox/SOSInteractiveSurvey/data2016_cleaned.rds, and Dropbox/SOSInteractiveSurvey/data2021_cleaned.rds into 01_merged.R, merged outputs saved as Box/organicseed/merged_df_3panel.csv --> 02_variable_creation.R reads in previous data frame, as well as funding data (Dropbox/SOSInteractiveSurvey/local_data/public-funding-Dec21.csv) and seed producer data (Box/osa_networks/data_combined_3/surveys_combined.csv) to combine with the producer data and create any new variables, saved as Box/organicseed/df.csv and Box/organicseed/df.long.csv --> use these in the following scripts