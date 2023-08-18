## Organic seed policy 

This repository houses code for the draft of CH1: *Environmental policy flexibility is a double-edged sword: The case of the seed loophole in certified organic production*. 

### Data storage:

The data of focus in this repository are individual-level data based on Organic Seed Alliance's State of Organic Seed organic grower surveys (2011, 2016, 2021). These data are initially stored in a shared DropBox/SOSInteractiveSurvey (owned by JZ), then revised version are moved to Box/organicseed_adoption (owned by LW). Codebooks for what is in the data are available in the `data/` folder of this repository. Efforts to make sure all relevant matches were made across survey cross-sections was done by first looking at `codebook_columns.csv` and making manual edits in `codebook_columns_manual.csv`. I then looked for matching questions across codebooks and matched up those for 2-panel and 3-panel matches. In the end, I rely on `codebook_3panel.csv` but make some manual additions.


### Code file structure:
```
    code/
      00_creating_codebook: looks at the three organic producer survey data sets
         (2011, 2016, 2021), and extracts the column titles and question text from 
         the three and compiles them together, looking for overlaps. 
      01_merging: reads in `data/codebook_3panel.csv` and the RDS files from the 
         Dropbox. It uses the codebook to select only the columns that overlap 
         across all 15 years and then also identifies the region for each respondent
         based on their state.
      02_variable_creation: adds in a few new columns for accomodating modelling. 
         This is importantly where I identify the `maincrop` variable of a producer 
         based on how they report their total acreage and then acreage by crop, 
         looking for the crop that has the largest  proportion. I also experiment 
         with factor analysis of challenges. I end with a df.RDS where main crops are
         used only, as well as a df_long.RDS which duplicates responses across all
         of a respondents answers (with relevant modifications to align with crop-
         specific responses). For now we use just the df.RDS.
      03_data_summary: using cleaned data from the previous script, I create 
         tables summarizing key variables of (Table 2). Also unused summary figures.
      04_map_making: generates maps to visualize how the data were sampled and 
         how they can represented at the national/regional scale (Figure 1)
      05_scaling_data: scaling relevant variables for modelling
      06_models_beta: runs final beta regression model in INLA and generates
         coefficient plot (Figure 2) and predicted probabilities (Figure 3). 
         Model comparison is in supplemental/
      functions: functions for summarizing model data and plotting
      supplemental/
      scraps/
         models_DVacreage_exploration: Currently unused. It used to fit a few 
           different models using the DV of percent acreage. These include acreage 
           of `maincrop` in the df  dataframe, identified in the variable_creation
           script, and of every crop that a producer grows, using df.long. 
         models_final_multilevel: Based on S5, I went with the Bayesian linear 
           model and ull in functions from the functions_model.R script to run 
           the model and generate Figures 5 (which is really S8)
    ch1-manuscript.Rmd: this is where I source my dissertation chapter from. The
        final dissertation chapter will live and die here. 
```
