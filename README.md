## Wood, Zystro and Hubbard  

This repository houses code for the paper: *Flexible regulation as a double-edged sword: The case of the organic seed standard*  

### Data storage:

The data used in these analyses are individual-level data based on Organic Seed Alliance's State of Organic Seed organic grower surveys (2011, 2016, 2021). These data are stored in a shared DropBox/SOSInteractiveSurvey (owned by JZ), then revised version are moved to Box/organicseed_adoption (owned by LW). Codebooks for what is in the data are available in the `data/` folder of this repository. Efforts to make sure all relevant matches were made across survey cross-sections was done by first looking at `codebook_columns.csv` and making manual edits in `codebook_columns_manual.csv`, which is used to create `codebook_3panel.csv`.


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
      01x_aca_variable_creation: identifies respondent by their certifier, where 
         possible. Because this is private information we keep this script and
         related data hidden, and in the end include only certifier codenames in
         the related analysis (this step prompted by reviewer comments). Here I also
         explore certifier 'population'  
      02_variable_creation: adds in a few new columns for accomodating modelling. 
         This is importantly where I identify the `maincrop` variable of a producer 
         based on how they report their total acreage and then acreage by crop, 
         looking for the crop that has the largest  proportion. I also experiment 
         with factor analysis of challenges. I end with a df.RDS where main crops are
         used only, as well as a df_long.RDS which duplicates responses across all
         of a respondents answers (with relevant modifications to align with crop-
         specific responses). For this paper we use only the df.RDS.
      03_data_summary: using cleaned data from the previous script, I create 
         tables with hypotheses (Table 1) and summarizing key variables of 
         (Table 2). Also unused summary figures.
      04_map_making: generates maps to visualize how the data were sampled and 
         how they can represented at the national/regional scale (Figure 1)
      05_scaling_data: scaling relevant variables for modelling
      06_models_beta: runs final beta regression model in INLA and generates
         coefficient plot (Figure 2) and predicted probabilities (Figure 3). 
         Model comparison is in supplemental/
      functions: functions for summarizing model data and plotting
      appendices/
        01_sample_v_usda_census: brings in USDA census data with tidyUSDA to
        identify N operations and acreage for organic farmers across the US, and
        compare our sample to that. The main challenge here is that the census
        only gives total acreages and N operations per state, so we are unable to
        really identify farm size distribution, only a crude mean. Creates 
        Appendix Tables A1 and A2. Unused is a more crop-specific look, because
        only possible for field and vegetable crops. 
        02_model selection: compares several different model builds to justify
        model selection. At once point ran through with both INLA estimation and
        betareg function, settling for betareg
```
