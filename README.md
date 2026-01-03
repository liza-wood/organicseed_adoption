
This repository houses code for the paper: *Compliance in regulatory gray areas: The case of the organic seed standard*  

### File structure:
```
00_implement_workflow: runs the main paper code, appendix sections, and reviewer
   response supplements
code/
  00_creating_codebook: looks at the three organic producer survey data sets
     (2011, 2016, 2022), and extracts the column titles and question text from 
     the three and compiles them together, looking for overlaps. 
  01_merging: reads in `data/codebook_3panel.csv` and the RDS files from the 
     Dropbox, which are survey responses cleaned by JZ. It uses the codebook
     to select only the columns that overlap across all 3 surveys and then 
     also identifies the region for each respondent based on their state.
  02_variable_creation: adds in a few new columns for accomodating modelling. 
     This is where we identify the `maincrop` variable of a producer 
     based on how they report their total acreage and then acreage by crop, 
     looking for the crop that has the largest  proportion. We end with a 
     df.RDS where main crops are used only, as well as a df_long.RDS which
     duplicates responses across all of a respondents answers (with relevant
     modifications to align with crop-specific responses). For this paper 
     we use only the df.RDS.
  03_data_summary: using cleaned data from the previous script, we create 
     tables with hypotheses (Table 1) and summarizing key variables of 
     (Table 2). Also unused summary figures.
  04_map_making: generates maps to visualize how the data were sampled and 
     how they can represented at the national/regional scale (Figure 1)
  05_scaling_data: scaling relevant variables for modelling
  06_models_beta: runs final beta regression model in betareg and generates
     model results (Table 3) and predicted probabilities (Figure 2). 
  07_comments: pulls out open comments that help unpack / explain the 
     difference between spirit and letter 
  functions: functions for summarizing model data and plotting
  appendices/
    00_compiled_appendix: brings together results from the scripts in this
       directory to create the appendix. 
    01_sampling_method: includes data collection method for descriptive
       purposes to make Table S1 comparing convenience and random sampling
    02_data_collection_models: models whether or not data collection method 
       or distributor (e.g. OSA) affects model results, Table S2 
    03_sample_v_usda_organic_census: brings in USDA census data with tidyUSDA 
       to identify N operations for organic farmers across the US regions and 
       compare our sample to that. (Could also do by crop, but this gets
       messier). Makes Table S3
    04_farmsize_representativeness_usda: uses the USDA organic tabulations
       (not in the API) from usdaoRganic to get information on farm size
    05_model selection: compares several different model builds to justify
       model selection, and comparing betareg to INLA. Not using in appendix. 
    06_models_timeperiod: runs each model by year to think through question
       wording variation  
    07_other_barriers: models the barriers disaggregated between allowable/
       non-allowable, and by each barrier individually 
    08_mediation_figures: exploring mediation analysis. Not included in 
       appendix
    09_mediation: exploring mediation analysis. Not included in appendix  
    10_certifier_models: adds (when available) the certifying agency of the
       producer to consider effects. Not included in appendix.
```

<!---
  review/ - ignored in the git repo  
  
  scraps/
      01x_aca_variable_creation: identifies respondent by their certifier, where 
     possible. Because this is private information we keep this script and
     related data hidden, and in the end include only certifier codenames in
     the related analysis. We used certifier codenames in a supplemental
     analysis, though this is not included in the final appendix.
---> 

### Data storage:

The data used in these analyses are individual-level data based on Organic Seed Alliance's State of Organic Seed organic grower surveys ([2011](https://seedalliance.org/publications/state-of-organic-seed-report/), [2016](https://seedalliance.org/publications/state-of-organic-seed-2016/), [2022](https://stateoforganicseed.org/state-of-organic-seed-2022/)). These data are stored in a shared DropBox/SOSInteractiveSurvey (owned by JZ), then revised version are moved to Box/organicseed_adoption (owned by LW). Codebooks for what is in the data are available in the `data/` folder of this repository. Efforts to make sure all relevant matches were made across survey cross-sections was done by first looking at `codebook_columns.csv` and making manual edits in `codebook_columns_manual.csv`, which is used to create `codebook_3panel.csv`.