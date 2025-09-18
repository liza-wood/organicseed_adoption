main_analyses <- c('code/02_variable_creation.R',
                   'code/03_data_summary.R',
                   'code/04_map_making.R',
                   'code/05_scaling_data.R',
                   'code/06_models_beta.R')
sapply(main_analyses, source)

suppl_analyses <- c('code/appendices/01_sampling_method.R',
                    'code/appendices/02_data_collection_model.R',
                    # Excluding right now because tidyUSDA stopped working
                    #'code/appendices/03_sample_v_usda_organic_census.R',
                    'code/appendices/04_farmsize_representativeness_usda.R',
                    'code/appendices/06_models_timeperiod.R',
                    'code/appendices/07_other_barriers.R')
sapply(suppl_analyses, source)

reviewer_response <- c('code/review/01_explore_split_ops.R')
sapply(reviewer_response, source)

rmarkdown::render('code/appendices/00_compiled_appendix.Rmd',
                  knit_root_dir = '~/Documents/Davis/R-Projects/organicseed_adoption')
