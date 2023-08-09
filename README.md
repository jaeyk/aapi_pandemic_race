# Replication code for Race in a Pandemic: Asian American Perceptions of Discrimination and Political Mobilization in the 2020 Election 

* Author: Vivien Leung, Jae Yeon Kim, Natalie Masuoka
* Last updated: Aug 5, 2023

## Session information 

* R version 4.2.2 Patched (2022-11-10 r83330)
* Platform: x86_64-pc-linux-gnu (64-bit)
* Running under: Ubuntu 23.04

## Data 

* The cleaned dataset is available [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/JEJQ4L)

## Replication code 

- [Data wrangling](https://github.com/jaeyk/asa_panel_data/blob/main/code/01_data_munging.Rmd)
    - This code constructs the following dataset: `panel_data.csv`

- [Data analysis with imputation](https://github.com/jaeyk/asa_panel_data/blob/main/code/02_desc_analysis_imputed.Rmd)
    - This code reproduces the following figures and tables:
        - Figures: figure1, figure2, figure_b1, figure_b2  
        - Tables: table2, table4, table5, table6, table_c4 

- [Data analysis without imputation](https://github.com/jaeyk/asa_panel_data/blob/main/code/02_desc_analysis_none.Rmd)

- [Additional descriptive analysis](https://github.com/jaeyk/asa_panel_data/blob/main/code/02_desc_analysis_subgroup.Rmd)
    - This code reproduces the following figures and tables:
        - Figures: figure3
        - Tables: table3, table_c1, table_c3