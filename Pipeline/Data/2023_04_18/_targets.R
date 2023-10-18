# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint
library(targets)
# Set target options:
tar_option_set(
  packages = c("tidyverse","janitor","data.table","ggplot2", "readr", "knitr", "stringr","qwraps2","openxlsx","readxl",
               "DBI", "conflicted", "svglite", "DescTools", "wordcloud2", "pacman", "odbc","bookdown",
               "lubridate", "psych", "corrplot", "car", "lubridate", "SMLE", "kableExtra", "rsvg", "flextable",
               "DescTools","tidyr", "parallel", "DiagrammeR", "DiagrammeRsvg", "gt", "gapminder", "spatstat","Hmisc", "RColorBrewer"),
  # packages that your targets need to run
  format = "rds", # default storage format
  error = "null"
  # Set other options as needed.
)

#---------------------------------------------------------------------------------------------------
# Set conflict prefer 
#---------------------------------------------------------------------------------------------------
conflicted::conflict_prefer("select", "dplyr") 
conflicted::conflict_prefer("filter", "dplyr")
#> [conflicted] Will prefer dplyr::filter over any other package
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("first", "dplyr")
conflicted::conflict_prefer("summarize", "dplyr") 
conflicted::conflict_prefer("summarise", "dplyr") 

#> [conflicted] Will prefer dplyr::lag over any other package


# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

##tar_helper()

# Run the R scripts in the R/ folder with your custom functions:

# source("other_functions.R") # Source other scripts as needed. # nolint

setup_user_date_directory<-read.csv(file.path(getwd(),"setup_user_date_directory.csv"))

my_directory<-setup_user_date_directory$my_directory
UserID <- setup_user_date_directory$UserID
DateofData <- setup_user_date_directory$DateofData
file_path <- setup_user_date_directory$file_path

setwd(file_path)

datasource_priority_lists <- c("gs1", "Kemiluppen","openfoodfacts", "frida", "purchased_itemname")


target.vars <- c("itemnumber", "itemname", "productname", "group","KJ", "protein", "fat","carbohydrates","weight", "weight_unit",
                 "food_list","chem_list")

old.names_kemiluppen <- c("gtin", NA, "produktnavn", "kategori", NA, NA, NA, NA, NA, NA, NA,  "ingredientstatement")

old.names_gs1 <- c("gtin", NA, "full_name", "classtitle", "ener-", "pro-", "fat", "choavl", "netweight",  "netweightmeasurementunitcode",
                   "ingredientstatement", NA)

old.names_frida <- c("itemnumber", "itemname", "navn",  "gruppe", "energy_kj", "protein", "fat",
                     "available_carbohydrates", NA,  NA, NA, NA)

old.names_OFF <- c("code", NA, "product_name", "categories_en", "energy_100g", "proteins_100g", "fat_100g", "carbohydrates_100g", "weight", "weight_unit", "ingredients_text", NA)


tar_source(file.path(file_path,"all_functions.R"))

# Replace the target list below with your own:
list(
  tar_target(name = cpd_data, command = file.path("ConsumerReceiptData_Simulated.csv"), format = "file"),
  tar_target(name = matchscore_cutoff, command = 6),
  tar_target(name = gpc_path, command = file.path("GPC_November_2021.csv"), format = "file"),
  tar_target(name = frida_match_data, command = file.path(my_directory,"Data/Common","SMIL-frida_match_2023_09_06.xlsx"), format = "file"),
  tar_target(name = gs1_path, command = file.path("gs1_ssi.csv"), format = "file"),
  tar_target(name = open_food_facts_link, command = file.path("https://static.openfoodfacts.org/data/en.openfoodfacts.org.products.csv.gz")),
  tar_target(name = kemiluppen_data, command = file.path(my_directory,"Data/Common", "kemiluppen_ssi_2022.xlsx"), format = "file"),
  tar_target(name = fg_path, command = file.path(my_directory,"Data/Common","nfg_tabel_rev_frtm.xlsx"), format = "file"),
  ##enrich cpd
  tar_target(
    name = cpd,
    command = cpd_import(cpd_data)),
  tar_target(
    name = cpd2,
    command = fix_weight_in_cpd_amount(cpd)),
  tar_target(
    name = cpd_proddata,
    command = create_weight_volume_eco_country_vars(cpd2)),
  tar_target(
    name = itemname_datasource,
    command = cpd_itemname_measure(cpd_proddata)),
  #######################################
  #Import GS1 and GPC and combine the two
  #######################################
  tar_target(
    name = gs1_gpc,
    command = gs1_gpc_import(gs1_path, gpc_path)),
  # tar_target(
  #   name = gpc_datasource,
  #   command = gpc_import(gpc_data)),
  tar_target(
    name = gs1_datasource,
    command = join_gpc_data_to_gs1(gs1_gpc, old.names = old.names_gs1, target.vars = target.vars)),
  tar_target(
    name = new_frida_group_names,
    command = newfoodgroups(data = frida_match, fg_path = fg_path)),
  tar_target(
    name = frida_match,
    command = import_produkt_and_frida_stems(frida_match_data)),
  # tar_target(
  #   name = cpd_tied_pruned,
  #   command = evaluate_overlap_in_frida_product_stems(frida_match)),
  tar_target(
    name = frida_match2,
    command = enrich_cpd_with_frida_data_new(frida_match, cpd_proddata)#,deployment = "worker"
  ),
 tar_target(
   name = frida_datasource,
   command = match_results(frida_match2, matchscore_cutoff = matchscore_cutoff,
                                old.names = old.names_frida, target.vars = target.vars)#,deployment = "worker"
 ),
  tar_target(
    name = kemiluppen_datasource,
    command = import_kemiluppen(data = kemiluppen_data, old.names = old.names_kemiluppen, target.vars),##deployment = "worker"
  ),
  tar_target(
    name = openfoodfacts,
    command = download_open_food_facts(open_food_facts_link),##deployment = "worker"
  ),
  tar_target(
    name = open_food_facts_datasource,
    command =  openfoodfacts_clean(openfoodfacts, target.vars = target.vars, old.names = old.names_OFF),##deployment = "worker"
  ),
  tar_target(
    name = final_enrich_datasource,
    command = combine_datasources(kemiluppen_datasource, gs1_datasource, frida_datasource,itemname_datasource,
                                  open_food_facts_datasource,new_frida_group_names = new_frida_group_names )
  ),
  #tar_target(
  #  name = final_enrich_datasource,
  #  command = select_datasources(combined_datasources)
  #),
  tar_target(
    name = cpd_enriched,
    command = cpd_join_datasources(cpd_data = cpd_proddata, datasources = final_enrich_datasource)
  ),#,
  tar_target(
    name = cpd_participants,
    command = cpd_join_datasources(cpd_data = cpd2, datasources = final_enrich_datasource)
  ),
  tar_target(
    name = cpd_enriched_final,
    command = cpd_enriched_clean(cpd_enriched = cpd_enriched,cpd_original = cpd2,
                                 gs1_gpc = gs1_gpc, new_frida_group_names = new_frida_group_names)
  ),
  tar_target(
    name = flowchart_data1,
    command = create_flowcharts(cpd_participants,kemiluppen_datasource,gs1_datasource,frida_match, open_food_facts_datasource)#(final_enrich_datasource,kemiluppen_ssi,gs1_ssi3,demografi2,frida_match,demografi_disease)
  ),
  tar_target(
    name = Nutri_plot,
    command = nutricient_fun(cpd_enriched,cpd_original = cpd_enriched_final, frida_match, off_df = open_food_facts_datasource,
                             gs1_df = gs1_datasource)#(final_enrich_datasource,kemiluppen_ssi,gs1_ssi3,demografi2,frida_match,demografi_disease)
  ),
  tar_target(
    name = Items_cumu,
    command = Items_cumu_plot(cpd_original = cpd_enriched_final)
    ),
 tar_target(
   name = StoreInfluence_table,
   command = StoreInfluence(cpd = cpd_enriched_final)
 ))


