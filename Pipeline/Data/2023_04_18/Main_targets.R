##################################################################
# This is the main script that will execute all the sub-functions 
# and create a summary using R-markdown. 
# This script also has the function to import the newest data-versions
# of all the different data-sources.
##################################################################

##########################
# Set working directory and UserID
##########################

#Set location of pipeline
my_directory <- "C:/Users/THGJ/Documents/ConsumerData - Github/Pipeline"
setwd(my_directory)

#Load necessary packages
source("Code/packages.R")


#Select the sub-directory from the desired Date
DateofData <- as.Date("2023-04-18")

file_path <- file.path(my_directory,"Data","2023_04_18") 
setup_user_date_directory<-data.frame("my_directory" = my_directory,
                                      "DateofData" = DateofData , "file_path" = file_path)

##  you can set this to previous date to access old code
setwd(file_path)
readr::write_csv(setup_user_date_directory,file.path(file_path,"setup_user_date_directory.csv"))

tar_manifest(fields = all_of("command"))
  
system.time(tar_make())




#####################################
# Call markdown to produce summary pdf
######################################
rmarkdown::render("Results_Word.Rmd", params = list(DateofData = DateofData,
                                                    cpd_data = tar_read(cpd_participants),
                                                    cpd_purch = tar_read(cpd2),
                                                    my_directory = my_directory))

library(officer)
file <- tempfile(fileext = ".docx")
doc <- read_docx("Results_Word.docx")
doc2 <- body_add_docx(doc, "Tables/Table s3_KJ.docx")
doc4 <- body_add_docx(doc2, "Tables/Table s3_protein.docx")
doc5 <- body_add_docx(doc4, "Tables/Table s3_fat.docx")
doc6 <- body_add_docx(doc5, "Tables/Table s3_carbohydrates.docx")
doc_final <- body_add_docx(doc6, "Tables/Table 2.docx")

print(doc_final, target = "Results_Word_tables.docx")


