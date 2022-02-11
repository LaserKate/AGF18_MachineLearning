# AGF18_MachineLearning

Project title: Finding Heat tolerant corals for selective breeding

Description of project goals: Characterize reef environments, calculate survival for larvae and juveniles from heat stress experiments, create BRTs to predict environments for high, heritable survival, map them to GBR locations

Notes: 
All required packages and the versions (especially for BRT section in Script 3) are listed. 

----

#Script 1 "eReefs extract_Githubtidy_Final.Rmd"

Files:
#Temperature data--
eReefs2014_2020_AGFReefs.csv #too large for Github, email me or download yourself directly from the "eReefs extractor tool" (https://extraction.ereefs.aims.gov.au/) for these reefs listed in map file available from Script 2
GBRONLYSSTAExport_Output_csv.csv
Export_OutputDHW.txt #this is also too large for Github. You can download directly from cortad via ftp (link in script). 
ARCGIS_KQuigley_Satdata_AGF18sites_CSVlatlong.csv

#Relative Importance--
RelativeImportance_summaryAGF18.csv

#Find the reefs--
#in zipped folder together "DRT and TS files zipped all GBR Github.zip"
#Alternatively, they can all be extracted using the "eReefs extractor tool" (https://extraction.ereefs.aims.gov.au/) for all reefs across hourly or daily intervals

2006.8591169-collected.csv
2006.db53de4-collected.csv
2005.5e5e11c-collected.csv
2005.380fbd9-collected.csv

----

#Script 2 "AGF2018_larval_heatstress_Githubtidy_Final.R"

Files:
AGF18.Sites_Github.csv
larvaldataAGF2018_Github.csv
AGF_yr2_Juvs_cur.singles_aliveatT1.csv #only single juveniles, clumps of juveniles removed
larv_juv_comparision_combo.csv

#Sections include: 
map,
larval survival,
juvenile survival,
juvenile survival by symbiont,
Supp figures

----

#Script 3 "Prediction_AGF18_githubtidy_NatCommschecks_v2_Final.R"

Files:
larv6.csv #Download the larval data with satellite data attached appended----
juv6.6_subset.zip #unzip for csv

-----

Supplementary Material

#Best to follow the original script https://github.com/LaserKate/MontiSymTransgen with full annotation but the version used for this analysis can be found in the script above

#Script 4 ITS2_test_analysis_KQ_AGFJuveniles2018_Githubtidy.Rmd

Files:
adaptors.fasta
arif_ITS2_DB_mod.fasta
Juveniles_AGF18_Phyloseq_Metadata.csv
ITS2_AGF18_Ariftax_Edit.csv

Source files can be requested as well but are too large for Github to host. However, all raw data is available on Github.
save30.11.21.Rdata (zip file) - 691.MB
retesting_BRTs_April2021.RData (zip file) - 78.6 MB

