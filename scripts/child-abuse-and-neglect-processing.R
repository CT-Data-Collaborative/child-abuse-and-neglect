library(dplyr)
library(datapkg)
library(tidyr)
library(stringr)

##################################################################
#
# Processing Script for Child Abuse and Neglect
# Created by Jenna Daly
# On 10/16/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
DD_file <- dir(path_to_raw, recursive=T, pattern = "DD.csv")
caan_file <- dir(path_to_raw, recursive=T, pattern = "CT_DCF")

#Bring in raw data set and DD to rename columns
caan_df <- read.csv(paste0(path_to_raw, "/", caan_file), stringsAsFactors=F, header=T, check.names=F)
dd_df <- read.csv(paste0(path_to_raw, "/", DD_file), stringsAsFactors=F, header=T, check.names=F)

#Convert wide to long
caan_df_long <- gather(caan_df, Type, Value, 6:34, factor_key=FALSE)

#Merge in DD
caan_df_long_with_desc <- merge(caan_df_long, dd_df, by.x="Type", by.y="COLUMN NAME", all=T)

#Break out columns based on Description column
caan_df_long_with_desc$`Allegation Type` <- "All"
caan_df_long_with_desc$`Allegation Type`[grep("Physical Abuse", caan_df_long_with_desc$Description)] <- "Physical Abuse"
caan_df_long_with_desc$`Allegation Type`[grep("Educational Neglect", caan_df_long_with_desc$Description)] <- "Educational Neglect" 
caan_df_long_with_desc$`Allegation Type`[grep("Emotional Neglect", caan_df_long_with_desc$Description)] <- "Emotional Neglect"
caan_df_long_with_desc$`Allegation Type`[grep("High Risk Newborn", caan_df_long_with_desc$Description)] <- "High Risk Newborn"
caan_df_long_with_desc$`Allegation Type`[grep("Medical Neglect", caan_df_long_with_desc$Description)] <- "Medical Neglect"
caan_df_long_with_desc$`Allegation Type`[grep("At Risk", caan_df_long_with_desc$Description)] <- "At Risk"
caan_df_long_with_desc$`Allegation Type`[grep("Physical Neglect", caan_df_long_with_desc$Description)] <- "Physical Neglect"
caan_df_long_with_desc$`Allegation Type`[grep("Sexual Abuse", caan_df_long_with_desc$Description)] <- "Sexual Abuse"

caan_df_long_with_desc$`Measure Type` <- "Number"
caan_df_long_with_desc$`Measure Type`[grep("Rate", caan_df_long_with_desc$Description)] <- "Rate"

caan_df_long_with_desc$`Report Status` <- NA
caan_df_long_with_desc$`Report Status`[grep("Accepted", caan_df_long_with_desc$Description)] <- "Accepted"
caan_df_long_with_desc$`Report Status`[grep("Substantiated", caan_df_long_with_desc$Description)] <- "Substantiated"
caan_df_long_with_desc$`Report Status`[grep("Substantiation", caan_df_long_with_desc$Description)] <- "Substantiated"
caan_df_long_with_desc$`Report Status`[grep("Allegations$", caan_df_long_with_desc$Description)] <- "Alleged"

#Remove unneeded rows
caan_df_long_with_desc <- caan_df_long_with_desc[!grepl("Reports", caan_df_long_with_desc$Description),]
caan_df_long_with_desc <- caan_df_long_with_desc[!grepl("unique", caan_df_long_with_desc$Description),]
caan_df_long_with_desc <- caan_df_long_with_desc[!(is.na(caan_df_long_with_desc$FISC_PERIOD)),]

#Keep columns we need
caan_df_long_with_desc <- caan_df_long_with_desc %>% 
  select(TOWN, -REGION, FISC_YEAR, `Allegation Type`, `Report Status`, `Measure Type`, Value)

#Set suppressed values
caan_df_long_with_desc$Value[caan_df_long_with_desc$Value == "-"] <- NA
caan_df_long_with_desc$Value[grep("<=", caan_df_long_with_desc$Value)] <- NA

#Rearrange df to calcuate rates for all allegation types
caan_df_calc <- caan_df_long_with_desc %>% 
  filter(`Measure Type` == "Number") %>% 
  spread(`Report Status`, Value) %>% 
  mutate(`Sub Rate` = round((as.numeric(Substantiated)/as.numeric(Alleged))*100, 2)) 

caan_df_calc_long <- gather(caan_df_calc, `Report Status`, Value, 5:7, factor_key=FALSE)

#Assign MT and RS accordingly
caan_df_calc_long$`Measure Type`[caan_df_calc_long$`Report Status` == "Sub Rate"] <- "Rate"
caan_df_calc_long$`Report Status`[caan_df_calc_long$`Report Status` == "Sub Rate"] <- "Substantiated"

#Merge in fips by town
caan_df_calc_long$TOWN <- str_to_title(caan_df_calc_long$TOWN)

#Rename Statewide to Connecticut
caan_df_calc_long$TOWN[caan_df_calc_long$TOWN == "Statewide"] <- "Connecticut"

town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

caan_df_calc_long_fips <- merge(caan_df_calc_long, fips, by.x = "TOWN", by.y = "Town", all.y=T)
caan_df_calc_long_fips$Variable <- "Child Abuse and Neglect"
caan_df_calc_long_fips$Value <- as.numeric(caan_df_calc_long_fips$Value)


#Select and arrange columns
caan_df_calc_long_fips <- caan_df_calc_long_fips %>% 
  select(TOWN, FIPS, FISC_YEAR, `Allegation Type`, `Report Status`, `Measure Type`, Variable, Value) %>% 
  rename(Town = TOWN, Year = FISC_YEAR) %>% 
  arrange(Town, Year, `Allegation Type`, `Report Status`, `Measure Type`)

# Write to File
write.table(
  caan_df_calc_long_fips,
  file.path(getwd(), "data", "child_abuse_neglect_2017.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)



