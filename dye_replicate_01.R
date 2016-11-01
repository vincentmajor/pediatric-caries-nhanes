## Vincent Major
## Health Informatics
## Nov 1 2016

if(!require(RNHANES)){install.packages('RNHANES');library(RNHANES)}
if(!require(tidyverse)){install.packages('tidyverse');library(tidyverse)}

#### Loading data ----
## Dye and colleagues used 2011-2012 data

## Load list of all availible files 
#files <- nhanes_data_files(destination = "data", cache = TRUE)
##variables <- nhanes_variables()

## Search for data in 2011-2012 cycle
#files.2011 = nhanes_search(files, "", cycle == "2011-2012")

## reduce this list to ones we want
#files.dye = files.2011[files.2011$data_file_name %in% c('DEMO_G', 'OHXDEN_G', 'OHXPER_G', 'OHXREF_G'),]

## Download the data - should only do once!
# data.dye.raw = nhanes_load_data(files.dye$data_file_name, files.dye$cycle, destination = "data", cache = TRUE)

temp.files = setdiff(list.files("data"), "nhanes_data_files.csv")

# manually coded for now
#data.demo = read_csv(file.path("data", temp.files[1]))
data.ohxden.raw = read_csv(file.path("data", temp.files[2]))
#data.ohxper = read_csv(file.path("data", temp.files[3]))
#data.ohxref = read_csv(file.path("data", temp.files[4]))

#data.test = dplyr::left_join(data.ohxden, data.demo)

## require complete examination status and dentition status
data.ohxden = filter(data.ohxden.raw, OHDEXSTS == 1, OHDDESTS == 1) # n = 8956 --> 8073

## drop dental implant question
data.ohxden = select(data.ohxden, -OHDEXSTS, -OHDDESTS, -OHXIMP) # remove 3 columns

## OHX01TC Tooth count. We don't care about primary/missing/permanent right?
data.ohxden = select(data.ohxden, -(OHX01TC:OHX32TC)) # remove 32 columns from 128 to 107

## Coronal caries: Tooth count
## going to assume that a missing due to dental disease counts as an enormous cavity
## flags for missing by dental disease: E, P, R
## for columns OHX02CTC:OHX31CTC flag any EPR

missing.values = c("E", "P", "R")
missing.mask = select(data.ohxden, SEQN, OHX02CTC:OHX31CTC) %>% 
  gather(tooth, value, OHX02CTC:OHX31CTC) %>% 
  mutate(missing_flag = value %in% missing.values) %>% 
  group_by(SEQN) %>% 
  summarize(missing_flag = any(missing_flag))

# clean up
remove(missing.values)

## Could filter on this flag but will keep to combine with caries.
#data.ohxden = slice(data.ohxden, !missing.mask$missing_flag) # 8073 --> 5360

## Done with those columns, remove them
data.ohxden = select(data.ohxden, -(OHX02CTC:OHX31CTC)) # remove 28 columns from 107 to 79
# no tooth 1, 16, 17 or 32...

#------------------------------------------------------------------------------
## Coronal Caries: Surface condition for Teeth 2:15, 18:31
## from the document, these values may be recorded as a string variable, for example: "0", "13", "468"
## where "468" is 4, 6, and 8!
## values 0-4 are caries, values 5-9 are restoration, blank is missing but presumably no caries
## whole dataset has missingness for tooth 2 of 6942/8956 = 77.5%. I think it means no caries.

## we want to separate zero caries from one or more caries
caries.mask = select(data.ohxden, SEQN, OHX02CSC:OHX31CSC) %>% 
  gather(tooth, value, OHX02CSC:OHX31CSC) %>% 
  mutate(flag = is.na(value)) %>% 
  group_by(SEQN) %>% 
  summarize(missing_flag = !all(flag)) ## all blank means no caries

mean(caries.mask$missing_flag) ## 66.5% of people have caries/had restoration.
## manual review of 62161=TRUE and 62162=FALSE where 62161 did have non-blank value in column OHX18CSC

#------------------------------------------------------------------------------
## Dental Sealants for teeth 2:5, 7, 10, 12:15, 18:21, 28:31 ## This makes ZERO sense!
## from the document, these values may be recorded as a string variable, for example: "0", "12", "13"
## where "12" is 1, and 2!
## values 0 sealant not present
## values 1-3 is sealant on permanent tooth
## value 4 is sealant on primary tooth
## value 9 is cannot be assessed (1848) and missing (5932 from 8956)
## I am going to assume cannot be assessed and missing == 0 == no sealant.

nosealant.values = c("0", "9")
sealant.mask = select(data.ohxden, SEQN, OHX02SE:OHX31SE) %>% 
  gather(tooth, value, OHX02SE:OHX31SE) %>% 
  mutate(missing = is.na(value), nosealant = value %in% nosealant.values, flag = !(missing | nosealant)) %>% 
  group_by(SEQN) %>% 
  summarize(flag = any(flag))

#test = select(data.ohxden, SEQN, OHX02SE:OHX31SE) %>% gather(tooth, value, OHX02SE:OHX31SE) %>% mutate(missing = is.na(value), nosealant = value %in% nosealant.values, flag = !(missing | nosealant))
#select(test, value:flag) %>% distinct()
## looks good!
## 62161 = FALSE has missing everywhere
## 62168 = TRUE has several values of 1 

mean(sealant.mask$flag) # 11.7% have sealant on one or more teeth. 
