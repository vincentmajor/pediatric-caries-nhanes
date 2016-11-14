## Vincent Major
## Health Informatics
## Nov 1 2016

if(!require(RNHANES)){install.packages('RNHANES');library(RNHANES)}
if(!require(tidyverse)){install.packages('tidyverse');library(tidyverse)}

#### Loading data ----
## Dye and colleagues used 2011-2012 data

## Check if a data/ directory exists 
if(!dir.exists("data")){
  dir.create("data")
  writeLines("Created a data/ directory to store the data")
  }

## Load list of all availible files 
if(length(list.files("data")) == 0){
  files <- nhanes_data_files(destination = "data", cache = TRUE)
  ##variables <- nhanes_variables()
  
  ## Search for data in 2011-2012 cycle
  files.2011 = nhanes_search(files, "", cycle == "2011-2012")
  
  ## reduce this list to ones we want
  files.dye = files.2011[files.2011$data_file_name %in% c('DEMO_G', 'OHXDEN_G', 'OHXPER_G', 'OHXREF_G'),]
  
  ## Download the data - should only do once!
  data.dye.raw = nhanes_load_data(files.dye$data_file_name, files.dye$cycle, destination = "data", cache = TRUE)
}

temp.files = setdiff(list.files("data"), "nhanes_data_files.csv")

# manually coded for now
#data.demo = read_csv(file.path("data", temp.files[1]))
data.ohxden.raw = read_csv(file.path("data", temp.files[2]))
#data.ohxper = read_csv(file.path("data", temp.files[3]))
#data.ohxref = read_csv(file.path("data", temp.files[4]))

#data.test = dplyr::left_join(data.ohxden, data.demo)

#### aggregating oral health examination into caries or not ----
## http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/OHXDEN_G.htm
## require complete examination status and dentition status
data.ohxden = filter(data.ohxden.raw, OHDEXSTS == 1, OHDDESTS == 1) # n = 8956 --> 8073

## next column is dental implant question - not needed
## drop the two above status columns, the implant and the final four filename, cycle and year columns
data.ohxden = select(data.ohxden, -OHDEXSTS, -OHDDESTS, -OHXIMP, -(file_name:end_year)) # remove 3 columns

## OHX01TC Tooth count. We don't care about primary/missing/permanent right?
## apparently we do so I will flag those that are primary as a separate count
#data.ohxden = select(data.ohxden, -(OHX01TC:OHX32TC)) # remove 32 columns from 124 to 103

## Coronal caries: Tooth count
## going to assume that a missing due to dental disease counts as an enormous cavity
## flags for missing by dental disease: E, P, R
## for columns OHX02CTC:OHX31CTC flag any EPR


missing.values = c("E", "P", "R")
permanent.missing.mask = select(data.ohxden, SEQN, OHX02CTC:OHX31CTC) %>% 
  gather(tooth, value, OHX02CTC:OHX31CTC) %>% 
  mutate(flag = value %in% missing.values) %>% 
  group_by(SEQN) %>% 
  summarize(flag = any(flag))

# clean up
remove(missing.values)

print(mean(missing.mask$flag)) # 33.6% of people are missing > 1 tooth

## Could filter on this flag but will keep to combine with caries.
#data.ohxden = slice(data.ohxden, !missing.mask$missing_flag) # 8073 --> 5360

## Done with those columns, remove them
data.ohxden = select(data.ohxden, -(OHX02CTC:OHX31CTC)) # remove 28 columns from 103 to 75
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
  mutate(flag = !is.na(value)) %>% 
  group_by(SEQN) %>% 
  summarize(flag = any(flag)) ## any non-blank means > 1 caries

print(mean(caries.mask$flag)) ## 66.5% of people have caries/had restoration.
## manual review of 62161=TRUE and 62162=FALSE where 62161 did have non-blank value in column OHX18CSC

## clean up by removing columns from dat.ohxden
data.ohxden = select(data.ohxden, -(OHX02CSC:OHX31CSC)) # drop 28 columns from 75 to 47

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

print(mean(sealant.mask$flag)) # 11.7% have sealant on one or more teeth. 

## clean up by removing nosealant.values and columns from dat.ohxden
remove(nosealant.values)
data.ohxden = select(data.ohxden, -(OHX02SE:OHX31SE)) #drop 18 columns from 47 to 29

#------------------------------------------------------------------------------
## Fluorosis Deans Index is the last columns
## Not interested in this right now, drop them
data.ohxden = select(data.ohxden, -(OHX02DI:OHX31DI)) #drop 28 from 29 to 1

## Left with only the SEQN column
## add in the masks now
## order is preserved, check with #all(caries.mask$SEQN == data.ohxden$SEQN)
data.ohxden$missing_flag = missing.mask$flag
data.ohxden$cavity_flag = caries.mask$flag
data.ohxden$sealant_flag = sealant.mask$flag


#### Pulling in demographics data ----
## Load from temp.files[1] - not reliable
data.demo.raw = read_csv(file.path("data", temp.files[1]))

## column SEQN is the responsent number - keep
## column RIAGENDR is gender 1=M, 2=F - keep
## column RIDAGEYR is age in yr - keep
## column RIDAGEMN is age in months < 24 - don't need these resolution
## column RIDETH1/3 is ethnicity with and without 6 = asian
# 1 mexican american
# 2 other hispanic
# 3 white
# 4 black
# 5 other (in RIDETH1)
# 6 asian
# 7 other (in RIDETH3)
## only keep RIDETH3
## DMDBORN4 is country of birth with USA=1
## DMDCITZN is citizenship with USA=1
## SIALANG is language spoken in interview, english=1, spanish all others.
## a few other fileds contain other language values
## INDHHIN2 annual household income - lots of bins, over, under xxx-yyy, annoying!
## INDFMPIR ratio of family income to poverty, 0-5 with some missing, much easier.
## DMDHHSIZ people in the household 1-7, no missing. 
## DMDFMSIZ people in the family 1-7, no missing.
## DMDHHSZA, DMDHHSZB, and DMDHHSZE are the number of people under 5, 6-17 nad > 60 in household
data.demo = select(data.demo.raw, SEQN, RIAGENDR, RIDAGEYR, RIDRETH3, RIDEXAGY, DMDBORN4, DMDCITZN, SIALANG, INDFMPIR, DMDHHSIZ, DMDFMSIZ, DMDHHSZA, DMDHHSZB, DMDHHSZE) %>%
  mutate(genderF = RIAGENDR == 2, 
         born_usa = DMDBORN4 == 1, 
         citizen = DMDCITZN == 1,
         interview_english = SIALANG == 1, #no missing here
         age = ifelse(is.na(RIDEXAGY), RIDAGEYR, RIDEXAGY)
         ) %>%
  rename(ethnicity = RIDRETH3, 
         #age_exam = RIDEXAGY, 
         income_ratio_over_poverty = INDFMPIR, 
         household_size = DMDHHSIZ,
         family_size = DMDFMSIZ,
         num_children = DMDHHSZA,
         num_adolescents = DMDHHSZB,
         num_elderly = DMDHHSZE) %>%
  select(-RIAGENDR, -DMDBORN4, -DMDCITZN, -SIALANG, -RIDAGEYR, -RIDEXAGY)

## merge into data.ohxden
data.ohxden.demo = left_join(data.ohxden, select(data.demo, SEQN, age, ethnicity))
## bin age into the defined age groups from Dye
agegroups.dye = c(0, 1.9, 5.1, 8.1, 11.1, 15.1, 19.1, 100)
data.ohxden.demo$age = as.integer(cut(data.ohxden.demo$age, breaks = agegroups.dye))

## drop infants and adults, 8073 --> 3300
data.ohxden.demo.pediatric = filter(data.ohxden.demo, age >=2, age<=6) 

agegroups.labels = c('2-5', '6-8', '9-11', '12-15', '16-19')
ethnicity.labels = c('mex', 'hispan', 'white', 'black', 'asian', 'other')

data.ohxden.demo.pediatric$age = factor(data.ohxden.demo.pediatric$age, labels = agegroups.labels)
data.ohxden.demo.pediatric$ethnicity = factor(data.ohxden.demo.pediatric$ethnicity, labels = ethnicity.labels)


summary_caries.age = group_by(data.ohxden.demo.pediatric, age) %>% summarize(n = n(), rate = mean(cavity_flag))
summary_caries.eth = group_by(data.ohxden.demo.pediatric, ethnicity) %>% summarize(n = n(), rate = mean(cavity_flag))  
summary_caries.age.eth = group_by(data.ohxden.demo.pediatric, age, ethnicity) %>%
  summarize(n = n(), rate = mean(cavity_flag))

## Dye actually stratifies by primary and permanent teeth for the 2-5 and 6-8 age groups
## Should we do that too?


## First plot, age 2-5 and 6-8 and all ethnicities
barplot(summary_caries.age$rate, ylim = c(0, 1), names.arg = agegroups.labels)
barplot(summary_caries.eth$rate, ylim = c(0, 1), names.arg = ethnicity.labels)
## Need to work on the plotting to replicate that of Dye

## from ggplot2 documentation of geom_bar
# ggplot(summary_caries.age, aes(age, rate)) + geom_bar(stat = "identity")
# ggplot(summary_caries.eth, aes(ethnicity, rate)) + geom_bar(stat = "identity")

#ggplot(summary_caries.age.eth, aes(age, ethnicity, rate)) + geom_tile(stat = "identity", color = 'red')

# if(!require(viridis)){install.packages('viridis');library(viridis)}
# if(!require(ggthemes)){install.packages('ggthemes');library(ggthemes)}

# gg <- ggplot(summary_caries.age.eth, aes(x=ethnicity, y=age, fill=rate))
# gg <- gg + geom_tile(color="white", size=0.1)
# # gg <- gg + scale_fill_viridis(name="# Events") # not a huge fan of blue -> yellow
# gg <- gg + scale_colour_grey(start = 1, end = 0)
# gg <- gg + coord_equal()
# gg <- gg + labs(x=NULL, y=NULL, title="Rates of Caries in children and adolescents")
# gg <- gg + theme_tufte(base_family="Helvetica")
# gg <- gg + theme(plot.title=element_text(hjust=0))
# gg <- gg + theme(axis.ticks=element_blank())
# gg <- gg + theme(axis.text=element_text(size=7))
# gg <- gg + theme(legend.title=element_text(size=8))
# gg <- gg + theme(legend.text=element_text(size=6))
# gg
## Not happy with the heatmap.
## I am going to revert back to bar charts 
