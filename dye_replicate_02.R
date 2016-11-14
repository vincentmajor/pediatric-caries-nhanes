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

## testing the regex
#temp = colnames(data.ohxden)[2:61]
#str_match_all(temp, "^OHX([:digit:]{2})([:upper:]{2,3})$")
## check after the extract
#a = unique(select(primary.missing.mask, tooth, question)) 
## no wisdom teeth in the cavies status..

tc.dict = data.frame(key = c(1, 2, 3, 4, 5, 9, NA),
                           value = c(0, 1, NA, NA, NA, NA, NA),
                           stringsAsFactors = FALSE)
ctc.dict = data.frame(key = c('D', 'E', 'J', 'K', 'M', 'P', 'Q', 'R', 'S', 'T', 'U', 'X', 'Y', 'Z'),
                      value = c(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, NA, 0, NA, 0), 
                      stringsAsFactors = FALSE)
csc.dict = data.frame(key = c(seq(0, 9), NA),
                      value = c(rep(1, 5), rep(0, 5), NA), 
                      stringsAsFactors = FALSE)
se.dict = data.frame(key = c(0, 1, 2, 4, 5, 9, NA),
                      value = c(0, 1, 1, 1, 1, NA, NA), 
                      stringsAsFactors = FALSE)

tooth.df = select(data.ohxden, SEQN, OHX01TC:OHX32TC, OHX02CTC:OHX31CTC, OHX02CSC:OHX31CSC, OHX02SE:OHX31SE) %>% 
  gather(key, value, OHX01TC:OHX32TC, OHX02CTC:OHX31CTC, OHX02CSC:OHX31CSC, OHX02SE:OHX31SE) %>% 
  extract(key, c("tooth", "question"), "^OHX([:digit:]{2})([:upper:]{2,3})$") %>%
  spread(question, value) %>%
  rename(tooth_type = TC, caries_tooth_type = CTC, caries = CSC, sealant = SE) %>%
  select(SEQN, tooth, tooth_type, caries_tooth_type, caries, sealant)

tooth.df.tidy = tooth.df %>% 
  mutate(tooth_type = tc.dict$value[match(tooth_type, tc.dict$key)]) %>%
  mutate(caries_tooth_type = ctc.dict$value[match(caries_tooth_type, ctc.dict$key)]) %>%
  mutate(caries = csc.dict$value[match(caries, csc.dict$key)]) %>%
  mutate(sealant = se.dict$value[match(sealant, se.dict$key)]) %>%
  rename(permanent = tooth_type, sound = caries_tooth_type, unrestored = caries) 

## check the permanent == NA cases before filtering out
temp = filter(tooth.df.tidy, is.na(permanent)) %>%
  group_by(permanent, sound, unrestored, sealant) %>%
  summarize(n())
## all other fields are NA or 0 - filter to permanent != NA
tooth.df.tidy = filter(tooth.df.tidy, !is.na(permanent))

a = unique(select(tooth.df.tidy, -SEQN, -tooth)) %>% arrange(permanent, sound, unrestored, sealant)
## Instead need to count the occurances. 

## define a sum with na.rm = T function
sum.naignore = function(x){sum(x, na.rm = T)}
b = tooth.df.tidy %>% 
  group_by(SEQN) %>%
  summarize_each(funs(sum.naignore), permanent:sealant)

c = tooth.df.tidy %>% 
  group_by(SEQN, permanent) %>%
  mutate(counter = 1) %>%
  summarize_each(funs(sum.naignore), sound:sealant, counter) %>%
  mutate(unsound = counter - sound) %>%
  select(SEQN, permanent, counter, sound, unsound, unrestored, sealant)
## interesting!
## check that unsound >= unrestored
all(c$unsound >= c$unrestored)
## should be TRUE and is - great1

# histogram of restored caries
par(mfrow = c(2, 1), mar = c(4,4,0,2))
hist(c$unsound - c$unrestored, breaks = seq(0, 32), main = '', xlab = 'restored caries', col = grey(0.8, 0.5))
par(mar = c(4,4,0,2))
hist(c$unrestored, breaks = seq(0, 32), main = '', xlab = 'untreated caries', col = grey(0.8, 0.5))


## count those individuals from each combination
binarize = function(x){as.integer(x > 0)}
d = c %>% 
  select(-SEQN) %>%
  mutate_each(funs(binarize), sound:sealant) %>%
  group_by(permanent, sound, unsound, unrestored, sealant) %>%
  summarize(n())

## I think this is everything I need to know about teeth. i.e. not age or ethnicity

# no tooth 1, 16, 17 or 32...

#------------------------------------------------------------------------------
## Coronal Caries: Surface condition for Teeth 2:15, 18:31
## from the document, these values may be recorded as a string variable, for example: "0", "13", "468"
## where "468" is 4, 6, and 8!
## values 0-4 are caries, values 5-9 are restoration, blank is missing but presumably no caries
## whole dataset has missingness for tooth 2 of 6942/8956 = 77.5%. I think it means no caries.

## we want to separate zero caries from one or more caries

#------------------------------------------------------------------------------
## Dental Sealants for teeth 2:5, 7, 10, 12:15, 18:21, 28:31 ## This makes ZERO sense!
## from the document, these values may be recorded as a string variable, for example: "0", "12", "13"
## where "12" is 1, and 2!
## values 0 sealant not present
## values 1-3 is sealant on permanent tooth
## value 4 is sealant on primary tooth
## value 9 is cannot be assessed (1848) and missing (5932 from 8956)
## I am going to assume cannot be assessed and missing == 0 == no sealant.



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

## merge into c
c.demo = left_join(c, select(data.demo, SEQN, age, ethnicity))
## bin age into the defined age groups from Dye
agegroups.dye = c(0, 1.9, 5.1, 8.1, 11.1, 15.1, 19.1, 100)
c.demo$age = as.integer(cut(c.demo$age, breaks = agegroups.dye))

## replace age keys with values
age.dict = data.frame(key = seq(1, length(agegroups.dye) - 1),
                      value = c('<2', '2-5', '6-8', '9-11', '12-15', '16-19', '>20'),
                      stringsAsFactors = FALSE)
c.demo = c.demo %>%
  mutate(age = age.dict$value[match(age, age.dict$key)])

## replace ethnicity keys with values
ethnicity.dict = data.frame(key = c(1, 2, 3, 4, 6, 7, NA),
                     value = c('mex', 'hisp', 'white', 'black', 'asian', 'other', NA), 
                     stringsAsFactors = FALSE)
c.demo = c.demo %>%
  mutate(ethnicity = ethnicity.dict$value[match(ethnicity, ethnicity.dict$key)])

## drop infants and adults, 8969 --> 4501
c.demo = filter(c.demo, age != '<2', age != '>20')

## drop 'other' ethnicity and combine 'mexican' and 'other hispanic'
c.demo = filter(c.demo, ethnicity != 'other') %>% ## 4501 --> 4253
  mutate(ethnicity = ifelse(ethnicity == 'mex', 'hisp', ethnicity))

## should be able to replicate creation of d but this time group_by age and ethnicty too
## count those individuals from each combination
binarize = function(x){as.integer(x > 0)}
d.demo = c.demo %>% 
  select(-SEQN) %>%
  mutate_each(funs(binarize), sound:sealant) %>%
  group_by(permanent, sound, unsound, unrestored, sealant, age, ethnicity) %>%
  summarize(counter = n())
## 207 rows but can now be aggreated in any way to create the results.

## define a helper function to arrange rows after summarization.
helper.agegroupsplit = function(x){
  temp.list = strsplit(x, '-', fixed = TRUE)
  as.numeric(sapply(temp.list, function(x) x[[1]]))
}
## usage:
# %>% mutate(a = helper.agegroupsplit(age)) %>% arrange(a) %>% select(-a)

## e.g. primary teeth by age
d.demo %>% 
  filter(permanent == 0) %>%
  group_by(age) %>%
  summarize_each(funs(sum), counter, unsound, unrestored) %>%
  mutate(unsound = unsound/counter, unrestored = unrestored/counter) %>%
  mutate(a = helper.agegroupsplit(age)) %>% arrange(a) %>% select(-a)

## e.g. permanent teeth by age
d.demo %>% 
  filter(permanent == 1) %>%
  group_by(age) %>%
  summarize_each(funs(sum), counter, unsound, unrestored) %>%
  mutate(unsound = unsound/counter, unrestored = unrestored/counter) %>%
  mutate(a = helper.agegroupsplit(age)) %>% arrange(a) %>% select(-a)

## similarly for ethnicity 
d.demo %>%
  filter(permanent == 0) %>%
  group_by(ethnicity) %>%
  summarize_each(funs(sum), counter, unsound, unrestored) %>%
  mutate(unsound = unsound/counter, unrestored = unrestored/counter)

d.demo %>%
  filter(permanent == 1) %>%
  group_by(ethnicity) %>%
  summarize_each(funs(sum), counter, unsound, unrestored) %>%
  mutate(unsound = unsound/counter, unrestored = unrestored/counter)

output.total = c %>% 
  select(-SEQN) %>%
  mutate_each(funs(binarize), counter, sound:sealant) %>%
  group_by(permanent) %>%
  summarize_each(funs(sum), counter, unsound, unrestored) %>%
  mutate(unsound = unsound/counter, unrestored = unrestored/counter)

caries_sealant_plots_Dye = function(df){
  ## take in a df
  ## expecting colnames SEQN, counter, sound, unsount, unrestored, sealant
  ## and only one value of primary/permanent
  output.total = df %>% 
    select(-SEQN) %>%
    mutate_each(funs(binarize), counter, sound:sealant) %>%
    group_by(permanent) %>%
    summarize_each(funs(sum), counter, unsound, unrestored) %>%
    mutate(unsound = unsound/counter, unrestored = unrestored/counter)
  
  output.age = df %>%
    mutate_each(funs(binarize), counter, sound:sealant) %>%
    group_by(age) %>%
    summarize_each(funs(sum), counter, unsound, unrestored) %>%
    mutate(unsound = unsound/counter, unrestored = unrestored/counter) %>%
    mutate(a = helper.agegroupsplit(age)) %>% arrange(a) %>% select(-a)
  
  output.eth = df %>%
    mutate_each(funs(binarize), counter, sound:sealant) %>%
    group_by(ethnicity) %>%
    summarize_each(funs(sum), counter, unsound, unrestored) %>%
    mutate(unsound = unsound/counter, unrestored = unrestored/counter)
  
  ## plotting
  ## first, how many bars we gonna have? add 2 for spaces between and 2 for width + margin
  n = nrow(output.total) + nrow(output.age) + nrow(output.eth) + 2
  
  temp.vector = c(output.total$unsound, output.age$unsound, output.eth$unsound)
  temp.names = c('total', output.age$age, output.eth$ethnicity)
  temp.cols = brewer.pal(4, 'Paired')[c(1,3)]
  temp.space = c(0, 1, rep(0, nrow(output.age) - 1), 1, rep(0, nrow(output.eth) - 1))
  barplot(temp.vector, names.arg = temp.names, width = 1, 
          main = paste(ifelse(output.total$permanent[1] == 0, 'Primary', 'Permanent'), 'teeth caries'),
          col = c(gray(0.8), rep(temp.cols[1], nrow(output.age)), rep(temp.cols[2], nrow(output.eth))),
          space = temp.space, 
          horiz = T, xlim = c(0,1), ylim = c(0, n), las = 2)
  box()
  
  temp.y = cumsum(temp.space) + seq(1, length(temp.vector)) - 0.5
  text(0.05, temp.y, paste0(signif(temp.vector*100, 3), '%'), adj = 0)
  
  ## single
  #text(0.05, 7.5, paste0(signif(temp.vector[2]*100, 3), '%'), adj = 0)
}

caries_sealant_plots_Dye(filter(c.demo, permanent == 0, age == '2-5' | age == '6-8'))

caries_sealant_plots_Dye(filter(c.demo, permanent == 1, age == '6-8' | age == '9-11'))

caries_sealant_plots_Dye(filter(c.demo, permanent == 1, age == '12-15' | age == '16-19'))
