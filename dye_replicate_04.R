## Vincent Major
## Health Informatics
## Nov 1 2016

if(!require(RNHANES)){install.packages('RNHANES');library(RNHANES)}
if(!require(plyr)){install.packages('plyr');library(plyr)}
if(!require(tidyverse)){install.packages('tidyverse');library(tidyverse)}
if(!require(stringr)){install.packages('stringr');library(stringr)}

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
  
  # 
  temp.datanames = c('BMX_G', 'BPX_G', 'DR1TOT_G', 'DR2TOT_G', 'COTNAL_G', 
                     'HDL_G', 'TRIGLY_G', 'TCHOL_G', 'GLU_G', 'OGTT_G', 
                     'ENX_G', 'MGX_G', 'CBQ_G', 'ECQ_G', 'INQ_G', 'SMQFAM_G',
                     'OHQ_G', 'DBQ_G', 'FSQ_G', 'WHQMEC_G', 'AUX_G')
  files.other = files.2011[files.2011$data_file_name %in% temp.datanames,]
  data.other.raw = nhanes_load_data(files.other$data_file_name, files.other$cycle, destination = "data", cache = TRUE)
  
#   files.2011[files.2011$data_file_name %in% c('PBCD_G', 'HDL_G', 'TRIGLY_G', 'TCHOL_G', 'COTNAL_G', 'CUSEZN_G', 'EPH_G', 'FOLATE_G', 'FOLFMS_G', 'GHB_G', 'UIO_G', 'IHGEM_G', 'UHG_G', 'UHM_G', 'UHMS_G', 'PP_G', 'PHTHTE_G', 'GLU_G'),]
#   ## definitely include
#   BIOPRO_G biochemistry profile
#   DR1TOT_G
#   DR2TOT_G
#   # maybe include in the older group
#   THYROD_G thyroid
#   TGEMA_G celiac 
}

## check if the saved df.full.RData exists
if(!("df_full.RData" %in% list.files("results/"))){
  
  ## nov 22 - loading files that aren't demographics or dental
  temp.files = setdiff(list.files("data"), c("nhanes_data_files.csv", 'DEMO_G.csv', 'OHXDEN_G.csv', 'OHXPER_G.csv', 'OHXREF_G.csv') )
  
  #data = lapply(temp.files, function(x) read_csv(file.path("data", x)))
  
  
  ####
  # manually coded for now
  #data.demo = read_csv(file.path("data", temp.files[1]))
  data.ohxden.raw = read_csv(file.path("data", 'OHXDEN_G.csv'))
  #data.ohxper = read_csv(file.path("data", temp.files[3]))
  #data.ohxref = read_csv(file.path("data", temp.files[4]))
  
  #data.test = dplyr::left_join(data.ohxden, data.demo)
  
  #### aggregating oral health examination into caries or not ----
  ## http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/OHXDEN_G.htm
  ## require complete examination status and dentition status
  data.ohxden = filter(data.ohxden.raw, OHDEXSTS == 1, OHDDESTS == 1) # n = 8956 --> 8073
  remove(data.ohxden.raw)
  
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
  
  ## tooth count, primary, permanent = 1, missing etc.
  tc.dict = data.frame(key = c(1, 2, 3, 4, 5, 9, NA),
                        value = c(0, 1, NA, NA, NA, NA, NA),
                        stringsAsFactors = FALSE)
  ## coronol tooth count, sound = 1, unsound etc
  ctc.dict = data.frame(key = c('D', 'E', 'J', 'K', 'M', 'P', 'Q', 'R', 'S', 'T', 'U', 'X', 'Y', 'Z'),
                        value = c(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, NA, 0, NA, 0), 
                        stringsAsFactors = FALSE)
  ## coronol surface condition, given unsound, is it a cavity = 1 or restored
  csc.dict = data.frame(key = c(seq(0, 9), NA),
                        value = c(rep(1, 5), rep(0, 5), NA), 
                        stringsAsFactors = FALSE)
  ## sealant, not present or present = 1
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
  ## no tooth there - don't know anything about it --> remove them
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
  # par(mfrow = c(2, 1), mar = c(4,4,0,2))
  # hist(c$unsound - c$unrestored, breaks = seq(0, 32), main = '', xlab = 'restored caries', col = grey(0.8, 0.5))
  # par(mar = c(4,4,0,2))
  # hist(c$unrestored, breaks = seq(0, 32), main = '', xlab = 'untreated caries', col = grey(0.8, 0.5))
  
  
  ## count those individuals from each combination
  binarize = function(x){as.integer(x > 0)}
  d = c %>% 
    #select(-SEQN) %>%
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
  data.demo.raw = read_csv(file.path("data", 'DEMO_G.csv'))
  
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
  ## replace ethnicity keys with values
  ethnicity.dict = data.frame(key = c(1, 2, 3, 4, 6, 7, NA),
                              value = c('mex', 'hisp', 'white', 'black', 'asian', 'other', NA), 
                              stringsAsFactors = FALSE)
  data.demo = select(data.demo.raw, SEQN, RIAGENDR, RIDAGEYR, RIDRETH3, RIDEXAGY, DMDBORN4, DMDCITZN, SIALANG, INDFMPIR, DMDHHSIZ, DMDFMSIZ, DMDHHSZA, DMDHHSZB, DMDHHSZE) %>%
    mutate(genderF = as.integer(RIAGENDR == 2), 
           born_usa = as.integer(DMDBORN4 == 1), 
           citizen = as.integer(DMDCITZN == 1),
           interview_english = as.integer(SIALANG == 1), #no missing here
           age = ifelse(is.na(RIDEXAGY), RIDAGEYR, RIDEXAGY),
           ethnicity = ethnicity.dict$value[match(RIDRETH3, ethnicity.dict$key)]
           ) %>%
    rename(income_ratio_over_poverty = INDFMPIR, 
           household_size = DMDHHSIZ,
           family_size = DMDFMSIZ,
           num_children = DMDHHSZA,
           num_adolescents = DMDHHSZB,
           num_elderly = DMDHHSZE) %>%
    select(-RIAGENDR, -DMDBORN4, -DMDCITZN, -SIALANG, -RIDAGEYR, -RIDEXAGY, -RIDRETH3)
  remove(data.demo.raw)
  
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
  
  ## drop infants and adults, 8969 --> 4501
  c.demo = filter(c.demo, age != '<2', age != '>20')
  
  ## drop 'other' ethnicity and combine 'mexican' and 'other hispanic'
  ## Nov 27 - trying with other in to get closer to Dye results
  c.demo = c.demo %>%
    #filter(ethnicity != 'other') %>% ## 4501 --> 4253
    mutate(ethnicity = ifelse(ethnicity == 'mex', 'hisp', ethnicity))
  
  ## should be able to replicate creation of d but this time group_by age and ethnicty too
  ## count those individuals from each combination
  binarize = function(x){as.integer(x > 0)}
  d.demo = c.demo %>% 
    #select(-SEQN) %>%
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
    #select(-SEQN) %>%
    mutate_each(funs(binarize), counter, sound:sealant) %>%
    group_by(permanent) %>%
    summarize_each(funs(sum), counter, unsound, unrestored) %>%
    mutate(unsound = unsound/counter, unrestored = unrestored/counter)
  
  caries_sealant_plots_Dye = function(df){
    ## take in a df
    ## expecting colnames SEQN, counter, sound, unsount, unrestored, sealant
    ## and only one value of primary/permanent
    output.total = df %>% 
      #select(-SEQN) %>%
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
    
    ## rearrange into Dye order and drop the other category from plotting
    output.eth = output.eth[match(c('asian', 'hisp', 'black', 'white'), output.eth$ethnicity),]
    
    ## plotting with untreated caries
    ## first, how many bars we gonna have? add 2 for spaces between and 2 for width + margin
    n = (nrow(output.total) + nrow(output.age) + nrow(output.eth) + 2) * 2 + 2
    
    #temp.vector = c(output.total$unsound, output.age$unsound, output.eth$unsound)
    #temp.names = c('total', output.age$age, output.eth$ethnicity)
    ## Trying to get the correct order
    temp.vector = c(c(output.eth$unrestored, rev(output.age$unrestored), output.total$unrestored),
                    c(output.eth$unsound, rev(output.age$unsound), output.total$unsound))
    temp.names = c(c(output.eth$ethnicity, rev(output.age$age), 'total'),
                   c(output.eth$ethnicity, rev(output.age$age), 'total'))
    
    ##
    if(!require(RColorBrewer)){install.packages('RColorBrewer');library(RColorBrewer)}
    temp.cols = brewer.pal(4, 'Paired')[c(1,3)]
    col.vector = c(rep(temp.cols[2], nrow(output.eth)), rep(temp.cols[1], nrow(output.age)), gray(0.8))
    temp.space = c(c(0, rep(0, nrow(output.eth) - 1), 1, rep(0, nrow(output.age) - 1), 1),
                   c(2, rep(0, nrow(output.eth) - 1), 1, rep(0, nrow(output.age) - 1), 1))
    barplot(temp.vector, names.arg = temp.names, width = 1, 
            main = paste(ifelse(output.total$permanent[1] == 0, 'Primary', 'Permanent'), 'teeth caries'),
            col = col.vector,
            space = temp.space, 
            horiz = T, xlim = c(0,1), ylim = c(0, n), las = 2)
    box()
    
    temp.y = cumsum(temp.space) + seq(1, length(temp.vector)) - 0.5
    text(0.05, temp.y, paste0(signif(temp.vector*100, 3), '%'), adj = 0)
    
    ## single
    #text(0.05, 7.5, paste0(signif(temp.vector[2]*100, 3), '%'), adj = 0)
    
  }
  
  png("results/figures/caries_Dye_figure_primary_2-8_years.png", width = 6, height = 6, units = 'in', res = 300)
  caries_sealant_plots_Dye(filter(c.demo, permanent == 0, age == '2-5' | age == '6-8'))
  dev.off()
  png("results/figures/caries_Dye_figure_permanent_6-11_years.png", width = 6, height = 6, units = 'in', res = 300)
  caries_sealant_plots_Dye(filter(c.demo, permanent == 1, age == '6-8' | age == '9-11'))
  dev.off()
  png("results/figures/caries_Dye_figure_permanent_12-19_years.png", width = 6, height = 6, units = 'in', res = 300)
  caries_sealant_plots_Dye(filter(c.demo, permanent == 1, age == '12-15' | age == '16-19'))
  dev.off()
  
  
  #### loading other data tables and preprocessing ----
  print(temp.files)
  
  ## body mass examination
  data.bmx.raw = read_csv(file.path("data", "BMX_G.csv"))
  data.bmx = select(data.bmx.raw, SEQN, BMXWT, BMXRECUM, BMXHT, BMXBMI, BMDBMIC, 
                    BMXWAIST, BMDAVSAD) %>%
    mutate(height_cm = if_else(is.na(BMXHT), BMXRECUM, BMXHT)) %>% select(-BMXHT, -BMXRECUM) %>%
    rename(weight_kg = BMXWT, bmi = BMXBMI, bmi_flag_child = BMDBMIC, 
           waist_cm = BMXWAIST, sagittal_dia_cm = BMDAVSAD)
  remove(data.bmx.raw)
  
  gather_cols_mean = function(df, cols, id = 'SEQN'){
    #temp.colname = substr(cols[1], 1, nchar(cols[1]) - 1)
    #print(temp.colname)
    df %>%
      gather_('temp.key', 'temp.value', cols) %>%
      group_by(SEQN) %>%
      summarize(a = mean(temp.value, na.rm = T)) %>%
      rename_(temp.colname = 'a')
  }
  
  data.bpx.raw = read_csv(file.path("data", "BPX_G.csv"))
  data.bpx = select(data.bpx.raw, SEQN, BPXPLS, BPXPULS, BPXSY1, BPXDI1, BPXSY2, BPXDI2, BPXSY3, BPXDI3) %>%
    rename(pulse_60s = BPXPLS, pulse_irreg = BPXPULS) %>%
    mutate(pulse_irreg = pulse_irreg - 1) %>% # from regular = 1/irregular = 2 to regular = 0/irregualar = 1
    left_join(gather_cols_mean(data.bpx.raw, c('BPXSY1', 'BPXSY2', 'BPXSY3'), 'SEQN') %>% rename(sysbp = temp.colname)) %>%
    left_join(gather_cols_mean(data.bpx.raw, c('BPXDI1', 'BPXDI2', 'BPXDI3'), 'SEQN') %>% rename(diabp = temp.colname)) %>%
    select(-(BPXSY1:BPXDI3))
  remove(data.bpx.raw)
  
  ## dietary intake over 2 days
  data.diet1.raw = read_csv(file.path("data", "DR1TOT_G.csv"))
  data.diet1 = select(data.diet1.raw, SEQN, DR1LANG, DR1MNRSP, DR1TKCAL, DR1TPROT, 
                      DR1TCARB, DR1TSUGR, DR1TFIBE, DR1TTFAT, DR1TSFAT, DR1TMFAT, 
                      DR1TPFAT, DR1TCHOL, DR1TATOC, DR1TATOA, DR1TRET, DR1TVARA, 
                      DR1TACAR, DR1TBCAR, DR1TCRYP, DR1TLYCO, DR1TLZ, DR1TVB1, 
                      DR1TVB2, DR1TNIAC, DR1TVB6, DR1TFOLA, DR1TFA, DR1TFF, 
                      DR1TFDFE, DR1TCHL, DR1TVB12, DR1TB12A, DR1TVC, DR1TVD, 
                      DR1TVK, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON, DR1TZINC, 
                      DR1TCOPP, DR1TSODI, DR1TPOTA, DR1TSELE, DR1TCAFF, DR1TTHEO, 
                      DR1TALCO, DR1TMOIS, DR1_320Z, DR1_330Z, DR1BWATZ)
  colnames(data.diet1) = str_to_lower(gsub("DR1", "", colnames(data.diet1)))
  
  data.diet2.raw = read_csv(file.path("data", "DR2TOT_G.csv"))
  data.diet2 = select(data.diet2.raw, SEQN, DR2LANG, DR2MNRSP, DR2TKCAL, DR2TPROT,
                      DR2TCARB, DR2TSUGR, DR2TFIBE, DR2TTFAT, DR2TSFAT, DR2TMFAT, 
                      DR2TPFAT, DR2TCHOL, DR2TATOC, DR2TATOA, DR2TRET, DR2TVARA, 
                      DR2TACAR, DR2TBCAR, DR2TCRYP, DR2TLYCO, DR2TLZ, DR2TVB1, 
                      DR2TVB2, DR2TNIAC, DR2TVB6, DR2TFOLA, DR2TFA, DR2TFF, 
                      DR2TFDFE, DR2TCHL, DR2TVB12, DR2TB12A, DR2TVC, DR2TVD, 
                      DR2TVK, DR2TCALC, DR2TPHOS, DR2TMAGN, DR2TIRON, DR2TZINC, 
                      DR2TCOPP, DR2TSODI, DR2TPOTA, DR2TSELE, DR2TCAFF, DR2TTHEO, 
                      DR2TALCO, DR2TMOIS, DR2_320Z, DR2_330Z, DR2BWATZ)
  colnames(data.diet2) = str_to_lower(gsub("DR2", "", colnames(data.diet2)))
  
  mean.narm = function(x){mean(x, na.rm = TRUE)}
  combine_common_tables = function(temp1, temp2){
    temp = rbind(temp1, temp2)
    output = temp %>% 
      group_by(seqn) %>%
      summarize_each(funs(mean.narm), -seqn)
  }
  
  data.diet = combine_common_tables(data.diet1, data.diet2)
  data.diet = rename(data.diet, SEQN = seqn, 
                     water_plain = `_320z`,
                     water_tap = `_330z`,
                     water_bottled = bwatz)
  remove(data.diet1.raw, data.diet1, data.diet2.raw, data.diet2)
  
  ## cotinine - smoking status
  data.cotnal.raw = read_csv(file.path("data", "COTNAL_G.csv"))
  ## If the measurements are below detection limit, zero the measurements. 
  data.cotnal = select(data.cotnal.raw, SEQN, LBXCOT, LBDCOTLC, URXNAL, URDNALLC) %>%
    mutate(cotinine_bld = if_else(LBDCOTLC == 0, LBXCOT, 0)) %>%
    mutate(nnal_urn = if_else(URDNALLC == 0, URXNAL, 0)) %>%
    select(-(LBXCOT:URDNALLC))
  remove(data.cotnal.raw)
  
  ## cholesterol
  data.hdl.raw = read_csv(file.path("data", "HDL_G.csv"))
  data.trigly.raw = read_csv(file.path("data", "TRIGLY_G.csv"))
  data.tchol.raw = read_csv(file.path("data", "TCHOL_G.csv"))
  data.hdl = select(data.hdl.raw, SEQN, LBDHDD) %>%
    rename(chol_hdl = LBDHDD)
  data.trigly = select(data.trigly.raw, SEQN, LBXTR, LBDLDL) %>%
    rename(chol_tri = LBXTR, chol_ldl = LBDLDL)
  data.tchol = select(data.tchol.raw, SEQN, LBXTC) %>%
    rename(chol_tot = LBXTC)
  data.chol = left_join(left_join(data.tchol, data.hdl), data.trigly) #%>%
  #mutate(test = chol_tot - chol_hdl - (chol_tri/5)) # checking the derived LDL values
  remove(data.hdl.raw, data.hdl, data.trigly.raw, data.trigly, data.tchol.raw, data.tchol)
  
  ## glucose, insulin and glucose tolerance tests - (pre-)diabetic status
  data.glu.raw = read_csv(file.path("data", "GLU_G.csv"))
  data.ogtt.raw = read_csv(file.path("data", "OGTT_G.csv"))
  data.glu = select(data.glu.raw, SEQN, LBXGLU, LBXIN) %>%
    rename(glucose = LBXGLU, insulin = LBXIN)
  data.ogtt = select(data.ogtt.raw, SEQN, GTXDRANK, GTDCODE, LBXGLT) %>%
    mutate(known_diabetic_meds = as.integer(GTDCODE == 22)) %>%
    rename(glucose_tolerence = LBXGLT) %>%
    select(-GTXDRANK, -GTDCODE)
  data.diabetes = left_join(data.glu, data.ogtt)
  ## GTXDRANK == 1 drank all of the challenge opposed to some(2) or none (3)
  ## GTDCODE == 0 complete, or 22 for diabetic on meds, all others incomplete - remove!
  ## LBXGLT is the oral glucose tolerance test results in native mg/dL
  ## review of data shows that GTXDRANK = 1, GTDCODE = 0 to have non-missing LBXGLT
  ## I want to flag the known diabetics on medications
  remove(data.glu.raw, data.ogtt.raw, data.glu, data.ogtt)
  #plot(data.diabetes$glucose, data.diabetes$glucose_tolerence)
  
  ## exhaled nitric oxide - indicates lung inflammation, higher in asthma etc.
  ## just going to use the derived mean of reproducible results
  data.enx.raw = read_csv(file.path("data", "ENX_G.csv"))
  data.enx = select(data.enx.raw, SEQN, ENXMEAN) %>%
    rename(exhaled_nitric_oxide = ENXMEAN)
  remove(data.enx.raw)
  
  ## muscle strength 
  ## just going to use the derived sum of the highest from each hand
  data.mgx.raw = read_csv(file.path("data", "MGX_G.csv"))
  data.mgx = select(data.mgx.raw, SEQN, MGDCGSZ) %>%
    rename(muscle_strength = MGDCGSZ)
  remove(data.mgx.raw)
  
  ## questionnaires
  
  ## money on food 
  data.cbq.raw = read_csv(file.path("data", "CBQ_G.csv"))
  noanswer.na = function(x){ifelse(x == 777777 | x == 999999, NA, x)}
  data.cbq = select(data.cbq.raw, SEQN:CBD130) %>%
    mutate_each(funs(noanswer.na(.)), CBD070:CBD130) %>%
    rename(money_grocery = CBD070,
           money_grocery_notfood = CBD090,
           money_other = CBD110,
           money_eatout = CBD120,
           money_fastfood = CBD130)
  remove(data.cbq.raw)
  ## I looked at the plots e.g. plot(data.cbq$money_grocery, data.cbq$money_grocery_notfood)
  ## to look for dependencies and that is the only one - which makes sense!
  
  ## early childhood - surrogates for socioeconomic status
  data.ecq.raw = read_csv(file.path("data", "ECQ_G.csv"))
  ## weight status dictionary
  weight_status.dict = data.frame(key = c(1, 2, 3, 7, 9, NA),
                       value = c('over', 'under', 'right', NA, NA, NA),
                       stringsAsFactors = FALSE)
  data.ecq = select(data.ecq.raw, SEQN, ECD010, ECQ020, ECD070A, ECD070B, WHQ030E, MCQ080E, ECQ150) %>%
    mutate(mother_age = ifelse(ECD010 == 7777 | ECD010 == 9999, NA, ECD010),
           mother_smoked = ifelse(ECQ020 == 7 | ECQ020 == 9, NA, as.integer(ECQ020 == 1)),
           birth_weight_lb = ifelse(ECD070A > 1000, NA, ECD070A + ECD070B/16),
           parent_child_weight_status = weight_status.dict$value[match(WHQ030E, weight_status.dict$key)],
           parent_told_child_overweight = ifelse(MCQ080E == 7 | MCQ080E == 9, NA, as.integer(MCQ080E == 1)),
           parent_helping_control_weight = ifelse(ECQ150 == 7 | ECQ150 == 9, NA, as.integer(ECQ150 == 1)) ) %>%
    select(-ECD010, -ECQ020, -ECD070A, -ECD070B, -WHQ030E, -MCQ080E, -ECQ150)
  remove(data.ecq.raw, weight_status.dict)
  
  ## income
  data.inq.raw = read_csv(file.path("data", "INQ_G.csv"))
  data.inq = select(data.inq.raw, SEQN, INDFMMPI) %>%
    rename(income_poverty_index = INDFMMPI)
  remove(data.inq.raw)
  
  ## smoking in home status
  data.smqfam.raw = read_csv(file.path("data", "SMQFAM_G.csv"))
  data.smqfam = select(data.smqfam.raw, SEQN, SMD410:SMD430) %>%
    mutate(household_smokers_flag = ifelse(SMD410 == 7 | SMD410 == 9, NA, as.integer(SMD410 == 1)) ) %>%
    rename(household_smokers_num = SMD415,
           household_cigarettes = SMD430) %>%
    select(-SMD410, -SMD415A)
  remove(data.smqfam.raw)
  
  ## oral health
  data.ohq.raw = read_csv(file.path("data", "OHQ_G.csv"))
  ## dental status dictionary
  dentist_visit.dict = data.frame(key = c(1, 2, 3, 4, 5, 6, 7, 77, 99, NA),
                                  value = c('< 0.5', '0.5 - 1', '1 - 2', '2 - 3', '3 - 5', '>5', 'never', NA, NA, NA),
                                  stringsAsFactors = FALSE)
  dentist_visit_reason.dict = data.frame(key = c(1, 2, 3, 4, 5, 7, 9, NA),
                                  value = c('routine', 'request', 'symptomatic', 'followup', 'other', NA, NA, NA),
                                  stringsAsFactors = FALSE)
  dental_self_rate.dict = data.frame(key = c(1, 2, 3, 4, 5, 7, 9, NA),
                                         value = c('excellent', 'very good', 'good', 'fair', 'poor', NA, NA, NA),
                                         stringsAsFactors = FALSE)
  data.ohq = select(data.ohq.raw, SEQN, OHQ030, OHQ033, OHQ770, OHQ845) %>%
    mutate(dental_visit_time = dentist_visit.dict$value[match(OHQ030, dentist_visit.dict$key)],
           dental_visit_reason = dentist_visit_reason.dict$value[match(OHQ033, dentist_visit_reason.dict$key)],
           dental_needed_couldnt = ifelse(OHQ770 == 7 | OHQ770 == 9, NA, as.integer(OHQ770 == 1)),
           dental_self_rate = dental_self_rate.dict$value[match(OHQ845, dental_self_rate.dict$key)]) %>%
    select(-OHQ030, -OHQ033, -OHQ770, -OHQ845)
  remove(data.ohq.raw, dentist_visit.dict, dentist_visit_reason.dict, dental_self_rate.dict)
  
  ## diet behaviour and nutrition
  data.dbq.raw = read_csv(file.path("data", "DBQ_G.csv"))
  diet_self_rate.dict = data.frame(key = c(1, 2, 3, 4, 5, 7, 9, NA),
                                     value = c('excellent', 'very good', 'good', 'fair', 'poor', NA, NA, NA),
                                     stringsAsFactors = FALSE)
  data.dbq = select(data.dbq.raw, SEQN, DBQ010, DBD030, DBD041, DBD050, DBD055, DBQ700, DBD900, DBD905, DBD910) %>%
    mutate(diet_breastfed_flag = ifelse(DBQ010 == 7 | DBQ010 == 9, NA, as.integer(DBQ010 == 1)),
           diet_breastfed_days = ifelse(DBD030 == 777777 | DBD030 == 999999, NA, ifelse(DBD030 == 0, 2*365, DBD030)),
           diet_formula_flag = ifelse(DBD041 > 0 & DBD041 < 731, 1, 0),
           diet_formula_started = ifelse(DBD041 == 777777 | DBD041 == 999999 | DBD041 == 0, NA, DBD041),
           diet_formula_ended = ifelse(DBD050 == 777777 | DBD050 == 999999, NA, ifelse(DBD050 == 0, 2*365, DBD050)),
           diet_formula_days = diet_formula_ended - diet_formula_started,
           diet_other_started = ifelse(DBD055 == 777777 | DBD055 == 999999, NA, DBD055),
           diet_self_rate = diet_self_rate.dict$value[match(DBQ700, diet_self_rate.dict$key)],
           diet_7daymeals_fastfood = ifelse(DBD900 == 7777 | DBD900 == 9999, NA, ifelse(DBD900 == 5555, 22, DBD900)),
           diet_30daymeals_readytoeat = ifelse(DBD905 == 7777 | DBD905 == 9999, NA, DBD905),
           diet_30daymeals_frozen = ifelse(DBD910 == 7777 | DBD910 == 9999, NA, DBD910) ) %>%
    select(-(DBQ010:DBD910))
  remove(data.dbq.raw, diet_self_rate.dict)
  ## Yasmi thinks that longer may be associated with higher caries rates
  ## include other milk based things
  
  ## food security
  data.fsq.raw = read_csv(file.path("data", "FSQ_G.csv"))
  food_security.dict = data.frame(key = c(1, 2, 3, 7, 9, NA),
                                     value = c('often', 'sometimes', 'never', NA, NA, NA),
                                     stringsAsFactors = FALSE)
  data.fsq = select(data.fsq.raw, SEQN, FSD032E, FSD032F) %>%
    mutate(food_security_cantafford = food_security.dict$value[match(FSD032E, food_security.dict$key)],
           food_security_notenough = food_security.dict$value[match(FSD032F, food_security.dict$key)]) %>%
    select(-FSD032E, -FSD032F)
  remove(data.fsq.raw, food_security.dict)
  
  ## weight management youth
  data.whq.raw = read_csv(file.path("data", "WHQMEC_G.csv"))
  weight_status.dict = data.frame(key = c(1, 2, 3, 7, 9, NA),
                                  value = c('over', 'under', 'right', NA, NA, NA),
                                  stringsAsFactors = FALSE)
  data.whq = select(data.whq.raw, SEQN, WHQ030M) %>%
    mutate(self_weight_status = weight_status.dict$value[match(WHQ030M, weight_status.dict$key)]) %>%
    select(-WHQ030M)
  remove(data.whq.raw, weight_status.dict)
  
  
  #### merging tables ----
  
  # ## little test of speed
  # curr.time = proc.time()
  # df.full = Reduce(function(...) merge(..., by = 'SEQN', all.x = T, all.y = F), list(c.demo, data.bmx, data.bpx, data.chol, data.cotnal, data.diabetes, data.diet))
  # print(proc.time() - curr.time); remove(curr.time)
  # print(dim(df.full)); remove(df.full)
  # # 0.075
  # 
  # curr.time = proc.time()
  # df.full = Reduce(function(...) left_join(..., by = 'SEQN'), list(c.demo, data.bmx, data.bpx, data.chol, data.cotnal, data.diabetes, data.diet))
  # print(proc.time() - curr.time); remove(curr.time)
  # print(dim(df.full)); remove(df.full)
  # # 0.021
  
  df.full = Reduce(function(...) left_join(..., by = 'SEQN'), 
                   list(select(c.demo, -age, -ethnicity), 
                        data.demo, data.bmx, data.bpx, data.chol, data.cotnal, 
                        data.diabetes, data.diet, data.enx, data.mgx, data.ecq,
                        data.inq, data.smqfam, data.ohq, data.dbq, data.fsq, 
                        data.whq))
  df.full = df.full %>% mutate(unsound_flag = as.integer(unsound > 0)) %>%
    ungroup()
  ## check column names
  colnames(df.full)
  length(unique(df.full$SEQN))
  sum(duplicated(df.full$SEQN))
  ## 3300 individuals, 1201 have a mix of primary and permanent teeth
  ## venn diagram
  library(gplots)
  png("results/figures/venn_primary_permanent.png", width = 5, height = 5, units = 'in', res = 300)
  venn( list(primary = unlist(subset(df.full, permanent == 0, SEQN)),
             permanent = unlist(subset(df.full, permanent == 1, SEQN))) )
  dev.off()
  
  ## one over arching model or two for primary/permanent?
  
  ## classification or regression?
  ## is one cavity out of 20 primary/permanent teeth good or bad?
  ## I would suggest bad as the goal is to prevent any cavities.
  ## start with classificaiton
  
  ## definitely need regularization for glm
  ## need to look into glmnet for logistic
  ## or, random forest
  
  ## replace NA with zeros
  df.full$household_cigarettes[is.na(df.full$household_cigarettes)] = 0
  df.full$household_smokers_num[is.na(df.full$household_smokers_num)] = 0
  
  df.full = df.full %>% mutate_each(funs(as.character), permanent, genderF, born_usa, citizen, interview_english, bmi_flag_child, pulse_irreg, lang, mnrsp, mother_smoked, parent_child_weight_status, parent_told_child_overweight, parent_helping_control_weight, household_smokers_flag, dental_needed_couldnt, diet_breastfed_flag, diet_formula_flag)
  df.full = as.data.frame(df.full, stringsAsFactors = TRUE)
  df.full[sapply(df.full, is.character)] <- lapply(df.full[sapply(df.full, is.character)], as.factor)
  
  write_csv(df.full, "results/df_full.csv")
  save(df.full, file = "results/df_full.RData")

}
## remove all objects except functions
rm(list = setdiff(ls(), lsf.str()))
## load the cached df_full to ensure behaviour is consistent
load("results/df_full.RData")



#### setting up independent training, validation and testing sets ---- 
temp.ids = unique(df.full$SEQN)
set.seed(123)
temp.ids = sample(temp.ids, replace = F) ## sample without replacement
train_val.split = length(temp.ids)*0.7
val_test.split = length(temp.ids)*0.8
train.ids = temp.ids[1:train_val.split]
val.ids = temp.ids[(train_val.split + 1):val_test.split]
test.ids = temp.ids[(val_test.split + 1):length(temp.ids)]
## checks
intersect(train.ids, val.ids)
intersect(test.ids, val.ids)
intersect(test.ids, train.ids)

# library(Amelia)
# missmap(df.full)

df.train = df.full %>% 
  filter(SEQN %in% train.ids) %>% 
  ungroup() %>% 
  mutate(unsound_flag = factor(unsound_flag)) %>%
  select(-SEQN, -(counter:sealant))
# df.train = as.data.frame(df.train, stringsAsFactors = TRUE)
# # df.train = sapply(df.train, function(x) ifelse(is.character(x) == TRUE, factor(x), x))
# df.train[sapply(df.train, is.character)] <- lapply(df.train[sapply(df.train, is.character)], as.factor)

df.test = df.full %>% 
  filter(SEQN %in% test.ids) %>% 
  ungroup() %>% 
  mutate(unsound_flag = factor(unsound_flag)) %>%
  select(-SEQN, -(counter:sealant))

#### randomforest learning ----
# library(randomForest)
# ## all rows have some missingness 
# model.rf = randomForest(x = select(df.train, -unsound),
#                         y = factor(unlist(transmute(df.train, unsound_flag = as.integer(unsound > 0)))), 
#                         na.action=na.omit)

if(!require(rpart)){install.packages('rpart');library(rpart)}
rpart(unsound_flag ~ ., data = df.train)


# if(!require(randomForestSRC)){install.packages('randomForestSRC');library(randomForestSRC)}
# ## paste(colnames(df.train), collapse = " + ")
# ## model.rfsrc = rfsrc(unsound_flag ~ permanent + income_ratio_over_poverty + household_size + family_size + num_children + num_adolescents + num_elderly + genderF + born_usa + citizen + interview_english + age + ethnicity + weight_kg + bmi + bmi_flag_child + waist_cm + sagittal_dia_cm + height_cm + pulse_60s + pulse_irreg + sysbp + diabp + chol_tot + chol_hdl + chol_tri + chol_ldl + cotinine_bld + nnal_urn + glucose + insulin + glucose_tolerence + known_diabetic_meds + lang + mnrsp + tkcal + tprot + tcarb + tsugr + tfibe + ttfat + tsfat + tmfat + tpfat + tchol + tatoc + tatoa + tret + tvara + tacar + tbcar + tcryp + tlyco + tlz + tvb1 + tvb2 + tniac + tvb6 + tfola + tfa + tff + tfdfe + tchl + tvb12 + tb12a + tvc + tvd + tvk + tcalc + tphos + tmagn + tiron + tzinc + tcopp + tsodi + tpota + tsele + tcaff + ttheo + talco + tmois + water_plain + water_tap + water_bottled + exhaled_nitric_oxide + muscle_strength + mother_age + mother_smoked + birth_weight_lb + parent_child_weight_status + parent_told_child_overweight + parent_helping_control_weight + income_poverty_index + household_smokers_num + household_cigarettes + household_smokers_flag + dental_visit_time + dental_visit_reason + dental_needed_couldnt + dental_self_rate + diet_breastfed_flag + diet_breastfed_days + diet_formula_flag + diet_formula_started + diet_formula_ended + diet_formula_days + diet_other_started + diet_self_rate + diet_7daymeals_fastfood + diet_30daymeals_readytoeat + diet_30daymeals_frozen + food_security_cantafford + food_security_notenough + self_weight_status + unsound_flag, 
# model.rfsrc = rfsrc(unsound_flag ~ ., 
#                     data = df.train,
#                     na.action = "na.impute", do.trace = TRUE)
# ## error: "Please Contact Technical Support"
# summary(model.rfsrc)
# a = predict(model.rfsrc, newdata = as.data.frame(df.test), na.action = "na.impute")
# test.probs = data.frame(label = as.integer(df.test$unsound_flag) - 1, prob = a$predicted[,2])


if(!require(caret)){install.packages('caret');library(caret)}
if(!require(e1071)){install.packages('e1071');library(e1071)}
fitControl <- trainControl(#method = "cv",
                           method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)
C5Grid = expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )
set.seed(123)
C5Fit <- train(
  x = df.train[, -ncol(df.train)],
  y = factor(df.train[, ncol(df.train)], labels = c('neg', 'pos')),
  method = "C5.0",
  na.action = na.pass,
  trControl = fitControl,
  verbose = FALSE,
  tuneGrid = C5Grid,
  ## Specify which metric to optimize
  metric = "ROC"
)
# started at 1:38
C5Fit
## loosely using an example from
## http://www.euclidean.com/machine-learning-in-practice/2015/6/12/r-caret-and-parameter-tuning-c50

# visualize the results
plot(C5Fit)

# model.c50 = train(unsound_flag ~ .,
#       data = df.train,
#       method = "C5.0",
#       na.action = na.pass)
# # started at 12:48, finished at 12:59 - ten minutes
# summary(model.c50)
# summary(model.c50$finalModel)
# plot(model.c50)
# 
# testX = df.test[,-ncol(df.test)]
# a = extractProb(model.c50, testX = testX)#, type = 'prob')
## Doesn't work...
# test.probs = data.frame(label = as.integer(df.test$unsound_flag) - 1, prob = a$predicted[,2])


## using the c5.0 package directly
## http://connor-johnson.com/2014/08/29/decision-trees-in-r-using-the-c50-package/
if(!require(C50)){install.packages('C50');library(C50)}
model.c50 <- C50::C5.0(df.train[,-ncol(df.train)], df.train[,ncol(df.train)])
summary( model.c50 )
## boosting 10 times
model.c50.boosted <-  C50::C5.0(df.train[,-ncol(df.train)], df.train[,ncol(df.train)], trials = 20 )
summary( model.c50.boosted )

#plot(model.c50.boosted)

## predicting
if(!require(ROCR)){install.packages('ROCR');library(ROCR)}
p <- predict( model.c50.boosted, df.test[,-ncol(df.test)], type = "prob" )
pred = prediction(p[,2], df.test$unsound_flag)
perf.full_C5 = performance(pred, measure="tpr", x.measure="fpr")
auc.full_C5 = performance(pred, measure = 'auc')@y.values[[1]]
print(auc.full_C5) ## 73.4 -- not too bad
png("results/figures/C5_ROC_0734.png", width = 4, height = 4, units = 'in', res = 300)
plot(perf.full_C5, linewidth = 3)
grid()
abline(0,1)
dev.off()

a = C50::C5imp(model.c50.boosted, metric = "usage")
b = C50::C5imp(model.c50.boosted, metric = "splits")
c = data.frame(variable = row.names(a), usage = a[,1])
d = data.frame(variable = row.names(b), splits = b[,1])
e = left_join(c, d)

barplot(e$usage, space = 0.2, names.arg = e$variable)
barplot(e$splits, space = 0.2, names.arg = e$variable)


if(!require(pdp)){install.packages('pdp');library(pdp)}
pdp::partial(model.c50.boosted, train = df.train, pred.var = c('age'), plot = TRUE)
pdp::partial(model.c50.boosted, train = df.train, pred.var = c('permanent'), plot = TRUE)
pdp::partial(model.c50.boosted, train = df.train, pred.var = c('income_ratio_over_poverty'), plot = TRUE)
pdp::partial(model.c50.boosted, train = df.train, pred.var = c('num_elderly'), plot = TRUE)
a = sapply(colnames(df.test[,-ncol(df.test)]), function(x) pdp::partial(model.c50.boosted, train = df.train, pred.var = x))
# pdp::plotPartial(a)

## impute missingness with mice package.
#if(!require(mice)){install.packages('mice');library(mice)}
## use pmm method, 5 times and for each fit a model on training ids, test on test ids
#tempData <- mice(df.full, m = 5, maxit = 5, meth = 'pmm', seed = 123)
#summary(tempData)
#df.imputed


## reduce to only the complete fields
df.complete = df.full[,sapply(df.full, function(x) all(!is.na(x)))]
df.complete.train = df.complete %>% 
  filter(SEQN %in% train.ids) %>% 
  ungroup() %>% 
  mutate(unsound_flag = factor(unsound_flag)) %>%
  select(-SEQN, -(counter:sealant))

df.complete.test = df.complete %>% 
  filter(SEQN %in% test.ids) %>% 
  ungroup() %>% 
  mutate(unsound_flag = factor(unsound_flag)) %>%
  select(-SEQN, -(counter:sealant))

model.complete = glm(unsound_flag ~ ., 
                     data = df.complete.train, 
                     family = binomial() )
## prediction on test
if(!require(ROCR)){install.packages('ROCR');library(ROCR)}
p <- predict( model.complete, newdata = df.complete.test, type = "response" )
pred = prediction(p, df.complete.test$unsound_flag)
perf.complete.glm = performance(pred, measure="tpr", x.measure="fpr")
auc.complete.glm = performance(pred, measure = 'auc')@y.values[[1]]
print(auc.complete.glm) ## 68.0%
png("results/figures/complete_glm_0680.png", width = 4, height = 4, units = 'in', res = 300)
plot(perf.complete.glm, linewidth = 3)
grid()
abline(0,1)
dev.off()


if(!require(mgcv)){install.packages("mgcv");library(mgcv)}
model.complete.gam = gam(unsound_flag ~ s(household_size, bs = 'ps') + 
                           s(family_size, bs = 'ps') +
                           s(household_size, bs = 'ps') + 
                           s(num_children, bs = 'ps') + 
                           s(num_adolescents, bs = 'ps') + 
                           s(num_elderly, bs = 'ps') + 
                           genderF + 
                           born_usa + 
                           citizen + 
                           interview_english + 
                           s(age, bs = 'ps') + 
                           ethnicity + 
                           lang + 
                           mnrsp + 
                           s(household_smokers_num, bs = 'ps') + 
                           s(household_cigarettes, bs = 'ps'), 
                         data = df.complete.train, 
                         family = binomial() )
## prediction on test
if(!require(ROCR)){install.packages('ROCR');library(ROCR)}
p <- predict( model.complete.gam, newdata = df.complete.test, type = "response" )
pred = prediction(as.numeric(p), df.complete.test$unsound_flag)
perf.complete.gam = performance(pred, measure="tpr", x.measure="fpr")
auc.complete.gam = performance(pred, measure = 'auc')@y.values[[1]]
print(auc.complete.gam) ## 68.5%
png("results/figures/complete_gam_0685.png", width = 4, height = 4, units = 'in', res = 300)
plot(perf.complete.gam, linewidth = 3)
grid()
abline(0,1)
dev.off()

png("results/figures/ROC_full_vs_complete.png", width = 6, height = 6, units = 'in', res = 300)
plot(perf.full_C5, xlim = c(0,1), ylim = c(0,1))
grid(lty = 'solid')
abline(0,1)
lines(perf.full_C5@x.values[[1]], perf.full_C5@y.values[[1]], lwd = 5, col = 'cornflowerblue')
lines(perf.complete.glm@x.values[[1]], perf.complete.glm@y.values[[1]], lwd = 5, col = 'forestgreen')
lines(perf.complete.gam@x.values[[1]], perf.complete.gam@y.values[[1]], lwd = 5, col = 'tomato')
legend('bottomright', legend = c('C5 RF - all fields', 'GLM - complete fields', 'GAM - complete fields'), col = c('cornflowerblue', 'forestgreen', 'tomato'), lwd = 3)
dev.off()

#### Information value ----
if(!require(Information)){install.packages("Information");library(Information)}
IV <- create_infotables(data=select(df.full, -SEQN, -(counter:sealant)), y="unsound_flag", ncore=2)

# Show the first records of the IV summary table
print(IV$Summary, row.names=FALSE)
a = data.frame(prop_missing = sapply(select(df.full, -SEQN, -(counter:sealant)), function(x) mean(is.na(x))))
a$Variable = rownames(a)
IV.summary = left_join(IV$Summary, a) %>% mutate(greater005 = ifelse(IV > 0.05, 1, 0))
subset(IV.summary, greater005 == 1, select = Variable)
# Show the WOE table for the variable called N_OPEN_REV_ACTS
# print(IV$Tables$cnt_weighted_sum_norm, row.names=FALSE)

# anova(glm.model, gam.model, test = 'Chisq') # GAM lower so better!
# AIC(glm.model, gam.model) #GAM lower so better

df_to_train_test = function(df){
  ## setting up independent training, validation and testing sets
  temp.ids = unique(df$SEQN)
  set.seed(123)
  temp.ids = sample(temp.ids, replace = F) ## sample without replacement
  train_val.split = length(temp.ids)*0.7
  val_test.split = length(temp.ids)*0.8
  train.ids = temp.ids[1:train_val.split]
  val.ids = temp.ids[(train_val.split + 1):val_test.split]
  test.ids = temp.ids[(val_test.split + 1):length(temp.ids)]
  return(list(train.ids, val.ids, test.ids))
}

model_predict_plot_ROC = function(model, testX, testY, model_name){
  ## prediction on test
  if(!require(ROCR)){install.packages('ROCR');library(ROCR)}
  p <- predict( model, newdata = testX, type = "response" )
  pred = prediction(as.numeric(p), testY)
  perf = performance(pred, measure="tpr", x.measure="fpr")
  auc = performance(pred, measure = 'auc')@y.values[[1]]
  print(auc) ## 68.5%
  png(paste0("results/figures/ROC_", model_name, "_", gsub(".", "", signif(auc, 3), fixed = T), ".png"), width = 6, height = 6, units = 'in', res = 300)
  plot(perf, linewidth = 3)
  grid()
  abline(0,1)
  dev.off()
}

## things we want to look at independently
if(!require(car)){install.packages('car');library(car)}

#breastfeeding
df.breastfeeding = df.full %>% select(SEQN, unsound_flag, diet_breastfed_flag:diet_other_started) %>%
  filter(!is.na(diet_breastfed_flag) | !is.na(diet_formula_flag))
df.breastfeeding = replace_na(df.breastfeeding, list(diet_breastfed_days = 0, diet_formula_started = 0, diet_formula_ended = 0, diet_formula_days = 0))
ids.breastfeeding = df_to_train_test(df.breastfeeding)
rpart(factor(unsound_flag) ~ ., data = df.breastfeeding[, -1])
#missmap(df.breastfeeding)
df.breastfeeding = na.omit(df.breastfeeding)

# model.breastfeeding.glm = glm(unsound_flag ~ ., data = filter(df.breastfeeding, SEQN %in% ids.breastfeeding[[1]]) %>% select(-SEQN), family = binomial())
model.breastfeeding.glm = glm(unsound_flag ~ ., 
                              #data = filter(df.breastfeeding, SEQN %in% ids.breastfeeding[[1]]) %>% select(-SEQN, -diet_formula_started), 
                              data = df.breastfeeding %>% select(-SEQN, -diet_formula_started), 
                              family = binomial())
summary(model.breastfeeding.glm)
car::vif(model.breastfeeding.glm)
## vif for diet_formula_started and diet_formula_days very high - remove diet_formula_started
model_predict_plot_ROC(model.breastfeeding.glm, 
                       filter(df.breastfeeding, SEQN %in% ids.breastfeeding[[3]]) %>% select(-SEQN, -diet_formula_started, -unsound_flag), 
                       filter(df.breastfeeding, SEQN %in% ids.breastfeeding[[3]]) %>% select(unsound_flag), "breastfeeding_subgroup")

summary(glm(unsound_flag ~ diet_breastfed_flag, data = df.breastfeeding, family = binomial()))
summary(glm(unsound_flag ~ diet_breastfed_days, data = df.breastfeeding, family = binomial()))
summary(glm(unsound_flag ~ diet_formula_flag, data = df.breastfeeding, family = binomial()))
summary(glm(unsound_flag ~ diet_formula_started, data = df.breastfeeding, family = binomial()))
summary(glm(unsound_flag ~ diet_formula_ended, data = df.breastfeeding, family = binomial()))
summary(glm(unsound_flag ~ diet_formula_days, data = df.breastfeeding, family = binomial()))
summary(glm(unsound_flag ~ diet_other_started, data = df.breastfeeding, family = binomial()))


model.breastfeeding.gam = gam(unsound_flag ~ diet_breastfed_flag + 
                                s(diet_breastfed_days, bs = 'ps') +
                                diet_formula_flag + 
                                s(diet_formula_ended, bs = 'ps') +
                                s(diet_formula_days, bs = 'ps') +
                                s(diet_other_started, bs = 'ps')
                                , data = filter(df.breastfeeding, SEQN %in% ids.breastfeeding[[1]]) %>% select(-SEQN, -diet_formula_started), family = binomial())
summary(model.breastfeeding.gam)
car::vif(model.breastfeeding.gam)
plot(model.breastfeeding.gam)

## smoking status, exhaled_ntr
df.smoking = na.omit(df.full %>% select(SEQN, unsound_flag, cotinine_bld, nnal_urn, exhaled_nitric_oxide, household_smokers_num, household_cigarettes, mother_smoked))
ids.smoking = df_to_train_test(df.smoking)

rpart(unsound_flag ~ ., data = df.smoking[, -1])

model.smoking.glm = glm(unsound_flag ~ ., 
                        #data = filter(df.smoking, SEQN %in% ids.smoking[[1]]) %>% select(-SEQN), 
                        data = df.smoking %>% select(-SEQN), 
                        family = binomial())
summary(model.smoking.glm)
cor(df.smoking$cotinine_bld, df.smoking$nnal_urn)
car::vif(model.smoking.glm)
## vifs okay
model_predict_plot_ROC(model.smoking.glm, 
                       filter(df.smoking, SEQN %in% ids.smoking[[3]]) %>% select(-SEQN, -unsound_flag), 
                       filter(df.smoking, SEQN %in% ids.smoking[[3]]) %>% select(unsound_flag), "smoking_subgroup")

summary(glm(unsound_flag ~ cotinine_bld, data = df.smoking, family = binomial())) # *
summary(glm(unsound_flag ~ nnal_urn, data = df.smoking, family = binomial())) # *
summary(glm(unsound_flag ~ exhaled_nitric_oxide, data = df.smoking, family = binomial())) # ns
summary(glm(unsound_flag ~ household_smokers_num, data = df.smoking, family = binomial()))
summary(glm(unsound_flag ~ household_cigarettes, data = df.smoking, family = binomial()))
summary(glm(unsound_flag ~ mother_smoked, data = df.smoking, family = binomial()))
## both cotinine and nnal are significantly associated with caries univariately. N02 is not.


## demographics and personal info
df.demo = df.full %>% select(SEQN, unsound_flag, income_ratio_over_poverty:ethnicity)
df.demo$ethnicity = relevel(df.demo$ethnicity, "white")
ids.demo = df_to_train_test(df.demo)

#missmap(df.demo)
df.demo = na.omit(df.demo)
df.demo$ethnicity = relevel(df.demo$ethnicity, 'white')

model.demo.glm = glm(unsound_flag ~ ., 
                     data = filter(df.demo, SEQN %in% ids.demo[[1]]) %>% select(-SEQN),
                     family = binomial())
summary(model.demo.glm)
car::vif(model.demo.glm)
## vifs okay
model_predict_plot_ROC(model.demo.glm, 
                       filter(df.demo, SEQN %in% ids.demo[[3]]) %>% select(-SEQN, -unsound_flag), 
                       filter(df.demo, SEQN %in% ids.demo[[3]]) %>% select(unsound_flag), "demographics_subgroup")

summary(glm(unsound_flag ~ age + relevel(factor(df.demo$ethnicity), 'white'), data = df.demo, family = binomial()))

library(mgcv)
model.demo.gam = mgcv::gam(unsound_flag ~ s(income_ratio_over_poverty, bs = 'ps') + 
                             s(num_children, bs = 'ps') + 
                             s(num_adolescents, bs = 'ps') + 
                             s(num_elderly, bs = 'ps') + 
                             citizen + 
                             s(age, bs = 'ps') + 
                             ethnicity, 
                           data = filter(df.demo, SEQN %in% ids.demo[[1]]) %>% select(-SEQN), family = binomial())
summary(model.demo.gam)
plot(model.demo.gam)
model_predict_plot_ROC(model.demo.gam, 
                       filter(df.demo, SEQN %in% ids.demo[[3]]) %>% select(-SEQN, -unsound_flag), 
                       filter(df.demo, SEQN %in% ids.demo[[3]]) %>% select(unsound_flag), "demographics_subgroup_GAM")

## plot the outcome over 1 year age bins


## cardio
df.cardio = df.full %>% select(SEQN, unsound_flag, weight_kg:diabp)
df.cardio$bmi_flag_child = relevel(factor(df.cardio$bmi_flag_child, labels = c('under', 'right', 'over', 'obese')), 'right')
ids.cardio = df_to_train_test(df.cardio)
#missmap(df.cardio)
df.cardio = na.omit(df.cardio)

model.cardio.glm = glm(unsound_flag ~ ., 
                     data = filter(df.cardio, SEQN %in% ids.cardio[[1]]) %>% select(-SEQN),
                     family = binomial())
summary(model.cardio.glm)
car::vif(model.cardio.glm)
## vifs not okay - weight and bmi -- drop weight for BMI
model.cardio.glm = glm(unsound_flag ~ ., 
                       data = filter(df.cardio, SEQN %in% ids.cardio[[1]]) %>% select(-SEQN, -weight_kg),
                       family = binomial())
summary(model.cardio.glm)
car::vif(model.cardio.glm)
## bmi flag 3/4 are significant, so is diastolic BP

model_predict_plot_ROC(model.cardio.glm, 
                       filter(df.cardio, SEQN %in% ids.cardio[[3]]) %>% select(-SEQN, -unsound_flag, -weight_kg), 
                       filter(df.cardio, SEQN %in% ids.cardio[[3]]) %>% select(unsound_flag), "cardio_subgroup_glm")
## AUC = 0.59
# Try GAM
library(mgcv)
model.cardio.gam = mgcv::gam(unsound_flag ~ s(weight_kg, bs = 'ps') + 
                             s(bmi, bs = 'ps') + bmi_flag_child + 
                             s(waist_cm, bs = 'ps') + 
                             s(sagittal_dia_cm, bs = 'ps') + 
                             s(height_cm, bs = 'ps') + 
                             s(pulse_60s, bs = 'ps') + 
                             pulse_irreg + 
                             s(sysbp, bs = 'ps') + 
                             s(diabp, bs = 'ps'), 
                           data = filter(df.cardio, SEQN %in% ids.cardio[[1]]) %>% select(-SEQN), family = binomial())
summary(model.cardio.gam)
## suddenly bmi_flag for under becomes significant, over very sig and obese signifcant with under > 0 and over/obest < 0
## cool
plot(model.cardio.gam)
model_predict_plot_ROC(model.cardio.gam, 
                       filter(df.cardio, SEQN %in% ids.cardio[[3]]) %>% select(-SEQN, -unsound_flag), 
                       filter(df.cardio, SEQN %in% ids.cardio[[3]]) %>% select(unsound_flag), "cardio_subgroup_GAM")
## performance got worse to 0.57

## food security maybe with diet


## univariate using df.full
## age
summary(glm(unsound_flag ~ age, data = df.full, family = binomial()))
## ethnicity
table(df.full$ethnicity) ## black actually most common
summary(glm(unsound_flag ~ relevel(factor(df.full$ethnicity), 'white'), data = df.full, family = binomial()))
summary(glm(unsound_flag ~ relevel(factor(df.full$ethnicity), 'black'), data = df.full, family = binomial()))
summary(glm(unsound_flag ~ birth_weight_lb, data = df.full, family = binomial()))
summary(glm(unsound_flag ~ num_elderly, data = df.full, family = binomial()))
table(df.full$num_elderly)
summary(glm(unsound_flag ~ food_security_cantafford, data = df.full, family = binomial()))
summary(glm(unsound_flag ~ food_security_notenough, data = df.full, family = binomial()))
table(df.full$food_security_cantafford)
table(df.full$food_security_notenough)
