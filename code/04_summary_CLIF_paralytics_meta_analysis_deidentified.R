library(meta)
library(metafor)
library(dplyr)
library(grid)

#Specific hospital names de-identified, per CLIF consortium policy

#random effects meta-analysis: 
#preCOVID/preROSE vs postROSE/COVID, unadjusted
meta_pre<- covid |>
  filter(total_proned>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$postROSE_COVID), seTE = meta_pre$postROSE_COVID_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

#preCOVID/preROSE vs postROSE/postCOVID, unadjusted
meta_pre<- post_covid |>
  filter(total_proned>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$post_ROSE_postCOVID), seTE = meta_pre$post_ROSE_postCOVID_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

#preCOVID/preROSE vs postROSE/COVID, adjusted
meta_pre<- covid_a |>
  filter(total_proned>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$Post_ROSE_COVID_a), seTE = meta_pre$Post_ROSE_COVID_a_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

#preCOVID/preROSE vs postROSE/postCOVID, adjusted
meta_pre<- post_covid_a |>
  filter(total_proned>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$post_ROSE_postCOVID_a), seTE = meta_pre$post_ROSE_postCOVID_a_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

#proned vs unproned, unadjusted
meta_pre<- prone_ma |>
  filter(total_proned>9) |>
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$prone_48), seTE = meta_pre$prone_48_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number, subgroup = meta_pre$hosp_type)
forest(  meta1,  layout = "JAMA",  backtransf = TRUE,  sortvar = subgroup,  test.subgroup = TRUE,  print.subgroup.name = FALSE,  subgroup.name = "")
grid.text("Proned Patients are More \n Likely to Receive NMBAs", x=.6, y=.975, gp=gpar(fontsize=10, fontface="bold"))
grid.text("Proned Patients are Less \n Likely to Receive NMBAs", x=.5, y=.975, gp=gpar(fontsize=10, fontface="bold"))
sum(meta_pre$n)

#pf among unproned patients
meta_pre<- pf_ma_unproned|>
  filter(n>50) |>
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1<- metagen(log(1/meta_pre$p_f_all)   , meta_pre$p_f_all_se , common=F, random=T, method.tau = "REML", sm="OR",   studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)
grid.text("Higher PaO2/FiO2 Ratio More \n Likely to Receive NMBAs", x=.5, y=.975, gp=gpar(fontsize=10, fontface="bold"))
grid.text("Lower PaO2/FiO2 Ratio More \n Likely to Receive NMBAs", x=.6, y=.975, gp=gpar(fontsize=10, fontface="bold"))

#ph among unproned patients
meta_pre<- ph_ma_unproned |>
  filter(n>50) |>
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1<- metagen(log(1/meta_pre$ph_unproned)   , meta_pre$ph_unproned_se , common=F, random=T, method.tau = "REML", sm="OR",   studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)
grid.text("Higher pH More \n Likely to Receive NMBAs", x=.5, y=.95, gp=gpar(fontsize=10, fontface="bold"))
grid.text("Lower pH More \n Likely to Receive NMBAs", x=.6, y=.95, gp=gpar(fontsize=10, fontface="bold"))

#SENSITIVITY ANALYSES: 
#no recent off-unit procedure
meta_pre<- covid_no_or |>
  filter(no_or>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$postROSE_COVID), seTE = meta_pre$postROSE_COVID_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_pre<- post_covid_no_or |>
  filter(no_or>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$post_ROSE_postCOVID), seTE = meta_pre$post_ROSE_postCOVID_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_pre<- prone_ma_no_or |>
  filter(no_or>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$prone_48), seTE = meta_pre$prone_48_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)
sum(meta_pre$n)

meta_pre<- covid_a_no_or |>
  filter(no_or>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$Post_ROSE_COVID_a), seTE = meta_pre$Post_ROSE_COVID_a_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_pre<- post_covid_a_no_or |>
  filter(no_or>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$post_ROSE_postCOVID_a), seTE = meta_pre$post_ROSE_postCOVID_a_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

#no covid-19 diagnosis
meta_pre<- covid_no_covid |>
  filter(no_covid>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$postROSE_COVID), seTE = meta_pre$postROSE_COVID_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_pre<- post_covid_no_covid |>
  filter(no_covid>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$post_ROSE_postCOVID), seTE = meta_pre$post_ROSE_postCOVID_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_pre<- prone_ma_no_covid |>
  filter(no_covid>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$prone_48), seTE = meta_pre$prone_48_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)
sum(meta_pre$n)

meta_pre<- covid_a_no_covid |>
  filter(no_covid>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$Post_ROSE_COVID_a), seTE = meta_pre$Post_ROSE_COVID_a_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_pre<- post_covid_a_no_covid |>
  filter(no_covid>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$post_ROSE_postCOVID_a), seTE = meta_pre$post_ROSE_postCOVID_a_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

#icd-10 consistent with ARDS
meta_pre<- covid_icd |>
  filter(ards>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  filter(hospital_id!="b") |>
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$postROSE_COVID), seTE = meta_pre$postROSE_COVID_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_pre<- post_covid_icd |>
  filter(ards>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  filter(hospital_id!="b") |>
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$post_ROSE_postCOVID), seTE = meta_pre$post_ROSE_postCOVID_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_pre<- prone_ma_icd |>
  filter(ards>9) |>
    mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  filter(hospital_number !=15) |> #remove hospital without proned non-paralyzed patients, unstable estimate
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$prone_48), seTE = meta_pre$prone_48_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)
sum(meta_pre$n)

meta_pre<- covid_a_icd |>
  filter(ards>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
    filter(hospital_id!="b") |>
    mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$Post_ROSE_COVID_a), seTE = meta_pre$Post_ROSE_COVID_a_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_pre<- post_covid_a_icd |>
  filter(ards>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  filter(hospital_id!="b") |>
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$post_ROSE_postCOVID_a), seTE = meta_pre$post_ROSE_postCOVID_a_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

#severe ards, per initial qualifying P/F or S/F ratio
meta_pre<- covid_severe |>
  filter(severe>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$postROSE_COVID), seTE = meta_pre$postROSE_COVID_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_pre<- post_covid_severe |>
  filter(severe>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$post_ROSE_postCOVID), seTE = meta_pre$post_ROSE_postCOVID_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_pre<- prone_ma_severe |>
  filter(severe>9) |>
    mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  filter(hospital_number!=15) |> #unstable esimate
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$prone_48), seTE = meta_pre$prone_48_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)
sum(meta_pre$n)

meta_pre<- covid_a_severe |>
  filter(severe>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
    mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$Post_ROSE_COVID_a), seTE = meta_pre$Post_ROSE_COVID_a_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_pre<- post_covid_a_severe |>
  filter(severe>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$post_ROSE_postCOVID_a), seTE = meta_pre$post_ROSE_postCOVID_a_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

#post-COVID/post-ROSE contemporary data
meta_pre<- prone_ma_now |>
  filter(now>9) |>
  filter(hospital_id!="a") |> #<25 pre-COVID patients
  filter(hospital_id!="b") |> #<25 patients per study period
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1 <- metagen(TE = log(meta_pre$prone_48), seTE = meta_pre$prone_48_se, common = FALSE, random = TRUE,  method.tau = "REML",  sm = "OR",  studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)
sum(meta_pre$n)

#Exploratory Mortality Landmark Analyasis
meta_pre<- landmark_unadjusted|>
  filter(n>50) |>
  filter(total_proned>9) |>
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1<- metagen(log(meta_pre$paralytics)   , meta_pre$paralytics_se , common=F, random=T, method.tau = "REML", sm="OR",   studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_pre<- landmark_adjusted|>
  filter(n>50) |>
  filter(total_proned>9) |>
  mutate(hospital_number_num = as.numeric(as.character(hospital_number))) |>
  arrange(hospital_number_num)
meta1<- metagen(log(meta_pre$paralytics)   , meta_pre$paralytics_se , common=F, random=T, method.tau = "REML", sm="OR",   studlab = meta_pre$hospital_number)
forest(meta1, layout = "JAMA", backtransf = TRUE)
