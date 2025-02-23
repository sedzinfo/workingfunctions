directory<-paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/")
directory<-gsub("working_examples/","",directory)

df_admission<-read.csv("https://raw.githubusercontent.com/researchpy/Data-sets/master/admission.csv")
df_automotive_data<-read.csv("https://raw.githubusercontent.com/researchpy/Data-sets/master/automotive_data.csv")
df_blood_pressure<-read.csv("https://raw.githubusercontent.com/researchpy/Data-sets/master/blood_pressure.csv")
df_crop_yield<-read.csv("https://raw.githubusercontent.com/researchpy/Data-sets/master/crop_yield.csv")
df_difficile<-read.csv("https://raw.githubusercontent.com/researchpy/Data-sets/master/difficile.csv")
df_insurance<-read.csv("https://raw.githubusercontent.com/researchpy/Data-sets/master/insurance.csv")
df_responses<-read.csv("https://raw.githubusercontent.com/researchpy/Data-sets/master/responses.csv")
df_responses_state<-read.csv("https://raw.githubusercontent.com/researchpy/Data-sets/master/responses_state.csv")
df_sexual_comp<-read.csv("https://raw.githubusercontent.com/researchpy/Data-sets/master/sexual_comp.csv")

save(df_admission,file=paste0(directory,"workingfunctions/data/df_admission.rda"))
save(df_automotive_data,file=paste0(directory,"workingfunctions/data/df_automotive_data.rda"))
save(df_blood_pressure,file=paste0(directory,"workingfunctions/data/df_blood_pressure.rda"))
save(df_crop_yield,file=paste0(directory,"workingfunctions/data/df_crop_yield.rda"))
save(df_difficile,file=paste0(directory,"workingfunctions/data/df_difficile.rda"))
save(df_insurance,file=paste0(directory,"workingfunctions/data/df_insurance.rda"))
save(df_responses,file=paste0(directory,"workingfunctions/data/df_responses.rda"))
save(df_responses_state,file=paste0(directory,"workingfunctions/data/df_responses_state.rda"))
save(df_sexual_comp,file=paste0(directory,"workingfunctions/data/df_sexual_comp.rda"))


