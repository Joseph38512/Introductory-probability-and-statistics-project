
library(readxl)
library(data.table)

data1 <- data.table(read_excel('biomarkers.xlsx'))
data2 <- read_excel('covariates.xlsx')




data1[, c("PatientID","Biomarker") := tstrsplit(Biomarker, "-",
                fixed = TRUE)]



print(data1[PatientID==5])


