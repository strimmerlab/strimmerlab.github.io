
# data sets used in Boulesteix and Strimmer (2005)


## hemoglobin data

load("hemoglobin.rda.gz")

dim(hemoglobin.mixed.spectra)  # 7 x 321
dim(hemoglobin.mixing)         # 7 x 3
dim(hemoglobin.pure.spectra)   # 3 x 321  (=true values)


##  ecoli data

load("kaoecoli.rda.gz")

dim(ecoli.expression)   # 100 x 23
dim(ecoli.connection)   # 100 x 16
length(ecoli.timepoint) # 23


## yeast data

load("yeast-tfa.rda.gz")

dim(gasch.expression)    # 1993 x 173
dim(gasch.connection)    # 1993 x 113
dim(spellman.expression) # 3638 x 24
dim(spellman.connection) # 3638 x 113



