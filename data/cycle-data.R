
# data sets used in Wichert, Fokianos, and Strimmer (2004)



## yeast data

load("spellmann-yeast.rda.gz")

dim(cdc15)       # 24 x 4289
dim(cdc28)       # 17 x 1365
dim(alpha)       # 18 x 4415
dim(elution)     # 14 x 5695


##  caulobacter data

library("GeneCycle")
data(caulobacter)

dim(caulobacter) # 11 x 1444



## human fibroblast data

load("fibroblasts.rda.gz")

dim(humanN2)     # 13 x 4574
dim(humanN3)     # 12 x 5079



## human hela data

load("humanhela.rda.gz")

dim(score1)      # 12 x 14728
dim(score2)      # 26 x 15472
dim(score3)      # 48 x 39724
dim(score4)      # 19 x 39192
dim(score5)      #  9 x 34890




