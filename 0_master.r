rm(list = ls())
dev.off()

# load relevant the data files
source('codes/1_loadData.r')

# source the age-structured SEIcIscR model functions
source('codes/function_model.r')

# source the age-structured SEIcIscR model functions
source('codes/function_postprocessing.r')

# simulate N oubtreaks
print("SEIR")
source('codes/2_simOutbreak_ncov_SEIR.r')

print("SEIcIscR")
source('codes/2_simOutbreak_ncov_SEIcIscR.r')






