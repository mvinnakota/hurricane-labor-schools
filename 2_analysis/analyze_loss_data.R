
# Loss Regression

# run a tract-level regression of loss values ~ direct hit + indirect hit + year FE + tract FE 
# this will show whether there were a lot of losses in our definition of indirect tracts 
# if we find that direct hit tracts losses >>> indirect tract losses, that validates our research design


# load in data 
load("intermediates/hazus_loss_cleaned.Rda")

# list of cz's with 64kt winds 
cz_with_64 <- unique(hazus_loss_final$cz_2000[hazus_loss_final$direct_64 == 1])
cz_with_50 <- unique(hazus_loss_final$cz_2000[hazus_loss_final$direct_50 == 1])


# get naive comparison of means 
mean(hazus_loss_final$EconLoss[hazus_loss_final$direct_64 == 1], na.rm = T)
mean(hazus_loss_final$EconLoss[hazus_loss_final$direct_50 == 1], na.rm = T)
mean(hazus_loss_final$EconLoss[hazus_loss_final$direct_50 == 1 & is.na(hazus_loss_final$direct_64)], na.rm = T)

mean(hazus_loss_final$EconLoss[hazus_loss_final$cz_2000 %in% cz_with_64 & is.na(hazus_loss_final$direct_64)], 
     na.rm = T)
mean(hazus_loss_final$EconLoss[hazus_loss_final$cz_2000 %in% cz_with_64 & is.na(hazus_loss_final$direct_50)], 
     na.rm = T)

