library(data.table)
library(psych)

# CHANGE THESE
path_to_raw <- '../../data/raw/study2.csv'
path_to_preprocessed <- '../../data/raw/study2_raw_new.csv'

# Calculate alpha with preprocessed data
d <- fread(path_to_preprocessed)
alpha(d[, c('affect_gut_subj', 'affect_interessant_subj','affect_stark_subj','affect_aktiv_subj')])
# Warnmeldung:
# In alpha(d[, c("affect_gut_subj", "affect_interessant_subj", "affect_stark_subj",  :
#   Some items were negatively correlated with the total scale and probably 
# should be reversed. 


# Recalculate it with the SAME subjects with the RAW data
dd <- fread(path_to_raw)
dd <- dd[lfdn %in% unique(d$id), grep('affect', names(dd), value = T), with = F]
dd <- melt(dd, measure = patterns('Interessant','Gut', 'Aktiv', 'Stark'))
setnames(dd, 2:5, c('Interessant','Gut', 'Aktiv', 'Stark'))
alpha(dd[, 2:5])
# Gives NO such warning