library(stm)
library(stmCorrViz)


############## REaD Prefitted Correlated Topic Model ##########

ctm <- readRDS("./saved_models/ctm_same_preprocessed.rds")
out <- readRDS("./saved_models/out_same_preprocessed.rds")

############ Prepare CLEAN TEXT FOR PRESENTATION ###############


# Basic Cleaning for Presentation
data_raw_no_chunk <- read.csv("./data/raw_nochunks.csv",stringsAsFactors = F)
data_presentation <- data_raw_no_chunk
data_presentation$presentation <- data_presentation$stripped

data_presentation <- data_presentation %>%
  mutate(presentation = str_replace_all(presentation,"(http[^ ]*)", " "))    # remove weblinks, replaced removing long strings
data_presentation$presentation <- gsub("[^[:alnum:][:blank:]+,.'!?[]()-:;]", "", data_presentation$presentation)

data_presentation <- data_presentation %>% 
  select(post_id, presentation)

# add presentation column
out$meta <- left_join(out$meta, data_presentation, by = "post_id")

# create correlated topic model correlation Viz
stmCorrViz(ctm, "ctm_corrviz.html",documents_raw = out$meta$presentation, documents_matrix = out$documents, display = T)

