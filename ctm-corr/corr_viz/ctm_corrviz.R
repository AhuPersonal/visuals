library(stm)
library(stmCorrViz)
library(tidyverse)
library(stringr)

############## REaD Prefitted Correlated Topic Model ##########

ctm <- readRDS("ctm_same_preprocessed.rds")
out <- readRDS("out_same_preprocessed.rds")

############ Prepare CLEAN TEXT FOR PRESENTATION ###############


# Basic Cleaning for Presentation
# NOTE: give the link to the file of the raw_no_chunk data creation
data_raw_no_chunk <- read.csv("raw_nochunks.csv",stringsAsFactors = F)
data_presentation <- data_raw_no_chunk
data_presentation$presentation <- data_presentation$stripped

data_presentation <- data_presentation %>%
  mutate(presentation = str_replace_all(presentation,"(http[^ ]*)", " "),    # remove weblinks, replaced removing long strings
         presentation = str_replace_all(presentation, "\\b\\S{20,}\\b", " "),   # DANGER: all words over 20 chars
         presentation = str_replace_all(presentation, "\\b\\w+[.]\\w+", "   "),   # remove stuff like blah.com 
         presentation = str_replace_all(presentation,"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "  "), # email addresses              # remove backticks around some code chunks
         presentation = str_replace_all(presentation,"\\{[^}]*\\}"," "),      # remove css code wrapped around {}
         presentation = str_replace_all(presentation, "[\r\n]"," "),          # remove new lines
         presentation = str_replace_all(presentation, "\\\"", ""),             # experimental, remove escaped quotations
         presentation = str_replace_all(presentation, "\\\\", ""),             # experimental, remove escape characters on special characters
         presentation = str_replace_all(presentation, "\\w*=\\w+", " "),       # experimental, remove anything with an equal sign touching it
         presentation = str_replace_all(presentation, "\\w*\\/\\w*", " "),      # words that touch a forward slash
         presentation = str_replace_all(presentation, ">|<", " ")) # experimental angle brackets



data_presentation <- data_presentation %>%
  
  mutate(
    
    presentation = str_replace_all(presentation, "[[:punct:]]* *(\\w+[']\\w+)|[[:punct:]]+ *| {2,}", " \\1"),
    presentation = str_replace_all(presentation, "[[:digit:]]", " "),   # remove numbers
    presentation = str_replace_all(presentation, "\\$", " "),           # remove dollar signs
    presentation = str_replace_all(presentation, "\`", " "),            # for some reason backticks survive
    presentation = str_replace_all(presentation, "\\b\\S{20,}\\b", " "),   # DANGER: all words over 20 chars
    presentation = str_replace_all(presentation, "\\b\\ {2,}\\b", " "), # remove whitespace more than 2 long
    presentation = str_replace_all(presentation, "\\s\\s+", " "),      # as above, more rigourous
    presentation = str_replace_all(presentation, "^[ \t]+|[ \t]+$","")) # remove extra wspace beg/end of line

data_presentation <- data_presentation %>% 
  select(post_id, presentation)

# add presentation column
out$meta <- left_join(out$meta, data_presentation, by = "post_id")

# create correlated topic model correlation Viz
stmCorrViz(ctm, "ctm_corrviz.html",documents_raw = out$meta$presentation, documents_matrix = out$documents, display = T)

