library(tidyverse)
library(janeaustenr)
library(tidytext)
library(stringr)
library(readxl)
library(gutenbergr)
library(magrittr)
library(foreach)

# Importar cada solicitud
# Contar frecuencia de cada palabra
# Hacer match basado en cosine similiarity
# Entrenar un clasificador de una regresión cuál número asignar.


# read austen books -------------------------------------------------------
tmp <- austen_books()


# thesis_words <- data_frame(file = paste0("~/thesis/thesis/", 
#                                          c("introduction.tex", "lit-review.tex", "methods.tex", 
#                                            "results.tex", "discussion.tex"))) %>%
# mutate(text = map(file, read_lines))

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

tidy_books <- original_books %>%
  unnest_tokens(word, text)

# import data into a tidy text format -------------------------------------

data(stop_words)

# gutenberg ----------------------------------------------------------------

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

dim(hgwells)

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)

# Stop words in spanish ---------------------------------------------------

library(tm)

stop_words_spanish <- stopwords(kind = "spanish")

stop_words_spanish %<>% c("usted", "ustedes")

# Read in stop words in spanish from other library

list.files('../stopwords-es/raw/')

# Read all the files in a folder
data_path <-'../stopwords-es/raw/'

stop_words_df <- foreach(current_file = list.files(data_path), .combine = rbind)%do%{
  
  # print(current_file)
  df_tmp <- file.path(data_path, current_file) %>% read_tsv(col_names = FALSE)
  
  if(ncol(df_tmp) == 2){
    df_tmp %<>% dplyr::select(X2) %>% rename(X1 = X2)
  }
  
  df_tmp
  
}

stop_words_df %<>% rbind(stop_words_spanish)

stop_words_df %<>% distinct()



# test --------------------------------------------------------------------

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)


tmp <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
          mutate(tidy_hgwells, author = "H.G. Wells"), 
          mutate(tidy_books, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word)%>%
  group_by(author) %>%
  mutate(proportion = n / sum(n))

tmp %<>%
  select(-n) %>%
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)
