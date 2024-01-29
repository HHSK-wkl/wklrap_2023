library(pdftools)
library(tidyverse)

list.files("pdfs", full.names = TRUE) %>% 
  pdftools::pdf_combine(output = "pdfs/waterkwaliteitsrapportage_2023_hhsk.pdf")

