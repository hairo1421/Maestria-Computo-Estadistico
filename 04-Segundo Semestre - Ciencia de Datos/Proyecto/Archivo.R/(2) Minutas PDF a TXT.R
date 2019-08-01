#############################################
# Limpieza de las minutas PDF a TXT
#############################################
library("tm")
library("magrittr")
library("gridExtra")
library("RColorBrewer")
library("wordcloud")
library("ggplot2")
library("irlba")



######################################
# Minutas a texto plano
######################################

# ingrese la dirección donde se encuentran sus archivos 

dest <- "C:/Users/h_air/Desktop/Minutas/England/txt/Octubre 1998 21"

# make a vector of PDF file names
myfiles <- list.files(path = dest, pattern = "pdf",  full.names = TRUE)

# convert each PDF file that is named in the vector into a text file 
# text file is created in the same directory as the PDFs
# note that my pdftotext.exe is in a different location to yours
lapply(myfiles, function(i) system(paste('"C:/Users/h_air/Downloads/xpdf-tools-win-4.01.01/xpdf-tools-win-4.01.01/bin64/pdftotext.exe"', 
                                         paste0('"', i, '"')), wait = FALSE) )



#########################
######### FIN   #########
#########################