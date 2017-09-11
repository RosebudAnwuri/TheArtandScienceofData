Telecoms-tweets-database/Database initial setup.R
#Setup of sqlite database to store tweets
library(dplyr)
telecoms_db = src_sqlite("Telecoms tweets database",create = T)
copy_to(telecoms_db,final_file,temporary = F)