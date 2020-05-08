# Set working directory
setwd('/home/sothomas/R_code/soil')
#setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')

# Clear folders

for (soil_layer in c('ts', 'ms')) {
  for (i in 1:12) {
  
    file_path = paste0('./compaction/characterization_factors_', soil_layer, '/', i, '/')
  
    files = list.files(path = file_path)
  
    for (f in files) {
      file.remove(paste0(file_path, f))
    }
  }
}