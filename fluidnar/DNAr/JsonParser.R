#Obter lista de ids de goticulas simples, criar objeto de goticulas para cada com tempo inicial e final padrão
#Obter as goticulas misturadas, atualizar tempo final dos objetos das goticulas que as formam. 
#Criar objeto de goticulas misturadas.
#Ordenas objetos de goticulas por tempo inicial.
install.packages("jsonlite")
# Load the jsonlite package
library(jsonlite)

parseJson <- function(){
  # Path to your JSON file
  json_file_path <- "C:/Dissertação Microfluídica/result.json"
  
  # Read the JSON file
  json_data <- fromJSON(json_file_path)
  
  # Extract droplet IDs
  droplet_ids <- json_data$droplets$id
  
  # Extract merged droplet IDs
  merged_droplet_ids <- json_data$droplets$mergedDropletIds
  
  times_of_merge <- c()
  merged_droplets_ids_list <- c()
  id_of_the_new_droplet <- c()
  final_simulation_time <- c()
  
  # Iterate over droplet IDs
  for (i in seq_along(droplet_ids)) {
    if (length(merged_droplet_ids[[i]]) > 1) {
      times_of_merge <- append(times_of_merge, merged_droplet_ids[[i]][length(merged_droplet_ids[[i]])])
      merged_droplets_ids_list <- append(merged_droplets_ids_list, c(merged_droplet_ids[[i]][1],merged_droplet_ids[[i]][2]))
      id_of_the_new_droplet <- append(id_of_the_new_droplet, droplet_ids[i])
      final_simulation_time <- append(final_simulation_time, json_data$states[[length(json_data$states)]][length(json_data$states[[length(json_data$states)]])])
    }
  }
 
  return(list(times_of_merge= times_of_merge, 
              final_simulation_time= final_simulation_time, 
              merged_droplet_ids= merged_droplets_ids_list, 
              id_of_the_new_droplet= id_of_the_new_droplet))
}