library(tidyverse)
library(readxl)

# Run the benchmark model
system("gams model s=mdl --tax=yes")

# initialize the results file
results_file <- "master_results.csv"
if (file.exists(results_file)) file.remove(results_file)

# Run the scenarios
counter <- 0

# Batching results for quicker execution
results_batch <- list()
batch_size <- 20  # or 10
batch_counter <- 0

# Loop over carbon tax and scc rates
 for (co2p in seq(0,500,by=25)) {
   for (scc in seq(0,500,by=25)) {
     for (mdl in c("mrt", "soe")) {
       for (rg in c("USA","EUR")) {
         for (arm in c("ref", "hi", "lo")) {
           for (esube in c("ref", "hi", "lo")) {
            

    # Time
    start <- Sys.time()
             
    # Execute the model
    cmd <- paste0("cmd /c \"gams scen r=mdl --region=", rg,
                  " --co2p=", co2p,
                  " --scc=", scc,
                  " --ssa=no",
                  " --mdl=", mdl,
                  " --arm=", arm,
                  " --esube=", esube,
                  " > NUL 2>&1\"")
    
    system(cmd)
    
    # Load the data
    dat_scen <- read_excel("single.xlsx", sheet="macro", skip=1, col_names = c("item", "sector", "region", "policy", "value"))
    
    # Add parameters
    dat_scen$co2p <- co2p
    dat_scen$scc <- scc
    dat_scen$region_implementing <- rg
    dat_scen$mdl <- mdl
    dat_scen$arm <- arm
    dat_scen$esube <- esube
    
    # Append to an output file
    results_batch[[length(results_batch) + 1]] <- dat_scen
    batch_counter <- batch_counter + 1
    
    # Write to file every 20 iterations
    if (batch_counter == batch_size) {
      batch_df <- bind_rows(results_batch)
      write_csv(batch_df, results_file, append = file.exists(results_file), col_names = !file.exists(results_file))
      results_batch <- list()
      batch_counter <- 0
    }
    

    
    # Remove temp files
    # List all directories in the current working directory
    dirs <- list.dirs(path = ".", full.names = FALSE, recursive = FALSE)
    
    # Filter for 4-character alphanumeric folders (like '225a', '7b6z')
    gams_temp_dirs <- dirs[grepl("^[0-9a-z]{4}$", dirs)]
    
    # Remove them
    unlink(gams_temp_dirs, recursive = TRUE)
    
    # Other gams files
    unlink(c("scen.lst", "single.xlsx", "single.gdx"))
    
    counter <- counter + 1
    print("*****************************************")
    print("*****************************************")
    print("")
    print(paste("Done ",counter, " scenarios. Last scenario took ", Sys.time() - start, "seconds"))
    print("")
    print("*****************************************")
    print("*****************************************")
    flush.console()
          }
        }
      }
    }
  }
}

# Cleanup any remaining unwritten data
if (length(results_batch) > 0) {
  batch_df <- bind_rows(results_batch)
  write_csv(batch_df, results_file, append = TRUE, col_names = FALSE)
}
