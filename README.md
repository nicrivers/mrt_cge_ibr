This folder contains files to replicate the analysis in:
Bohringer, C., Fischer, C., Rivers, N. "Rebating revenues from a unilateral emission price"

To replicate all tables and figures in the paper, you will need a recent version of GAMS and R.

1. Run "run_script.R" to produce all results. This produces the file "master_results.csv". Run time is 1-2 days
2. Run "benchmark_graphs.R" to produce the table of benchmark country statistics
3. Run "analysis_script.R" to produce all figures. This reads in the file "master_results.csv"
