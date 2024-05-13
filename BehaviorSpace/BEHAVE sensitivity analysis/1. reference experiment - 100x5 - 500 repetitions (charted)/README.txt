Experiment run on NetLogo BehaviorSpace with all 5 strategies and varying money parameters.
The experiment was run in two separate occasions: first with 200 repetitions and later with 300 repetitions. Results were later joined for analysis in R.



Notes:
Best parameters for direct and reciprocity strategies were chosen for the benchmark (based on qualitative searches).

Experiment parameters are copied below:

["N-coop" 100]
["N-defect" 100]
["N-direct" 100]
["N-indirect" 100]
["N-money" 100]
["benefit-to-cost-ratio" 1 2 5 20]
["initial-money" 0 1 10 999]
["debt-threshold" 0 -10 10]
["quid-pro-quo?" true]
["evolutionary-updating?" true]
["initial-reputation" 1]
["reputation-threshold" -1]
["forgiveness?" true]
(memory = population maximum)