Purpose: to evaluate stability of money strategy (and others) in fact of different high-defection scenarios, across different conditions of liquidity and benefit/cost ratio.


BEHAVIORSPACE SETUP: 

WITH MONEY: 25 repetitions, time limit 10000 steps, data collected every 250 steps (4000 runs)

["N-coop" 25 100]
["N-defect" 100 200 500 1000]
["N-direct" 100]
["N-indirect" 100]
["N-money" 100]

["benefit-to-cost-ratio" 1.1 2 5 100]

["initial-money" 0 0.5 1 5 5000]
["debt-threshold" 0]
["quid-pro-quo?" true]

["evolutionary-updating?" true]

["initial-reputation" 1]
["reputation-threshold" -1]
["forgiveness?" true]
["full-memory?" true]
["error-noise" 0]
["force-mutate-share" 0]
["cumulative-fitness?" false]
["full-evolutionary-update?" false]

WITHOUT MONEY: 25 repetitions, time limit 10000 steps, data collected every 250 steps (800 runs)