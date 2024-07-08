50 repetitions

Simplified reputation-to-money model

["N-ind-mon" 100]
["N-coop" 0 100]
["N-defect" 0 100]
["self-enforcement?" true false]
["interdependence?" true false]
["evolutionary-updating?" true false]
["benefit-to-cost-ratio" 1 2 10 100]
["initial-balance-scores" 0 1 5 1000]
["threshold" 0]


cooperation-rate
count cooperators
count defectors
count ind-mons
sum [balance-score] of cooperators
sum [balance-score] of defectors
sum [balance-score] of ind-mons


ticks mod 250 = 0


time limit 5000