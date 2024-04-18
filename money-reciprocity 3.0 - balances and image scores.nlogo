; some results under evaluation (disregarding visibility for now - only comparing the image-score and money-balance mechanisms)
; money-cooperation more difficult to achieve, and more unstable, than recip-coop;
; when achieved, cooperation frequencies are lower (?)
; for very high benefit/cost - easier to establish money cooperation
; also argues for liquidity issues? where free-riders»
; to do:
; force k=0
; get behaviorspace overview
; check sensitivity to initial score spread
; how are balances distributed in each case?
; debt is common! and useful-?

extensions [rnd]

globals [
  benefit cost
  average-agent-fitness
  k-list
  k-payoffs-probs
  cooperations-this-round defections-this-round
]

turtles-own [
  fitness
  memory
  strategy
  score-balance
  current-partner
  my-k
]

; SET UP ENVIRONMENT AND PARAMETERS: strategies, benefit/cost, payoff evolution list
to setup
  clear-all

  set cost 1
  set benefit cost * benefit-to-cost-ratio

  set k-list (range -5 7)

  create-turtles population [
    set fitness 0
    set score-balance random 3 - 1
    set my-k one-of k-list
  ]

  set k-payoffs-probs n-values 12 [0]

  reset-ticks
end

; DEFINE BASE ACTIONS:
to cooperate
  set fitness fitness - cost
  set score-balance score-balance + 1
  ask current-partner [
    set fitness fitness + benefit
    if money? [set score-balance score-balance - 1]
  ]
  ; reporter:
  set cooperations-this-round cooperations-this-round + 1
end

to defect
  if not money? [set score-balance score-balance - 1]
  ; reporter:
  set defections-this-round defections-this-round + 1
end

; DYNAMICS (an approximation of Nowak 1998):
; Each turn represents one generation. Each agent acts once and, on average, is chosen as partner once (Nowak's m = 100%*)
to go
  ; Reset reporters:
  set cooperations-this-round 0
  set defections-this-round 0

  ; Prime agents and find a random match for this round:
  ask turtles [
    set fitness 0
    set current-partner one-of other turtles
  ]

  ; Agents act: they help if the partner's score (Nowak) or balance (Bigoni) is higher than their personal threshold:
  ask turtles [ ifelse [ score-balance ] of current-partner >= my-k [ cooperate ][ defect ] ]

  ; Scores and fitnesses are adjusted to maintain parameters (Nowak 1998)
  ask turtles [
    if score-balance  > 5 [set score-balance 5]
    if score-balance  < -5 [set score-balance -5]
    set fitness fitness + 1 ; added to avoid negative balances, as per Nowak 1998
  ]
  ; Set average-agent-fitness sum [fitness] of turtles / count turtles

  ; Run learning/evolution: procedure
  if offspring? [ spring-off ]

  if stopper? and ticks > 500 [stop]
  tick
end

; Define learning/evolution mechanism: strategy reproduction proportional to fitness (Nowak 1998) using random-weigthed choices (RND library). To confirm in light of Nowak 2005 attachments.
to spring-off
  set k-payoffs-probs map get-k-probs k-list
  let pairs (map list k-list k-payoffs-probs)

  ask turtles [
    set my-k first rnd:weighted-one-of-list pairs [ [p] -> last p ]
    if mutation? [ if random-float 1 < 0.001 [ set my-k one-of k-list ]]
  ]
end

to-report get-k-probs [ x ]
  report sum [fitness] of turtles with [my-k = x] / sum [fitness] of turtles
end
@#$#@#$#@
GRAPHICS-WINDOW
223
25
264
67
-1
-1
1.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
19
29
82
62
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
84
29
147
62
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
150
29
213
62
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
527
14
925
256
total fitness by strategy
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"-5 (cooperators)" 1.0 0 -13840069 true "" "plot sum [fitness] of turtles with [my-k = -5]"
"-4" 1.0 0 -1184463 true "" "plot sum [fitness] of turtles with [my-k = -4]"
"-3" 1.0 0 -987046 true "" "plot sum [fitness] of turtles with [my-k = -3]"
"-2" 1.0 0 -723837 true "" "plot sum [fitness] of turtles with [my-k = -2]"
"-1" 1.0 0 -526419 true "" "plot sum [fitness] of turtles with [my-k = -1]"
"0" 1.0 0 -13345367 true "" "plot sum [fitness] of turtles with [my-k = 0]"
"1" 1.0 0 -612749 true "" "plot sum [fitness] of turtles with [my-k = 1]"
"2" 1.0 0 -817084 true "" "plot sum [fitness] of turtles with [my-k = 2]"
"3" 1.0 0 -817084 true "" "plot sum [fitness] of turtles with [my-k = 3]"
"4" 1.0 0 -955883 true "" "plot sum [fitness] of turtles with [my-k = 4]"
"5" 1.0 0 -3844592 true "" "plot sum [fitness] of turtles with [my-k = 5]"
"6 (defectors)" 1.0 0 -12895429 true "" "plot sum [fitness] of turtles with [my-k = 6]"

PLOT
933
14
1348
254
strategy distribution
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"-5 (cooperators)" 1.0 0 -13840069 true "" "plot count turtles with [my-k = -5]"
"-4" 1.0 0 -1184463 true "" "plot count turtles with [my-k = -4]"
"-3" 1.0 0 -987046 true "" "plot count turtles with [my-k = -3]"
"-2" 1.0 0 -723837 true "" "plot count turtles with [my-k = -2]"
"-1" 1.0 0 -526419 true "" "plot count turtles with [my-k = -1]"
"0" 1.0 0 -13345367 true "" "plot count turtles with [my-k = 0]"
"1" 1.0 0 -408670 true "" "plot count turtles with [my-k = 1]"
"2" 1.0 0 -612749 true "" "plot count turtles with [my-k = 2]"
"3" 1.0 0 -817084 true "" "plot count turtles with [my-k = 3]"
"4" 1.0 0 -955883 true "" "plot count turtles with [my-k = 4]"
"5" 1.0 0 -6995700 true "" "plot count turtles with [my-k = 5]"
"6 (defectors)" 1.0 0 -12895429 true "" "plot count turtles with [my-k = 6]"

SLIDER
20
84
192
117
benefit-to-cost-ratio
benefit-to-cost-ratio
0
100
100.0
1
1
NIL
HORIZONTAL

MONITOR
772
473
870
518
average scores
sum [score-balance] of turtles / count turtles
1
1
11

INPUTBOX
25
125
93
185
population
100.0
1
0
Number

TEXTBOX
79
281
452
528
Nowak 1998: setup\nThe strategies are given by ki and the image levels by si. At the beginning of each generation, the image levels of all players are zero (assuming that children do not inherit the image of their parents). In succession, m donor–recipient pairs are chosen. A donor, i, cooperates with a recipient, j, if ki ≤ sj. The fitness of a player is given by the total number of points received during the m interactions. Some players may never be chosen, in which case their payoff from the game will be zero. On average, a player will be chosen 2m/n times, either as donor or as recipient. At the end of each generation, players leave offspring in proportion to the their fitness. We find that if the game is played for many generations, then eventually all players will adopt the same strategy. If the k value of this strategy is 0 or less then cooperation is established; if the value is 1 or more then defection has won. Cooperation is more likely to win if the number of interactions, m, per generation is large.
12
0.0
1

PLOT
527
321
727
471
histogram k
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-pen-mode 1\nset-plot-x-range -6 7\nset-histogram-num-bars 12" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [my-k] of turtles"

PLOT
730
321
930
471
histogram scores
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-pen-mode 1\nset-plot-x-range -6 7\nset-histogram-num-bars 12" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [score-balance] of turtles"

PLOT
935
321
1347
471
cooperation frequency
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "carefully [ plot cooperations-this-round / ( cooperations-this-round + defections-this-round ) ] []"

MONITOR
1091
474
1218
519
cooperation frequency
cooperations-this-round / ( cooperations-this-round + defections-this-round )
1
1
11

TEXTBOX
28
597
1195
697
We observe endless cycles of cooperation and defection. Cooperative populations are relatively stable if they consist of discriminating players with strategies such as k = 0 or −1. But after some time these populations are undermined (through random drift) by players with strategies such as k = −4 or −5, which are too cooperative. Then defectors, with strategies k = 4 or 5, can invade. These defectors can, in turn, be overcome by stern discriminators again. In the long run, cooperation is harmed by unconditional cooperators, because they enable defectors to invade. In the absence of unconditional cooperators, cooperative populations persist for much longer. a, The average k value of the population. b, The average payoff per individual, per generation. c, Frequency distribution of strategies sampled over many generations (t = 107). Parameter values are as for Fig. 1 , but m = 300 rounds per generation.\n\n
10
0.0
1

SWITCH
202
179
295
212
mutation?
mutation?
0
1
-1000

TEXTBOX
27
657
402
1138
interdependence: \n\nAnother interesting expansion of the basic model is to include strategies that consider both the recipient's and the donor's image score. We explored two types of strategies. ‘And’ strategies involve cooperation if the image score of the recipient is larger than a certain value and the image score of the donor is less than a certain value. The idea is that if an individual has already a high image score, it is not necessary to aim for a still higher image score (by helping others). On the other hand, ‘or’ strategies result in cooperation if the image score of the recipient is larger than a certain value or the image score of the donor is less than a certain value. Here the idea is that if an individual has a low image score it may be advantageous to increase the score by helping others regardless of how low their image score is. In both cases, highly cooperative societies form (Fig 4). If, in contrast, we simulate strategies that only consider their own image and do not take into account the image of the recipient, cooperation does not emerge.
10
0.0
1

TEXTBOX
434
659
1037
881
analytical and/or balance?\n\nThe models above are based on computer simulations, but we can derive analytical insights from a simplified model. Suppose that there are only two image levels, 0 (for bad) and 1 (for good). The image of a player depends on his or her last action as a donor: players who defected have score 0, and players who cooperated have score 1. Let us only consider two types of player: first, defectors, who never provide assistance; and second, discriminators who help players having image 1, but not players having image 0. A given player knows the score of only a fraction, q, of the population. A discriminator who has no information on potential recipients will assume, with a certain probability, p, that they have image 1. In each round of the game all individuals of the population are chosen, each with the same probability of being a donor or a recipient. If w < 1 denotes the probability of another round, there are on average 1/(1 − w) rounds per generation. We have derived the equations (see Methods) that describe how the frequencies of discriminators and defectors change from one generation to the next. It should be stressed that discriminators are not ‘tit-for-tat’ players; tit-for-tat strategists base their decisions on their own previous experience with the co-player, whereas discriminators use the experience of others. This is an essential advantage for a player who interacts with many co-players but only a few times with each. (Such discriminators are also different from strategies based on ‘standing’27, which is an internal switch distinguishing between defection in response to a co-player's cooperation or defection.)
10
0.0
1

SWITCH
202
143
295
176
offspring?
offspring?
0
1
-1000

SWITCH
201
84
293
117
money?
money?
0
1
-1000

MONITOR
586
477
671
522
NIL
count turtles
1
1
11

SWITCH
1366
190
1495
223
stopper?
stopper?
1
1
-1000

INPUTBOX
26
189
95
249
initial-scores
0.0
1
0
Number

MONITOR
661
258
753
303
overall welfare
sum [fitness] of turtles
1
1
11

MONITOR
1048
519
1180
564
NIL
cooperations-this-round
17
1
11

MONITOR
1181
520
1299
565
NIL
defections-this-round
17
1
11

TEXTBOX
302
87
500
126
Off: Nowak 1998 image-score replication\nOn: money-balance modification
10
0.0
1

TEXTBOX
28
578
178
596
Nowak 1998: results
10
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
