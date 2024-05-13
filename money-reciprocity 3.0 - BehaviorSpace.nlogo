; pending: review evolution/learning to match Nowak 1998 proportional reproduction
extensions [ rnd ]

globals [ benefit cost C D cooperation-rate cumulative-coop-rate ;initial-populations
  ; variables for BehaviorSpace no-learning validation
  fitness-cooperators-this-round
  fitness-defectors-this-round
  fitness-directs-this-round
  fitness-indirects-this-round
  fitness-moneys-this-round
]
turtles-own [ fitness memory score balance strategy current-partner cumulative-fitness ]




breed [ cooperators cooperator ]
breed [ defectors defector ]
breed [ directs direct ]
breed [ indirects indirect ]
breed [ moneys money ]

; BehaviorSpace setup auxiliary routines:
to BS-set-populations
  ;set initial-populations [ "explicit" ]
  ;SETUP Ninitial for each trategy

  ; direct memory-size to total population (max), set forgiveness false
end

to setup
  clear-all

  set cost 1
  set benefit cost * benefit-to-cost-ratio

  create-cooperators N-coop
  create-defectors N-defect
  create-directs N-direct
  create-indirects N-indirect
  create-moneys N-money

  ;BS:
  if full-memory? [ set memory-size count turtles ] ;REMOVED FOR DIRECT RECIPROCITY CHECK

  ask turtles [
    set fitness 0
    set memory []
    set score initial-reputation
    set balance initial-money
    fd 5
  ]

  reset-ticks
end

to go
  ;set reputation-threshold (sum [score] of turtles / count turtles) - 1 average reputation scoring! quite effective
  set C 0
  set D 0

  ask turtles [
    set fitness 0 ;testing
    set current-partner one-of other turtles
  ]

  ask cooperators [ cooperate ]
  ask defectors [ defect ]
  ask directs [ ifelse not member? current-partner memory [ cooperate ][ defect if forgiveness? [ if member? current-partner memory [ set memory remove current-partner memory] ]] ] ; TO CODE: REMOVE ONCE PUNISHED
  ask indirects [ ifelse [score] of current-partner > reputation-threshold [ cooperate ][ defect ]] ; if already punished, remove (Fr) ] ]
  ask moneys [ ifelse [balance] of current-partner > debt-threshold
    [ cooperate if quid-pro-quo? [set balance balance + 1 ask current-partner [ set balance balance - 1 ]]]
    [ defect ]
  ]

  ask turtles [ set fitness fitness + 1 ] ; Correction to avoid negative probabilities during evolutionary learning (Nowak 1998)
  track-fitness
  if evolutionary-updating? [ spring-off ]



  set cooperation-rate C / (C + D)

  tick
end


to cooperate
  set fitness fitness - cost
  set score score + 1
  if not quid-pro-quo? [set balance balance + 1]
  ask current-partner [
    set fitness fitness + benefit
    if not quid-pro-quo? [set balance balance - 1]
  ]
  set C C + 1
end


to defect
  set score score - 1
  ask current-partner [
    if length memory < memory-size [ set memory lput myself memory ];[ set memory lput myself memory set memory remove-item 0 memory]
  ]

  set D D + 1
end

to track-fitness
  ask turtles [ set cumulative-fitness cumulative-fitness + fitness ]
  set fitness-cooperators-this-round sum [fitness] of cooperators
  set fitness-defectors-this-round sum [fitness] of defectors
  set fitness-directs-this-round sum [fitness] of directs
  set fitness-indirects-this-round sum [fitness] of indirects
  set fitness-moneys-this-round sum [fitness] of moneys
end

to spring-off ; (Nowak 2005 Evolutionary updating: "in each time step a random individual is chosen to die; the neighbors compete for the empty site proportional to their fitness")
  ask one-of turtles [
    set breed [breed] of rnd:weighted-one-of turtles [fitness]
    ;if mutation? if [ random-float < 0.001 [ set breed one-of breeds ]

  ; for invasion/"handcraft mutation" tests:
    if random-float 1 < force-mutate-share [ choose-breed ]
  ]
end


to choose-breed
  let x random 5
  if x = 0 [set breed cooperators]
  if x = 1 [set breed defectors]
  if x = 2 [set breed directs]
  if x = 3 [set breed indirects]
  if x = 4 [set breed moneys]

end

to set-equals
  set N-coop set-equals-n
  set N-defect set-equals-n
  set N-direct set-equals-n
  set N-indirect set-equals-n
  set N-money set-equals-n

end
@#$#@#$#@
GRAPHICS-WINDOW
248
38
289
80
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
6
27
69
60
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
73
27
136
60
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
139
27
202
60
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
377
33
803
274
total fitness
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
"direct-reciprocators" 1.0 0 -955883 true "" "plot sum [fitness] of directs"
"indirect-reciprocators" 1.0 0 -1184463 true "" "plot sum [fitness] of indirects"
"money-users" 1.0 0 -13791810 true "" "plot sum [fitness] of moneys"
"defectors" 1.0 0 -7500403 true "" "plot sum [fitness] of defectors"
"cooperators" 1.0 0 -13840069 true "" "plot sum [fitness] of cooperators"

SLIDER
17
66
189
99
benefit-to-cost-ratio
benefit-to-cost-ratio
0
100
10.0
1
1
NIL
HORIZONTAL

MONITOR
566
281
664
326
average scores
sum [score] of turtles / count turtles
1
1
11

MONITOR
746
278
831
323
total balances
sum [balance] of turtles
1
1
11

INPUTBOX
214
26
266
86
N-coop
100.0
1
0
Number

INPUTBOX
270
26
325
86
N-defect
100.0
1
0
Number

INPUTBOX
19
163
78
223
N-direct
100.0
1
0
Number

INPUTBOX
17
265
80
325
N-indirect
100.0
1
0
Number

INPUTBOX
17
364
81
424
N-money
100.0
1
0
Number

MONITOR
364
286
498
331
average memory length
(sum [length memory] of turtles) / count turtles
1
1
11

SLIDER
82
179
223
212
memory-size
memory-size
0
count turtles
500.0
1
1
NIL
HORIZONTAL

MONITOR
914
279
1049
324
average agent fitness
sum [fitness] of turtles / count turtles
1
1
11

PLOT
705
326
878
452
balances
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
"default" 1.0 0 -13791810 true "" "plot sum [balance] of moneys"
"pen-1" 1.0 0 -955883 true "" "plot sum [balance] of directs"
"pen-2" 1.0 0 -1184463 true "" "plot sum [balance] of indirects"
"pen-3" 1.0 0 -13840069 true "" "plot sum [balance] of cooperators"
"pen-4" 1.0 0 -7500403 true "" "plot sum [balance] of defectors"

PLOT
1138
32
1477
273
surviving strategies
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
"cooperators" 1.0 0 -13840069 true "" "plot count cooperators"
"defectors" 1.0 0 -7500403 true "" "plot count defectors"
"direct reciprocators" 1.0 0 -955883 true "" "plot count directs"
"indirect reciprocators" 1.0 0 -1184463 true "" "plot count indirects"
"money users" 1.0 0 -13791810 true "" "plot count moneys"

PLOT
526
331
694
451
scores
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
"default" 1.0 0 -955883 true "" "plot sum [score] of directs"
"pen-1" 1.0 0 -1184463 true "" "plot sum [score] of indirects"
"pen-2" 1.0 0 -13791810 true "" "plot sum [score] of moneys"
"pen-3" 1.0 0 -7500403 true "" "plot sum [score] of defectors"
"pen-4" 1.0 0 -13840069 true "" "plot sum [score] of cooperators"

PLOT
808
32
1135
273
per-agent fitness
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
"default" 1.0 0 -955883 true "" "carefully [ plot sum [fitness] of directs / count directs ] []"
"pen-1" 1.0 0 -1184463 true "" "carefully [ plot sum [fitness] of indirects / count indirects ][]"
"pen-2" 1.0 0 -13791810 true "" "carefully [ plot sum [fitness] of moneys / count moneys ][]"
"pen-3" 1.0 0 -13840069 true "" "carefully [ plot sum [fitness] of cooperators / count cooperators ][]"
"pen-4" 1.0 0 -7500403 true "" "carefully [ plot sum [fitness] of defectors / count defectors ][]"

INPUTBOX
177
265
283
325
reputation-threshold
-1.0
1
0
Number

INPUTBOX
83
265
173
325
initial-reputation
1.0
1
0
Number

INPUTBOX
84
364
172
424
initial-money
0.0
1
0
Number

INPUTBOX
176
364
282
424
debt-threshold
-1.0
1
0
Number

TEXTBOX
109
116
259
134
MECHANISMS:
10
0.0
1

TEXTBOX
37
133
274
172
Direct-reciprocators remember defectors and only help if current partner is not in memory
10
0.0
1

TEXTBOX
37
231
260
283
Indirect Reciprocators only help agents with a reputation score higher than a threshold
10
0.0
1

TEXTBOX
52
332
263
371
\"Money users\" only help agents with a \"money balance\" higher than a threshold
10
0.0
1

TEXTBOX
21
429
291
572
Background mechanics (always active):\nWhen an agent cooperates with a partner:\n- the agent's reputation score is increased\n- the partner's money balance is decreased**\n- the agent's money balance is increased**\nWhen an agent does not cooperate:\n- the agent's reputation score is decreased\n- the agent enters partner's memory\nBUT note the role of Quid-pro-Quo: when active, this happens only for \"money\" agents
10
0.0
1

SWITCH
885
365
1057
398
evolutionary-updating?
evolutionary-updating?
0
1
-1000

SWITCH
1237
557
1364
590
quid-pro-quo?
quid-pro-quo?
0
1
-1000

MONITOR
1333
440
1428
485
NIL
cooperation-rate
11
1
11

MONITOR
714
586
873
631
NIL
variance [balance] of moneys
17
1
11

PLOT
704
454
878
579
variance of balances
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
"default" 1.0 0 -13791810 true "" "carefully [plot variance [balance] of moneys] []"
"pen-1" 1.0 0 -7500403 true "" "carefully [plot variance [balance] of turtles] []"

SWITCH
1239
594
1358
627
forgiveness?
forgiveness?
0
1
-1000

BUTTON
1058
534
1144
567
NIL
set-equals
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1158
521
1230
581
set-equals-n
300.0
1
0
Number

TEXTBOX
1071
497
1221
523
BEHAVIORSPACE-ONLY: MODIFICATIONS
10
0.0
1

PLOT
1140
279
1475
425
cooperation rate
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
"default" 1.0 0 -16777216 true "" "plot cooperation-rate"

TEXTBOX
900
408
1050
426
NO MUTATION?
10
0.0
1

TEXTBOX
916
433
1066
459
RANDOM OR FITNESS-BASED ELIMINATION?
10
0.0
1

SWITCH
1237
522
1360
555
full-memory?
full-memory?
0
1
-1000

TEXTBOX
922
481
1072
507
IF FITNESS; CUMULATIVE OR PER-TURN?
10
0.0
1

SLIDER
331
475
503
508
force-mutate-share
force-mutate-share
0
1
0.0
0.001
1
NIL
HORIZONTAL

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
<experiments>
  <experiment name="experiment" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>cooperation-rate</metric>
    <metric>count cooperators</metric>
    <metric>count defectors</metric>
    <metric>count directs</metric>
    <metric>count indirects</metric>
    <metric>count moneys</metric>
    <metric>(sum [length memory] of turtles) / count turtles</metric>
    <metric>sum [balance] of cooperators</metric>
    <metric>sum [balance] of defectors</metric>
    <metric>sum [balance] of directs</metric>
    <metric>sum [balance] of indirects</metric>
    <metric>sum [balance] of moneys</metric>
    <metric>sum [score] of cooperators</metric>
    <metric>sum [score] of defectors</metric>
    <metric>sum [score] of directs</metric>
    <metric>sum [score] of indirects</metric>
    <metric>sum [score] of moneys</metric>
    <runMetricsCondition>ticks mod 200 = 0</runMetricsCondition>
    <enumeratedValueSet variable="N-coop">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-defect">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-direct">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-indirect">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-money">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit-to-cost-ratio">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-money">
      <value value="0"/>
      <value value="1"/>
      <value value="10"/>
      <value value="999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debt-threshold">
      <value value="0"/>
      <value value="-10"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quid-pro-quo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolutionary-updating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-reputation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-threshold">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forgiveness?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="reference experiment - sensitivity analysis full sweep 100x5 v2" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>cooperation-rate</metric>
    <metric>count cooperators</metric>
    <metric>count defectors</metric>
    <metric>count directs</metric>
    <metric>count indirects</metric>
    <metric>count moneys</metric>
    <metric>(sum [length memory] of turtles) / count turtles</metric>
    <metric>sum [balance] of cooperators</metric>
    <metric>sum [balance] of defectors</metric>
    <metric>sum [balance] of directs</metric>
    <metric>sum [balance] of indirects</metric>
    <metric>sum [balance] of moneys</metric>
    <metric>sum [score] of cooperators</metric>
    <metric>sum [score] of defectors</metric>
    <metric>sum [score] of directs</metric>
    <metric>sum [score] of indirects</metric>
    <metric>sum [score] of moneys</metric>
    <runMetricsCondition>ticks mod 200 = 0</runMetricsCondition>
    <enumeratedValueSet variable="N-coop">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-defect">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-direct">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-indirect">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-money">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit-to-cost-ratio">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-money">
      <value value="0"/>
      <value value="1"/>
      <value value="10"/>
      <value value="999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debt-threshold">
      <value value="0"/>
      <value value="-10"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quid-pro-quo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolutionary-updating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-reputation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-threshold">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forgiveness?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="reciprocity verification" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>cooperation-rate</metric>
    <metric>count cooperators</metric>
    <metric>count defectors</metric>
    <metric>count directs</metric>
    <metric>count indirects</metric>
    <metric>count moneys</metric>
    <metric>(sum [length memory] of turtles) / count turtles</metric>
    <metric>sum [balance] of cooperators</metric>
    <metric>sum [balance] of defectors</metric>
    <metric>sum [balance] of directs</metric>
    <metric>sum [balance] of indirects</metric>
    <metric>sum [balance] of moneys</metric>
    <metric>sum [score] of cooperators</metric>
    <metric>sum [score] of defectors</metric>
    <metric>sum [score] of directs</metric>
    <metric>sum [score] of indirects</metric>
    <metric>sum [score] of moneys</metric>
    <metric>fitness-cooperators-this-round</metric>
    <metric>fitness-defectors-this-round</metric>
    <metric>fitness-directs-this-round</metric>
    <metric>fitness-indirects-this-round</metric>
    <metric>fitness-moneys-this-round</metric>
    <runMetricsCondition>ticks mod 100 = 0</runMetricsCondition>
    <enumeratedValueSet variable="N-money">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-defect">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolutionary-updating?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit-to-cost-ratio">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-money">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debt-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quid-pro-quo?">
      <value value="true"/>
    </enumeratedValueSet>
    <subExperiment>
      <enumeratedValueSet variable="N-indirect">
        <value value="0"/>
        <value value="&quot;N-coop&quot;"/>
        <value value="0"/>
        <value value="100"/>
        <value value="&quot;memory-size&quot;"/>
        <value value="1"/>
        <value value="10"/>
        <value value="50"/>
        <value value="200"/>
        <value value="&quot;forgiveness?&quot;"/>
        <value value="true"/>
        <value value="false"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="N-direct">
        <value value="100"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="N-direct">
        <value value="0"/>
        <value value="&quot;N-coop&quot;"/>
        <value value="0"/>
        <value value="100"/>
        <value value="&quot;initial-reputation&quot;"/>
        <value value="-5"/>
        <value value="0"/>
        <value value="1"/>
        <value value="5"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="N-indirect">
        <value value="100"/>
      </enumeratedValueSet>
    </subExperiment>
  </experiment>
  <experiment name="reference experiment - without evolution 100x5 500steps 500repetitions" repetitions="500" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>cooperation-rate</metric>
    <metric>count cooperators</metric>
    <metric>count defectors</metric>
    <metric>count directs</metric>
    <metric>count indirects</metric>
    <metric>count moneys</metric>
    <metric>(sum [length memory] of turtles) / count turtles</metric>
    <metric>sum [balance] of cooperators</metric>
    <metric>sum [balance] of defectors</metric>
    <metric>sum [balance] of directs</metric>
    <metric>sum [balance] of indirects</metric>
    <metric>sum [balance] of moneys</metric>
    <metric>sum [score] of cooperators</metric>
    <metric>sum [score] of defectors</metric>
    <metric>sum [score] of directs</metric>
    <metric>sum [score] of indirects</metric>
    <metric>sum [score] of moneys</metric>
    <metric>fitness-cooperators-this-round</metric>
    <metric>fitness-defectors-this-round</metric>
    <metric>fitness-directs-this-round</metric>
    <metric>fitness-indirects-this-round</metric>
    <metric>fitness-moneys-this-round</metric>
    <enumeratedValueSet variable="N-coop">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-defect">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-direct">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-indirect">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-money">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit-to-cost-ratio">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-money">
      <value value="0"/>
      <value value="1"/>
      <value value="10"/>
      <value value="999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debt-threshold">
      <value value="0"/>
      <value value="-10"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quid-pro-quo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolutionary-updating?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-reputation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-threshold">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forgiveness?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="reference experiment - without evolution 125x4 500steps 500repetitions" repetitions="500" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>cooperation-rate</metric>
    <metric>count cooperators</metric>
    <metric>count defectors</metric>
    <metric>count directs</metric>
    <metric>count indirects</metric>
    <metric>count moneys</metric>
    <metric>(sum [length memory] of turtles) / count turtles</metric>
    <metric>sum [balance] of cooperators</metric>
    <metric>sum [balance] of defectors</metric>
    <metric>sum [balance] of directs</metric>
    <metric>sum [balance] of indirects</metric>
    <metric>sum [balance] of moneys</metric>
    <metric>sum [score] of cooperators</metric>
    <metric>sum [score] of defectors</metric>
    <metric>sum [score] of directs</metric>
    <metric>sum [score] of indirects</metric>
    <metric>sum [score] of moneys</metric>
    <metric>fitness-cooperators-this-round</metric>
    <metric>fitness-defectors-this-round</metric>
    <metric>fitness-directs-this-round</metric>
    <metric>fitness-indirects-this-round</metric>
    <metric>fitness-moneys-this-round</metric>
    <enumeratedValueSet variable="N-coop">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-defect">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-direct">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-indirect">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-money">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit-to-cost-ratio">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-money">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debt-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quid-pro-quo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolutionary-updating?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-reputation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-threshold">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forgiveness?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="half-large population - sensitivity analysis full sweep 125x4 250r" repetitions="250" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>cooperation-rate</metric>
    <metric>count cooperators</metric>
    <metric>count defectors</metric>
    <metric>count directs</metric>
    <metric>count indirects</metric>
    <metric>count moneys</metric>
    <metric>(sum [length memory] of turtles) / count turtles</metric>
    <metric>sum [balance] of cooperators</metric>
    <metric>sum [balance] of defectors</metric>
    <metric>sum [balance] of directs</metric>
    <metric>sum [balance] of indirects</metric>
    <metric>sum [balance] of moneys</metric>
    <metric>sum [score] of cooperators</metric>
    <metric>sum [score] of defectors</metric>
    <metric>sum [score] of directs</metric>
    <metric>sum [score] of indirects</metric>
    <metric>sum [score] of moneys</metric>
    <metric>fitness-cooperators-this-round</metric>
    <metric>fitness-defectors-this-round</metric>
    <metric>fitness-directs-this-round</metric>
    <metric>fitness-indirects-this-round</metric>
    <metric>fitness-moneys-this-round</metric>
    <runMetricsCondition>ticks mod 200 = 0</runMetricsCondition>
    <enumeratedValueSet variable="N-coop">
      <value value="313"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-defect">
      <value value="313"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-direct">
      <value value="312"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-indirect">
      <value value="312"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-money">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit-to-cost-ratio">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-money">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debt-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quid-pro-quo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolutionary-updating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-reputation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-threshold">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forgiveness?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="half-large population - sensitivity analysis full sweep 100x5 250r" repetitions="250" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>cooperation-rate</metric>
    <metric>count cooperators</metric>
    <metric>count defectors</metric>
    <metric>count directs</metric>
    <metric>count indirects</metric>
    <metric>count moneys</metric>
    <metric>(sum [length memory] of turtles) / count turtles</metric>
    <metric>sum [balance] of cooperators</metric>
    <metric>sum [balance] of defectors</metric>
    <metric>sum [balance] of directs</metric>
    <metric>sum [balance] of indirects</metric>
    <metric>sum [balance] of moneys</metric>
    <metric>sum [score] of cooperators</metric>
    <metric>sum [score] of defectors</metric>
    <metric>sum [score] of directs</metric>
    <metric>sum [score] of indirects</metric>
    <metric>sum [score] of moneys</metric>
    <metric>fitness-cooperators-this-round</metric>
    <metric>fitness-defectors-this-round</metric>
    <metric>fitness-directs-this-round</metric>
    <metric>fitness-indirects-this-round</metric>
    <metric>fitness-moneys-this-round</metric>
    <runMetricsCondition>ticks mod 200 = 0</runMetricsCondition>
    <enumeratedValueSet variable="N-coop">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-defect">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-direct">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-indirect">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-money">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit-to-cost-ratio">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-money">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debt-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quid-pro-quo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolutionary-updating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-reputation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-threshold">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forgiveness?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="reference experiment - sensitivity analysis full sweep high-defection 500r" repetitions="500" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>cooperation-rate</metric>
    <metric>count cooperators</metric>
    <metric>count defectors</metric>
    <metric>count directs</metric>
    <metric>count indirects</metric>
    <metric>count moneys</metric>
    <metric>(sum [length memory] of turtles) / count turtles</metric>
    <metric>sum [balance] of cooperators</metric>
    <metric>sum [balance] of defectors</metric>
    <metric>sum [balance] of directs</metric>
    <metric>sum [balance] of indirects</metric>
    <metric>sum [balance] of moneys</metric>
    <metric>sum [score] of cooperators</metric>
    <metric>sum [score] of defectors</metric>
    <metric>sum [score] of directs</metric>
    <metric>sum [score] of indirects</metric>
    <metric>sum [score] of moneys</metric>
    <runMetricsCondition>ticks mod 200 = 0</runMetricsCondition>
    <enumeratedValueSet variable="N-coop">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-defect">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-direct">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-indirect">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-money">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit-to-cost-ratio">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-money">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debt-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quid-pro-quo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolutionary-updating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-reputation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-threshold">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forgiveness?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="full-memory?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="reference experiment - sensitivity analysis full sweep high-defection 500r 125x4" repetitions="500" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>cooperation-rate</metric>
    <metric>count cooperators</metric>
    <metric>count defectors</metric>
    <metric>count directs</metric>
    <metric>count indirects</metric>
    <metric>count moneys</metric>
    <metric>(sum [length memory] of turtles) / count turtles</metric>
    <metric>sum [balance] of cooperators</metric>
    <metric>sum [balance] of defectors</metric>
    <metric>sum [balance] of directs</metric>
    <metric>sum [balance] of indirects</metric>
    <metric>sum [balance] of moneys</metric>
    <metric>sum [score] of cooperators</metric>
    <metric>sum [score] of defectors</metric>
    <metric>sum [score] of directs</metric>
    <metric>sum [score] of indirects</metric>
    <metric>sum [score] of moneys</metric>
    <runMetricsCondition>ticks mod 200 = 0</runMetricsCondition>
    <enumeratedValueSet variable="N-coop">
      <value value="84"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-defect">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-direct">
      <value value="83"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-indirect">
      <value value="83"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-money">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit-to-cost-ratio">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-money">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debt-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quid-pro-quo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolutionary-updating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-reputation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-threshold">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forgiveness?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="full-memory?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="large population - sensitivity analysis full sweep 300x5 20k-steps 100r" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>cooperation-rate</metric>
    <metric>count cooperators</metric>
    <metric>count defectors</metric>
    <metric>count directs</metric>
    <metric>count indirects</metric>
    <metric>count moneys</metric>
    <metric>(sum [length memory] of turtles) / count turtles</metric>
    <metric>sum [balance] of cooperators</metric>
    <metric>sum [balance] of defectors</metric>
    <metric>sum [balance] of directs</metric>
    <metric>sum [balance] of indirects</metric>
    <metric>sum [balance] of moneys</metric>
    <metric>sum [score] of cooperators</metric>
    <metric>sum [score] of defectors</metric>
    <metric>sum [score] of directs</metric>
    <metric>sum [score] of indirects</metric>
    <metric>sum [score] of moneys</metric>
    <metric>fitness-cooperators-this-round</metric>
    <metric>fitness-defectors-this-round</metric>
    <metric>fitness-directs-this-round</metric>
    <metric>fitness-indirects-this-round</metric>
    <metric>fitness-moneys-this-round</metric>
    <runMetricsCondition>ticks mod 500 = 0</runMetricsCondition>
    <enumeratedValueSet variable="N-coop">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-defect">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-direct">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-indirect">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-money">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit-to-cost-ratio">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-money">
      <value value="0"/>
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debt-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quid-pro-quo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolutionary-updating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-reputation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-threshold">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forgiveness?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="full-memory?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="large population - sensitivity analysis full sweep 375x4 20k-steps 100r (copy)" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>cooperation-rate</metric>
    <metric>count cooperators</metric>
    <metric>count defectors</metric>
    <metric>count directs</metric>
    <metric>count indirects</metric>
    <metric>count moneys</metric>
    <metric>(sum [length memory] of turtles) / count turtles</metric>
    <metric>sum [balance] of cooperators</metric>
    <metric>sum [balance] of defectors</metric>
    <metric>sum [balance] of directs</metric>
    <metric>sum [balance] of indirects</metric>
    <metric>sum [balance] of moneys</metric>
    <metric>sum [score] of cooperators</metric>
    <metric>sum [score] of defectors</metric>
    <metric>sum [score] of directs</metric>
    <metric>sum [score] of indirects</metric>
    <metric>sum [score] of moneys</metric>
    <metric>fitness-cooperators-this-round</metric>
    <metric>fitness-defectors-this-round</metric>
    <metric>fitness-directs-this-round</metric>
    <metric>fitness-indirects-this-round</metric>
    <metric>fitness-moneys-this-round</metric>
    <runMetricsCondition>ticks mod 500 = 0</runMetricsCondition>
    <enumeratedValueSet variable="N-coop">
      <value value="375"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-defect">
      <value value="375"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-direct">
      <value value="375"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-indirect">
      <value value="375"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-money">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit-to-cost-ratio">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-money">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debt-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quid-pro-quo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolutionary-updating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-reputation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reputation-threshold">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forgiveness?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="full-memory?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
