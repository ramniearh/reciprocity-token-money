;Dawkins 1976 "scratch back", Trivers 1971
; see interpretation issues with evolutionary fitness x individual satisfaction
; review https://www.cambridge.org/core/journals/journal-of-institutional-economics/article/abs/money-and-its-institutional-substitutes-the-role-of-exchange-institutions-in-human-cooperation/F3CEF9F728283724A6A6852E8B14DD75
; MESO and institutions/coop https://www.sciencedirect.com/science/article/abs/pii/S105353570900064X

; MODEL PENDING/CHECK:
; Token strategy: if all holders have token, there should be no trades
        ; strange behavior in token swap? undetermined strategy?
        ; alternative: quantified tokens. should generate concentration? TEST

; new turtles created with average fitness.
     ; alternative of resetting total fitness at every round privileges cheaters (doesn't leave enough time to accumulate?)


; questions for parameter-sweeping:
; ok, suckers are vulnerable to cheater invasion. cheaters are vulnerable to grudger invasion, for certain parameters (-check: benefit/cost > than..
; but! grudger total welfare higher than token!
; RQ: under which circumstances/parameters is a grudger population vulnerable to token invasion? and under which circumstances is this welfare-enhancing?
; without grudger memory cap: always outperforms? or depends on structure of tokens?
; with grudger memory cap - exogenous explanation, ok. still, how does token structure affect welfare? and what are some constraints on which token routines might evolve?
; then compare token with reputation
; then spatialize
;
;
;
; also ofcourse review structured populations, Smaldino 2017; x division of labor as a modifier to conclusions above


; review Nowak ESS RD AD:
;Figure 2.
;Evolutionary dynamics of cooperators and defectors. The red and blue arrows indicate
;selection favoring defectors and cooperators, respectively. (a) Without any mechanism for
;the evolution of cooperation, defectors dominate. A mechanism for evolution of cooperation
;can allow cooperators to be the evolutionarily stable strategy (ESS), risk dominant (RD) or
;advantageous (AD) in comparison with defectors. (b) Cooperators are ESS if they can resist
;invasion by defectors. (c) Cooperators are RD if the basin of attraction of defectors is less
;than 1/2. (d) Cooperators are AD if the basin of attraction of defectors is less that 1/3. In this
;case, the fixation probability of a single cooperator in a finite population of defectors is
;greater than the inverse of the population size (for weak selection). (e) Some mechanisms
;allow cooperators to dominate defectors.


; finally: in this framework, money IS a social norm? x araújo 2004


globals [ active-strategies total-welfare avg-welfare sucker-welfare cheater-welfare grudger-welfare token-welfare
  total-welfare-per-tick
  avg-welfare-per-tick
  sucker-welfare-per-tick
  cheater-welfare-per-tick
  grudger-welfare-per-tick
  token-welfare-per-tick
  total-welfare-1000
  avg-welfare-1000
  sucker-welfare-1000
  cheater-welfare-1000
  grudger-welfare-1000
  token-welfare-1000
  total-welfare-10000
  avg-welfare-10000
  sucker-welfare-10000
  cheater-welfare-10000
  grudger-welfare-10000
  token-welfare-10000

]

turtles-own [ has-token? my-strategy fitness blacklist ]

to setup
  clear-all
  set active-strategies [ ]
  if sucker [ set active-strategies lput "sucker" active-strategies ]
  if cheater [ set active-strategies lput "cheater" active-strategies ]
  if grudger-memory-cap > 0 [ set active-strategies lput "grudger" active-strategies ]
  if token% > 0 [ set active-strategies lput "token" active-strategies ]
  if ( not sucker ) and ( not cheater ) and ( grudger-memory-cap = 0 ) and ( token% = 0 ) [ set active-strategies lput "NOSTRAT-ERROR"active-strategies]

  crt population [ right random 180 fd 13
    set fitness 0
    ifelse random 100 < token% [set has-token? true][set has-token? false]
    set my-strategy one-of active-strategies
    set blacklist []
    color-me
    shape-me
  ]

  reset-ticks
  tick
end

to color-me
  if my-strategy = "sucker" [ set color green ]
  if my-strategy = "cheater" [ set color grey ]
  if my-strategy = "grudger" [ set color orange ]
  if my-strategy = "token" [ set color blue ]
end

to shape-me
  ifelse has-token? [ set shape "star" ][set shape "default"]
  if my-strategy = "NOSTRAT-ERROR" [set shape "turtle"]
end


to go

  ask links [ die ]

  ask turtles [
    ;set fitness 0 ; reset all fitnesses! testing
    find-partner
    interact
    shape-me
  ]

  set sucker-welfare sum [fitness] of turtles with [my-strategy = "sucker"]
  set cheater-welfare sum [fitness] of turtles with [my-strategy = "cheater"]
  set grudger-welfare sum [fitness] of turtles with [my-strategy = "grudger"]
  set token-welfare sum [fitness] of turtles with [my-strategy = "token"]

  set total-welfare sum [fitness] of turtles
  set avg-welfare total-welfare / population

  if evolve? [evolve-all]

  tick
  report-welfare-per-tick
  if ticks = 1000 [report1000]
  if ticks = 10000 [report10000]
end

to find-partner
  if ( not any? link-neighbors ) and ( any? other turtles with [ not any? link-neighbors ] ) [
    create-link-with one-of other turtles with [ not any? link-neighbors ]
  ]
end

to interact
    if my-strategy = "sucker" [
    groom
  ]
    if my-strategy = "cheater" [
    not-groom ;implies received groomed but did not reciprocate
  ]
    if my-strategy = "grudger" [ ;nice tit for tat. evaluate grim trigger?
    ifelse not member? item 0 [other-end] of my-links blacklist [ groom ] [ not-groom ]
  ]
    if ( my-strategy = "token" ) and has-token? = false [ ;"conditional hastoken"
      ifelse [has-token?] of item 0 [other-end] of my-links = true [
      groom
      ask item 0 [other-end] of my-links [set has-token? false]
      set has-token? true
    ]
    [ not-groom ] ;RECHECK TOKEN BEHAVIOR WITH 0 tokens
    ; implement token-swap (self-myself)?
  ]
  ; other variations of token? actual swap interaction; unconditional groom-for-token (even if already has token)? add quantifiable/addable tokens?
  ; add reciprocator? only groom whitelist? need nice trigger?
end

to groom
  set fitness fitness - cost
  ask link-neighbors [
    set fitness fitness + benefit
  ]
end

to not-groom
  ask link-neighbors [
    if not member? myself blacklist [
      set blacklist fput myself blacklist
    ]
    if length blacklist > grudger-memory-cap [
      set blacklist remove-item ( length blacklist - 1 ) blacklist
    ]; cap length of memory string
  ]
end

to report-welfare-per-tick
  set total-welfare-per-tick total-welfare / ticks
  set avg-welfare-per-tick avg-welfare / ticks
  set sucker-welfare-per-tick sucker-welfare / ticks
  set cheater-welfare-per-tick cheater-welfare / ticks
  set grudger-welfare-per-tick grudger-welfare / ticks
  set token-welfare-per-tick token-welfare / ticks

end


to report1000
  set total-welfare-1000 total-welfare
  set avg-welfare-1000 avg-welfare
  set sucker-welfare-1000 sucker-welfare
  set cheater-welfare-1000 cheater-welfare
  set grudger-welfare-1000 grudger-welfare
  set token-welfare-1000 token-welfare
end

to report10000
  set total-welfare-10000 total-welfare
  set avg-welfare-10000 avg-welfare
  set sucker-welfare-10000 sucker-welfare
  set cheater-welfare-10000 cheater-welfare
  set grudger-welfare-10000 grudger-welfare
  set token-welfare-10000 token-welfare
end



to token-swap [ A B ]; risk of double-swap with two interactions per turn? - implies some DC or?
  ;tokens disappearing!
end

to evolve-all
  let n replacement-rate * population

  ask min-n-of n turtles [ fitness ] [ die ]
  create-turtles n [ right random 180 fd random 13
    set fitness avg-welfare ;new turtles have average fitness. alternatively, reset and recalculate fitness?
    ;set fitness 0;testing
    ifelse random 100 < token% [set has-token? true][set has-token? false] ;REVIEW
    set my-strategy one-of active-strategies
    set blacklist []
    color-me
    shape-me
  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
241
12
678
450
-1
-1
13.0
1
10
1
1
1
0
0
0
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

INPUTBOX
25
39
95
99
population
100.0
1
0
Number

BUTTON
335
462
398
495
NIL
setup
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
406
463
469
496
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
475
463
538
496
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

SWITCH
23
236
113
269
sucker
sucker
0
1
-1000

SWITCH
23
274
113
307
cheater
cheater
0
1
-1000

TEXTBOX
45
213
195
243
--AVAILABLE STRATEGIES--
11
0.0
1

INPUTBOX
101
39
151
99
cost
2.0
1
0
Number

INPUTBOX
157
40
210
100
benefit
5.0
1
0
Number

TEXTBOX
70
22
220
40
--PARAMETERS--
11
0.0
1

PLOT
698
174
1181
324
per-capita welfare of strategy
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
"sucker" 1.0 0 -10899396 true "" "plot sucker-welfare "
"cheater" 1.0 0 -7500403 true "" "plot cheater-welfare"
"grudger" 1.0 0 -955883 true "" "plot grudger-welfare"
"token" 1.0 0 -13345367 true "" "plot token-welfare"
"total" 1.0 0 -16777216 true "" "plot total-welfare"

PLOT
698
21
1181
171
strategy distribution
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
"\"suckers\"" 1.0 0 -10899396 true "" "plot count turtles with [ my-strategy = \"sucker\" ]"
"\"cheaters\"" 1.0 0 -7500403 true "" "plot count turtles with [ my-strategy = \"cheater\" ]"
"\"grudgers\"" 1.0 0 -955883 true "" "plot count turtles with [ my-strategy = \"grudger\" ]"
"\"token\"" 1.0 0 -13345367 true "" "plot count turtles with [ my-strategy = \"token\" ]"

MONITOR
835
331
920
376
NIL
total-welfare
1
1
11

MONITOR
924
331
1004
376
NIL
avg-welfare
1
1
11

INPUTBOX
115
125
211
185
replacement-rate
0.01
1
0
Number

MONITOR
741
331
824
376
NIL
count turtles
17
1
11

SLIDER
19
348
111
381
token%
token%
0
100
0.0
1
1
NIL
HORIZONTAL

SWITCH
22
141
112
174
evolve?
evolve?
0
1
-1000

MONITOR
1013
331
1122
376
circulating tokens
count turtles with [has-token? = true]
0
1
11

TEXTBOX
814
399
1001
427
(TOTAL) WELFARE REPORTERS
11
0.0
1

TEXTBOX
857
425
1007
443
at 1000 ticks
11
0.0
1

TEXTBOX
1034
424
1184
442
at 10000 ticks
11
0.0
1

MONITOR
927
449
987
494
total
total-welfare-1000
1
1
11

MONITOR
855
440
922
485
sucker
sucker-welfare-1000
1
1
11

MONITOR
856
487
922
532
cheater
cheater-welfare-1000
1
1
11

MONITOR
857
536
923
581
grudger
grudger-welfare-1000
1
1
11

MONITOR
856
584
922
629
token
token-welfare-1000
1
1
11

MONITOR
1108
488
1166
533
total
total-welfare-10000
1
1
11

MONITOR
1030
438
1099
483
sucker
sucker-welfare-10000
1
1
11

MONITOR
1030
486
1100
531
cheater
cheater-welfare-10000
1
1
11

MONITOR
1030
534
1100
579
grudger
grudger-welfare-10000
1
1
11

MONITOR
1030
581
1100
626
token
token-welfare-10000
1
1
11

MONITOR
928
536
991
581
average
avg-welfare-1000
1
1
11

MONITOR
1107
536
1166
581
average
avg-welfare-10000
1
1
11

SLIDER
6
310
125
343
grudger-memory-cap
grudger-memory-cap
0
population
0.0
1
1
NIL
HORIZONTAL

MONITOR
700
437
757
482
sucker
sucker-welfare-per-tick
1
1
11

MONITOR
701
488
758
533
cheater
cheater-welfare-per-tick
1
1
11

MONITOR
701
540
758
585
grudger
grudger-welfare-per-tick
1
1
11

MONITOR
702
588
759
633
token
token-welfare-per-tick
1
1
11

TEXTBOX
706
420
856
438
per tick
11
0.0
1

MONITOR
766
489
823
534
total
total-welfare-per-tick
17
1
11

MONITOR
766
540
825
585
average
avg-welfare-per-tick
1
1
11

PLOT
1215
421
1628
624
per-tick welfare of strategy
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
"total" 1.0 0 -16777216 true "" "plot total-welfare-per-tick"
"sucker" 1.0 0 -10899396 true "" "plot sucker-welfare-per-tick"
"cheater" 1.0 0 -7500403 true "" "plot cheater-welfare-per-tick"
"grudger" 1.0 0 -955883 true "" "plot grudger-welfare-per-tick"
"token" 1.0 0 -13345367 true "" "plot token-welfare-per-tick"

TEXTBOX
129
322
279
340
0 to remove strategy
9
0.0
1

TEXTBOX
121
355
271
373
0 to remove strategy
9
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
<experiments>
  <experiment name="sucker-p100-c2b5-e1%" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1111"/>
    <metric>sucker-welfare-1000</metric>
    <enumeratedValueSet variable="cheater">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sucker">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="token%">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grudger-memory-cap">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolve?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="replacement-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grudger">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="token">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="cheater-p100-c2b5-e1%" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1111"/>
    <metric>cheater-welfare-1000</metric>
    <enumeratedValueSet variable="cheater">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sucker">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="token%">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grudger-memory-cap">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolve?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="replacement-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grudger">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="token">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="grudger-p100-c2b5-e1%" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1111"/>
    <metric>grudger-welfare-1000</metric>
    <enumeratedValueSet variable="cheater">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sucker">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="token%">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grudger-memory-cap">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolve?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="replacement-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grudger">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="token">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="token-p100-c2b5-e1%" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1111"/>
    <metric>token-welfare-1000</metric>
    <enumeratedValueSet variable="cheater">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sucker">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="token%">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grudger-memory-cap">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolve?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="replacement-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grudger">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="token">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="complete-p100-c2b5-e1%" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <exitCondition>( not sucker ) and ( not cheater ) and ( grudger-memory-cap = 0 ) and ( token% = 0 )</exitCondition>
    <metric>sucker-welfare</metric>
    <metric>cheater-welfare</metric>
    <metric>grudger-welfare</metric>
    <metric>token-welfare</metric>
    <metric>total-welfare</metric>
    <metric>count turtles with [ my-strategy = "sucker" ]</metric>
    <metric>count turtles with [ my-strategy = "cheater" ]</metric>
    <metric>count turtles with [ my-strategy = "grudger" ]</metric>
    <metric>count turtles with [ my-strategy = "token" ]</metric>
    <enumeratedValueSet variable="cheater">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sucker">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grudger-memory-cap">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="token%">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="complete-p100-c2b5-e1% streamlined" repetitions="25" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <exitCondition>( not sucker ) and ( not cheater ) and ( grudger-memory-cap = 0 ) and ( token% = 0 )</exitCondition>
    <metric>sucker-welfare</metric>
    <metric>cheater-welfare</metric>
    <metric>grudger-welfare</metric>
    <metric>token-welfare</metric>
    <metric>total-welfare</metric>
    <metric>count turtles with [ my-strategy = "sucker" ]</metric>
    <metric>count turtles with [ my-strategy = "cheater" ]</metric>
    <metric>count turtles with [ my-strategy = "grudger" ]</metric>
    <metric>count turtles with [ my-strategy = "token" ]</metric>
    <enumeratedValueSet variable="cheater">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sucker">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grudger-memory-cap">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="token%">
      <value value="0"/>
      <value value="20"/>
      <value value="40"/>
      <value value="60"/>
      <value value="80"/>
      <value value="100"/>
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
