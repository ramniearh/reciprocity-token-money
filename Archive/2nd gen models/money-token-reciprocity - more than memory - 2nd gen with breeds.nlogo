; Bigoni et al 2020: "producer has good: can consume or transfer(help). consumer values good more"
; early/test version, near-undocumented

;; add assortativity
; ask links [
;  ifelse (([fitness] of end1 >= [previous-fitness] of end 1) and
;         ([fitness] of end2 > [previous-fitness] of end 2))
;         or
;         (([fitness] of end1 > [previous-fitness] of end 1) and
;         ([fitness] of end2 >= [previous-fitness] of end 2))
;         [if random-float 1 > prob-maintaining [ die ]] ; if true
;         [ die ] ; if false
; ]
; ; still dyadic exchanges, some are persistent with prob = prob-maintaining
; ; prob-maintaining parameter from the GUI
; ; previous-fitness is an agent specific attribute
;
; to groom
;  set previous-fitness fitness ;store current fitness before updating it
;  set fitness fitness - cost
;  ask link-neighbors [ ; confirm peculiar behavior "one-of" x "item 0"
;    set previous-fitness fitness
;    set fitness fitness + benefit
;  ]
;end
;
;#Scenarios (w/out assortativity)
;- Baseline
;- Memory
;- Tokens
;- Memory & Tokens
;
;Re-do this with Assortativity in place
;#See what happens


globals [ active-strategies total-welfare avg-welfare sucker-welfare cheater-welfare grudger-welfare token-welfare ledger-welfare reputation-welfare
  total-welfare-1000
  avg-welfare-1000
  sucker-welfare-1000
  cheater-welfare-1000
  grudger-welfare-1000
  token-welfare-1000
  ledger-welfare-1000
  reputation-welfare-1000
  total-welfare-10000
  sucker-population
  cheater-population
  grudger-population
  token-population
  ledger-population
  reputation-population
]

turtles-own [ has-token? my-strategy fitness blacklist my-balance image-score normalized-fitness previous-fitness ]

to setup
  clear-all
  reset-ticks

  if ( not sucker ) and ( not cheater ) and ( not grudger ) and ( not token ) and ( not ledger ) and ( not reputation ) [ error "No strategy selected. Execution stopped." stop ]

  set-up-strategies

  crt population [ right random 180 fd 13
    set-me-up
  ]

end

to set-up-strategies
  set active-strategies [ ]
  if sucker [ set active-strategies lput "sucker" active-strategies ]
  if cheater [ set active-strategies lput "cheater" active-strategies ]
  if grudger [ set active-strategies lput "grudger" active-strategies ]
  if token [ set active-strategies lput "token" active-strategies ]
  if ledger [ set active-strategies lput "ledger" active-strategies ]
  if reputation [ set active-strategies lput "reputation" active-strategies ]
end

to set-me-up
  set-my-fitness
    set my-strategy one-of active-strategies
    set blacklist []
    ifelse random 100 < token-share [ set has-token? true ][ set has-token? false ] ; to review: tokens disappearing during evolution
    ifelse oversupply? [ set my-balance 5 ] [ set my-balance one-of [ 0 1 ] ]; personal index, Bigoni 2020 (REVIEW OVERSUPPLY)
    shape-me
    size-me
    color-me
end

to set-my-fitness
  ifelse ticks = 0 [ set fitness 0 ] [ set fitness avg-welfare ]

end

to color-me
  if my-strategy = "sucker" [ set color green ]
  if my-strategy = "cheater" [ set color grey ]
  if my-strategy = "grudger" [ set color orange ]
  if my-strategy = "token" [ set color blue ]
  if my-strategy = "ledger" [ set color violet ]
  if my-strategy = "reputation" [ set color yellow set pcolor image-score ]
end

to shape-me
    ifelse has-token? [ set shape "star" ][ set shape "default" ]
end

to size-me
    set size log ( my-balance + 2 ) 2
end

to go
  ask links [  ;; adding persistence/assortativity!
    ifelse (([fitness] of end1 >= [previous-fitness] of end1) and ([fitness] of end2 >= [previous-fitness] of end2)) ;if interaction was at least partially positive for both/either side
      [if random-float 1 > link-persistence-prob [die]] ;if true
      [die] ;if false

    ]

  ask turtles [
    find-partner ]

  ask turtles [
    interact
    if my-strategy = "token" [ shape-me ]
    if my-strategy = "ledger" [ size-me ]
  ]

  ;ask turtles [ set normalized-fitness fitness - avg-welfare ] ; normalize fitnesses:

  run-reporters

  if evolve? [ evolve-all ]

  tick
  if ticks = 1000 [ report1000 ]

end

to find-partner ; random match with a free agent, for the duration of the tick
  if ( not any? link-neighbors ) and ( any? other turtles with [ not any? link-neighbors ] ) [
    create-link-with one-of other turtles with [ not any? link-neighbors ]
  ]
end

to interact ; unlike BigoniCC, turtles interact twice per round, each time initiated by one member of the pair

    if my-strategy = "sucker" [ ; Trivers 1971
    groom
  ]

    if my-strategy = "cheater" [ ; Trivers 1971
    not-groom
  ]

    if my-strategy = "grudger" [ ; Confirm: equivalent to Trivers 1971 with added memory cap? "personalistic grim trigger"? to do: evaluate nice tit for tat?
    ifelse not member? one-of link-neighbors blacklist [ groom ] [ not-groom ]
    if length blacklist >= grudger-memory [
      set blacklist remove-item ( length blacklist - 1 ) blacklist ; cap length of memory string by killing last/oldest item
    ];show length blacklist
  ]

    if ( my-strategy = "token" ) [
      ifelse ( [has-token?] of one-of link-neighbors = true and has-token? = false ) [ ; "conditional hastoken" - agent only trades in case it doesn't already have a token, and counterpart does. somewhat arbitrary.
      groom ; Netlogo peculiarity? if "one-of" is removed from line above, tokens slowly disappear
      ask link-neighbors [ set has-token? false ]
      set has-token? true
    ] [ not-groom ]
  ]

  if ( my-strategy = "ledger" ) [
    ifelse leaky? [ groom ] [
      ifelse [my-balance] of one-of link-neighbors > 0 [
      groom
      ask link-neighbors [ set my-balance my-balance - 1 ]
      set my-balance my-balance + 1
    ] [ not-groom ]
    ]
  ]


  if ( my-strategy = "reputation" ) [ ; Nowak 1998 K-minimum image scoring "If a player is chosen as a donor and decides to cooperate then his (or her) image score increases by one unit; if the donor does not cooperate then it decreases by one unit. "
    let perceived-partner-score 0
    if random 100 < visibility [ set perceived-partner-score [image-score] of one-of link-neighbors ] ;if visibility is low (agents observe few other agents's image scores), it's likely that they consider their partner as having score 0

    ifelse perceived-partner-score > min-K-score [ groom ] [ not-groom ]
  ]

end

to groom
  set fitness fitness - cost
  ask link-neighbors [ ; confirm peculiar behavior "one-of" x "item 0"
    set fitness fitness + benefit
  ]
  set image-score image-score + 1
end

to not-groom
  ask link-neighbors with [ my-strategy = "grudger" ] [
    if not member? myself blacklist [
      set blacklist fput myself blacklist
    ]
  ]
  set image-score image-score - 1
end

to evolve-all
  let n replacement-rate * population
  ask min-n-of n turtles [ fitness ] [ die ]
  create-turtles n [ right random 180 fd random 13
    set-me-up
  ]

end


to run-reporters
  set sucker-population count turtles with [my-strategy = "sucker"]
  set cheater-population count turtles with [my-strategy = "cheater"]
  set grudger-population count turtles with [my-strategy = "grudger"]
  set token-population count turtles with [my-strategy = "token"]
  set ledger-population count turtles with [my-strategy = "ledger"]
  set reputation-population count turtles with [my-strategy = "reputation"]

  set sucker-welfare sum [fitness] of turtles with [ my-strategy = "sucker" ]
  set cheater-welfare sum [fitness] of turtles with [ my-strategy = "cheater" ]
  set grudger-welfare sum [fitness] of turtles with [ my-strategy = "grudger" ]
  set token-welfare sum [fitness] of turtles with [ my-strategy = "token" ]
  set ledger-welfare sum [fitness] of turtles with [ my-strategy = "ledger" ]
  set reputation-welfare sum [fitness] of turtles with [ my-strategy = "reputation" ]

  set total-welfare sum [fitness] of turtles
  set avg-welfare total-welfare / population

end


to report1000
  set total-welfare-1000 total-welfare
  set avg-welfare-1000 avg-welfare
  set sucker-welfare-1000 sucker-welfare
  set cheater-welfare-1000 cheater-welfare
  set grudger-welfare-1000 grudger-welfare
  set token-welfare-1000 token-welfare
  set ledger-welfare-1000 ledger-welfare
  set reputation-welfare-1000 reputation-welfare
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
26
27
96
87
population
100.0
1
0
Number

BUTTON
24
173
87
206
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
95
172
158
205
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
164
172
227
205
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
34
236
124
269
sucker
sucker
0
1
-1000

SWITCH
243
491
333
524
token
token
0
1
-1000

SWITCH
123
236
213
269
cheater
cheater
0
1
-1000

SWITCH
31
299
121
332
grudger
grudger
0
1
-1000

TEXTBOX
67
215
217
245
--BASE STRATEGIES--
11
0.0
1

INPUTBOX
102
27
152
87
cost
1.0
1
0
Number

INPUTBOX
158
28
211
88
benefit
5.0
1
0
Number

TEXTBOX
71
10
221
28
--PARAMETERS--
11
0.0
1

PLOT
698
177
1181
327
total welfare of strategies
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
"cheater" 1.0 0 -7500403 true "" "plot cheater-welfare "
"grudger" 1.0 0 -955883 true "" "plot grudger-welfare "
"token" 1.0 0 -13345367 true "" "plot token-welfare "
"RLM" 1.0 0 -8630108 true "" "plot ledger-welfare"
"reputation" 1.0 0 -1184463 true "" "plot reputation-welfare "
"average" 1.0 0 -16777216 true "" "plot avg-welfare"

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
"\"suckers\"" 1.0 0 -10899396 true "" "plot sucker-population"
"\"cheaters\"" 1.0 0 -7500403 true "" "plot cheater-population"
"\"grudgers\"" 1.0 0 -955883 true "" "plot grudger-population\n"
"\"token\"" 1.0 0 -13345367 true "" "plot token-population"
"\"RLM\"" 1.0 0 -8630108 true "" "plot ledger-population"
"\"reputation\"" 1.0 0 -1184463 true "" "plot reputation-population"

MONITOR
700
486
785
531
NIL
total-welfare
1
1
11

MONITOR
789
486
869
531
NIL
avg-welfare
1
1
11

INPUTBOX
118
93
214
153
replacement-rate
0.01
1
0
Number

SLIDER
243
526
415
559
token-share
token-share
0
100
50.0
1
1
NIL
HORIZONTAL

SWITCH
25
109
115
142
evolve?
evolve?
0
1
-1000

MONITOR
874
486
970
531
circulating tokens
count turtles with [has-token? = true]
0
1
11

TEXTBOX
1192
17
1379
45
(TOTAL) WELFARE REPORTERS
11
0.0
1

TEXTBOX
1214
31
1364
49
at 1000 ticks
11
0.0
1

MONITOR
1285
95
1354
140
total
total-welfare-1000
1
1
11

MONITOR
1210
48
1276
93
sucker
sucker-welfare-1000
1
1
11

MONITOR
1211
95
1277
140
cheater
cheater-welfare-1000
1
1
11

MONITOR
1212
144
1278
189
grudger
grudger-welfare-1000
1
1
11

MONITOR
1211
192
1278
237
token
token-welfare-1000
1
1
11

MONITOR
1285
142
1355
187
average
avg-welfare-1000
1
1
11

SLIDER
31
335
214
368
grudger-memory
grudger-memory
1
population
100.0
1
1
NIL
HORIZONTAL

SWITCH
447
492
537
525
ledger
ledger
0
1
-1000

MONITOR
1210
290
1281
335
reputation
reputation-welfare-1000
1
1
11

PLOT
906
331
1174
480
RLM "personal index" - total balance
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
"total" 1.0 0 -16777216 true "" "plot sum [my-balance] of turtles"
"suckers" 1.0 0 -10899396 true "" "plot sum [my-balance] of turtles with [ my-strategy = \"sucker\" ]"
"cheaters" 1.0 0 -7500403 true "" "plot sum [my-balance] of turtles with [ my-strategy = \"cheater\" ]"
"grudgers" 1.0 0 -955883 true "" "plot sum [my-balance] of turtles with [ my-strategy = \"grudger\" ]"
"token" 1.0 0 -13345367 true "" "plot sum [my-balance] of turtles with [ my-strategy = \"token\" ]"
"RLM" 1.0 0 -8630108 true "" "plot sum [my-balance] of turtles with [ my-strategy = \"ledger\" ]"

TEXTBOX
546
496
670
522
BigoniCC:\nreputation-ledger
10
0.0
1

SWITCH
1232
536
1322
569
KLRM
KLRM
1
1
-1000

TEXTBOX
1327
528
1451
580
\"Kocherlakota-memory\" - (not implemented - complete history of partner's transactions)
10
0.0
1

TEXTBOX
125
302
275
328
Trivers 1971\n(plus memory cap)
10
0.0
1

TEXTBOX
342
494
455
548
BigoniCC:\n\"token-reciprocity\"
10
0.0
1

SWITCH
446
526
536
559
leaky?
leaky?
1
1
-1000

TEXTBOX
543
529
693
555
\"Bigoni-memory (2020):\nunbounded negative reciprocity
10
0.0
1

SWITCH
32
377
126
410
reputation
reputation
1
1
-1000

TEXTBOX
134
382
284
409
Nowak 1998\n(simplified)
10
0.0
1

PLOT
698
331
900
481
image scores histogram
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
"default" 1.0 0 -16777216 true "" "plot sum [image-score] of turtles"

SLIDER
31
413
123
446
min-K-score
min-K-score
- population
population
-1.0
1
1
NIL
HORIZONTAL

SWITCH
445
564
561
597
oversupply?
oversupply?
0
1
-1000

MONITOR
1211
243
1278
288
RLM
ledger-welfare-1000
17
1
11

PLOT
1518
409
1718
559
per-capita welfare of strategies
NIL
NIL
0.0
1000.0
-5.0
5.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot avg-welfare"
"sucker" 1.0 0 -10899396 true "" "if sucker-population > 0 [ plot sucker-welfare / sucker-population ]"
"pen-2" 1.0 0 -7500403 true "" "if cheater-population > 0 [ plot cheater-welfare / cheater-population ]"
"pen-3" 1.0 0 -955883 true "" "if grudger-population > 0 [ plot grudger-welfare / grudger-population ]"
"pen-4" 1.0 0 -13345367 true "" "if token-population > 0 [ plot token-welfare / token-population ]"
"pen-5" 1.0 0 -8630108 true "" "if RLM-population > 0 [ plot RLM-welfare / RLM-population ]"
"pen-6" 1.0 0 -1184463 true "" "if reputation-population > 0 [ plot reputation-welfare / reputation-population ]"

SLIDER
126
414
218
447
visibility
visibility
0
100
96.0
1
1
NIL
HORIZONTAL

MONITOR
974
485
1100
530
average blacklist length
(sum [length blacklist ] of turtles with [my-strategy = \"grudger\"]) / ( count turtles with [my-strategy = \"grudger\"] )
1
1
11

TEXTBOX
54
280
204
298
--RECIPROCITY STRATEGIES--
10
0.0
1

TEXTBOX
362
467
512
485
--MONETARY STRATEGIES--\n
10
0.0
1

TEXTBOX
1233
508
1383
526
not implemented:
10
0.0
1

SLIDER
477
405
649
438
link-persistence-prob
link-persistence-prob
0
1
1.0
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This is a (simplistic) implementation of an evolutionary helping game, where each agent has the chance to provide a benefit to a counterpart (at a smaller personal cost), and the least succesful strategies are progressively eliminated. The "Sucker" strategy (the name comes from Dawkin's "The Selfish Gene") does this at every interaction - and a society of suckers does pretty well, as the overall level of benefits received is high. Suppose, however, that there is a "Cheater" strategy, which doesn't reciprocate the help: they can receive benefits from Suckers without paying a personal cost. If cheaters are mixed into the population, Suckers are soon extinct - and the entire community is worse off. "Grudgers", in turn, can remember when someone has cheated them - they can only be fooled once. If the benefit/cost ratio from cooperation is high (and if Grudger memory is large enough to remember a large share of the Cheaters),this strategy can win out and invade the population, bringing aggregate levels of welfare back up.

The above is well-known from cooperation studies in evolutionary theory (more mechanisms, including reputation when dealing with strangers, are surveyed in Nowak's "Evolving Cooperation"). This model extends this logic to incorporate some suggestions from the "Money is Memory" literature in economics (Kocherlakota 1998, Bigoni et al 2020). 

Now imagine a large enough group of agents, one for which Grudger memory is not a powerful enough tool (think of the Dunbar number and our societies made up of millions of strangers). The model suggests that a "Token" strategy - one which only agrees to help on the condition that its counterpart transfers an intrisically useless object - provides a large-scale solution that is comparable to small-scale Grudger memory. Further, a "quantified tokens strategy" ("RLM") folds in the mechanics of reputation, ledgers and memory to be even more effective, and brings us close to classical notions of Social Accounting in economics - provided this mechanism is considered legitimate or enforceable.


## HOW IT WORKS

Early draft description:

Setup: Define-se a população de agentes (por padrão 100), o custo ao agente da cooperação (padrão 2), o benefício à contraparte (5), e a taxa de evolução (1%). Definem-se as estratégias.

Go: são criados pares aleatórios entre todos os agentes. Todos os agentes são chamados a realizar uma ação unilateral por turno. As ações são:- para agentes Sucker, sempre pagar custo e prover benefício à contraparte- para agentes Cheater, não fazer nada- agentes Grudger têm uma memória em que gravam outros agentes com quem tiveram interações negativas. Ao agir, caso a contraparte não esteja nessa lista, o agente Grudger provê benefício e paga custo. Caso a contraparte esteja na lista, o Grudger não faz nada. Além disso, quando a contraparte interage com o Grudger, caso ela não tenha feito nada (não provendo benefício ao grudger), ela entra na memória do Grudger. O limite máximo da memória do Grudger é configurável no modelo.- agentes Token têm uma variável True/False que representa a posse ou não de um objeto sem valor intrínseco. Ao agir, se o agente Token não tiver o objeto (false) e a contraparte tiver (true), ele paga o custo, provê o benefício, e recebe esse objeto da contraparte (invertendo true e false). A distribuição inicial de tokens é configurável no modelo.- agentes RLM têm uma variável numérica "balanço de ajudas". Ao agir, se a contraparte tiver um balanço maior do que zero, o agente RLM paga o custo, provê o benefício, e tem seu balanço aumentado em uma unidade. A contraparte tem o balanço diminuído em uma unidade. A distribuição inicial de balanços é configurável no modelo. Depois de completas as ações, uma porcentagem dos agentes, aqueles com as menores fitness, são substituídos por novos agentes aleatórios..

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
  <experiment name="persistence-assortativity" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 1000</exitCondition>
    <metric>total-welfare</metric>
    <metric>avg-welfare</metric>
    <metric>sucker-welfare</metric>
    <metric>cheater-welfare</metric>
    <metric>grudger-welfare</metric>
    <metric>ledger-welfare</metric>
    <metric>count turtles with [ my-strategy = "sucker" ]</metric>
    <metric>count turtles with [ my-strategy = "cheater" ]</metric>
    <metric>count turtles with [ my-strategy = "grudger" ]</metric>
    <metric>count turtles with [ my-strategy = "ledger" ]</metric>
    <metric>sum [my-balance] of turtles</metric>
    <runMetricsCondition>ticks mod 25 = 0</runMetricsCondition>
    <enumeratedValueSet variable="cheater">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sucker">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ledger">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grudger">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link-persistence-prob">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="1"/>
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
