; Bigoni et al 2020: "producer has good: can consume or transfer(help). consumer values good more"
; early/test version, near-undocumented

globals [ active-strategies total-welfare avg-welfare sucker-welfare cheater-welfare grudger-welfare token-welfare RLM-welfare
  total-welfare-per-tick
  avg-welfare-per-tick
  sucker-welfare-per-tick
  cheater-welfare-per-tick
  grudger-welfare-per-tick
  token-welfare-per-tick
  RLM-welfare-per-tick
  total-welfare-1000
  avg-welfare-1000
  sucker-welfare-1000
  cheater-welfare-1000
  grudger-welfare-1000
  token-welfare-1000
  RLM-welfare-1000
  total-welfare-10000
]

turtles-own [ has-token? my-strategy fitness blacklist my-balance ]

to setup
  clear-all

  if ( not sucker ) and ( not cheater ) and ( not grudger ) and ( not token ) and ( not RLM )[ error "No strategy selected. Execution stopped." stop ]

  set active-strategies [ ]
  if sucker [ set active-strategies lput "sucker" active-strategies ]
  if cheater [ set active-strategies lput "cheater" active-strategies ]
  if grudger [ set active-strategies lput "grudger" active-strategies ]
  if token [ set active-strategies lput "token" active-strategies ]
  if RLM [ set active-strategies lput "RLM" active-strategies ]

  crt population [ right random 180 fd 13
    set fitness 0
    set-me-up
  ]
  reset-ticks
end

to set-me-up
    set my-strategy one-of active-strategies
    if my-strategy = "grudger" [ set blacklist [] ]
    if my-strategy = "token" [
      ifelse random 100 < token-share [ set has-token? true ][ set has-token? false ] ; to review: tokens disappearing during evolution
      shape-me
    ]
    if ( my-strategy = "RLM" ) [
      set my-balance one-of [ 0 1 ]; personal index, Bigoni 2020
      size-me

    ;the lines below are only relevant to simplify BehaviorSpace runs:
    if not grudger [ set grudger-memory 0 ]
    if not token [ set tok-memory 0 ]

    ]
    color-me
end

to color-me
  if my-strategy = "sucker" [ set color green ]
  if my-strategy = "cheater" [ set color grey ]
  if my-strategy = "grudger" [ set color orange ]
  if my-strategy = "token" [ set color blue ]
  if my-strategy = "RLM" [ set color violet ]
end

to shape-me
    ifelse has-token? [ set shape "star" ][ set shape "default" ]
end

to size-me
    set size log ( my-balance + 2 ) 2
end

to go
  ask links [ die ]

  ask turtles [
    find-partner
    interact
    if my-strategy = "token" [ shape-me ]
    if my-strategy = "RLM" [ size-me ]
  ]

  set sucker-welfare sum [fitness] of turtles with [ my-strategy = "sucker" ]
  set cheater-welfare sum [fitness] of turtles with [ my-strategy = "cheater" ]
  set grudger-welfare sum [fitness] of turtles with [ my-strategy = "grudger" ]
  set token-welfare sum [fitness] of turtles with [ my-strategy = "token" ]
  set RLM-welfare sum [fitness] of turtles with [ my-strategy = "RLM" ]

  set total-welfare sum [fitness] of turtles
  set avg-welfare total-welfare / population

  if evolve? [ evolve-all ]

  tick
  report-welfare-per-tick
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
    ifelse not member? one-of link-neighbors blacklist [ groom ] [ not-groom ] ; netlogo peculiarity? grudger memory seems to not operate without specifying "one-of" link-neighbors
    if length blacklist > grudger-memory [
      set blacklist remove-item ( length blacklist - 1 ) blacklist ; cap length of memory string by killing last/oldest item
    ]
  ]

    if ( my-strategy = "token" ) and has-token? = false [ ; Dawkins 1985, Kocherlakota 1998: "money is a token of delayed reciprocal altruism". Underlying model for BigoniCC?
      ifelse [has-token?] of one-of link-neighbors = true [ ; "conditional hastoken" - agent only trades in case it doesn't already have a token, and counterpart does. somewhat arbitrary.
      groom ; Netlogo peculiarity? if "one-of" is removed from line above, tokens slowly disappear

      ; Token swap mechanism:
      ask link-neighbors [ set has-token? false ]
      set has-token? true
    ]
    [ not-groom ]
  ]

  if ( my-strategy = "RLM" ) [ ; Bigoni MMM 2020 - supplementary material p. 10. Reputation balances supposed to be less successful than tokens (!= model results)
    ifelse [my-balance] of one-of link-neighbors > 0 [
      groom

      ; RLM swap mechanism: increase balance of benefit provider, decrease balance of receiver
      set my-balance my-balance + 1
      ask link-neighbors [ set my-balance my-balance - 1 ]
    ]
    [ not-groom ]
  ]

end

to groom
  set fitness fitness - cost
  ask link-neighbors [ ; confirm peculiar behavior "one-of" x "item 0"
    set fitness fitness + benefit
  ]
end

to not-groom
  ask link-neighbors with [ my-strategy = "grudger" ] [
    if not member? myself blacklist [
      set blacklist fput myself blacklist
    ]
  ]
end

to evolve-all
  let n replacement-rate * population
  ask min-n-of n turtles [ fitness ] [ die ]
  create-turtles n [ right random 180 fd random 13
    set fitness avg-welfare ; new turtles are born with average fitness. alternatively, reset and recalculate fitness?
    set-me-up
  ]

end

to report-welfare-per-tick
  set total-welfare-per-tick total-welfare / ticks
  set avg-welfare-per-tick avg-welfare / ticks
  set sucker-welfare-per-tick sucker-welfare / ticks
  set cheater-welfare-per-tick cheater-welfare / ticks
  set grudger-welfare-per-tick grudger-welfare / ticks
  set token-welfare-per-tick token-welfare / ticks
  set RLM-welfare-per-tick RLM-welfare / ticks
end

to report1000
  set total-welfare-1000 total-welfare
  set avg-welfare-1000 avg-welfare
  set sucker-welfare-1000 sucker-welfare
  set cheater-welfare-1000 cheater-welfare
  set grudger-welfare-1000 grudger-welfare
  set token-welfare-1000 token-welfare
  set RLM-welfare-1000 RLM-welfare
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
25
213
115
246
sucker
sucker
0
1
-1000

SWITCH
25
356
115
389
token
token
0
1
-1000

SWITCH
114
213
204
246
cheater
cheater
0
1
-1000

SWITCH
24
253
114
286
grudger
grudger
0
1
-1000

TEXTBOX
58
192
208
222
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
"RLM" 1.0 0 -8630108 true "" "plot RLM-welfare"
"total" 1.0 0 -16777216 true "" "plot total-welfare "

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
"RLM" 1.0 0 -8630108 true "" "plot count turtles with [ my-strategy = \"RLM\" ]"

MONITOR
830
492
915
537
NIL
total-welfare
1
1
11

MONITOR
919
492
999
537
NIL
avg-welfare
1
1
11

INPUTBOX
117
105
213
165
replacement-rate
0.01
1
0
Number

MONITOR
753
492
825
537
NIL
count turtles
17
1
11

SLIDER
25
391
197
424
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
24
121
114
154
evolve?
evolve?
0
1
-1000

MONITOR
1008
492
1104
537
circulating tokens
count turtles with [has-token? = true]
0
1
11

TEXTBOX
1234
29
1421
57
(TOTAL) WELFARE REPORTERS
11
0.0
1

TEXTBOX
1331
54
1481
72
at 1000 ticks
11
0.0
1

MONITOR
1402
118
1471
163
total
total-welfare-1000
1
1
11

MONITOR
1327
71
1393
116
sucker
sucker-welfare-1000
1
1
11

MONITOR
1328
118
1394
163
cheater
cheater-welfare-1000
1
1
11

MONITOR
1329
167
1395
212
grudger
grudger-welfare-1000
1
1
11

MONITOR
1328
215
1395
260
token
token-welfare-1000
1
1
11

MONITOR
1402
165
1472
210
average
avg-welfare-1000
1
1
11

SLIDER
24
289
207
322
grudger-memory
grudger-memory
1
population
100.0
1
1
NIL
HORIZONTAL

MONITOR
1200
69
1250
114
sucker
sucker-welfare-per-tick
1
1
11

MONITOR
1201
120
1251
165
cheater
cheater-welfare-per-tick
1
1
11

MONITOR
1201
172
1251
217
grudger
grudger-welfare-per-tick
1
1
11

MONITOR
1202
220
1252
265
token
token-welfare-per-tick
1
1
11

TEXTBOX
1206
52
1356
70
per tick
11
0.0
1

MONITOR
1255
120
1305
165
total
total-welfare-per-tick
1
1
11

MONITOR
1255
171
1305
216
average
avg-welfare-per-tick
1
1
11

PLOT
698
331
1182
479
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
"RLM" 1.0 0 -8630108 true "" "plot RLM-welfare-per-tick"

SWITCH
24
440
114
473
RLM
RLM
0
1
-1000

MONITOR
1201
266
1251
311
RLM
RLM-welfare-per-tick
1
1
11

MONITOR
1328
264
1396
309
RLM
RLM-welfare-1000
1
1
11

PLOT
1200
332
1468
481
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
"RLM" 1.0 0 -8630108 true "" "plot sum [my-balance] of turtles with [ my-strategy = \"RLM\" ]"

TEXTBOX
120
442
270
468
\"Bigoni-memory\":\nreputation-ledger
10
0.0
1

SWITCH
24
488
114
521
KLRM
KLRM
1
1
-1000

TEXTBOX
119
480
243
532
\"Kocherlakota-memory\" - (not implemented - complete history of partner's transactions)
10
0.0
1

TEXTBOX
137
256
287
282
Trivers 1971\n(plus memory cap)
10
0.0
1

TEXTBOX
124
359
237
413
Dawkins-Koch-BigoniCC: \"token-reciprocity\"
10
0.0
1

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
  <experiment name="complete-p100-c2b5-e1%" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 300</exitCondition>
    <metric>total-welfare</metric>
    <metric>avg-welfare</metric>
    <metric>sucker-welfare</metric>
    <metric>cheater-welfare</metric>
    <metric>grudger-welfare</metric>
    <metric>token-welfare</metric>
    <metric>RLM-welfare</metric>
    <metric>count turtles with [ my-strategy = "sucker" ]</metric>
    <metric>count turtles with [ my-strategy = "cheater" ]</metric>
    <metric>count turtles with [ my-strategy = "grudger" ]</metric>
    <metric>count turtles with [ my-strategy = "token" ]</metric>
    <metric>count turtles with [ my-strategy = "RLM" ]</metric>
    <metric>sum [my-balance] of turtles</metric>
    <metric>count turtles with [ has-token? = TRUE ]</metric>
    <runMetricsCondition>ticks mod 20 = 0</runMetricsCondition>
    <enumeratedValueSet variable="cheater">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sucker">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="token-share">
      <value value="25"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="benefit">
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost">
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grudger-memory">
      <value value="0"/>
      <value value="5"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolve?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RLM">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="replacement-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grudger">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="token">
      <value value="true"/>
      <value value="false"/>
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
