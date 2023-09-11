val+ test2__35 : T__528 =
  bind/cc+ a23__38 : T__37 ->
    cmd- : T__39 val =
      match this.thunk().ret(a__73__41 : T__42) ->
        cmd- : T__44 val =
          bind/cc- a24__47 : T__46 ->
            cmd- : T__48 val =
              match this.thunk().ret(a65__50 : T__51) ->
                int{1}().ret(a65__50)
            stk =
              this.bind- (x__60 : T__59) ->
                cmd- : T__61 val =
                  bind/cc- c__52__64 : T__63 ->
                    x__60.thunk().bind+ (s__53__71 : T__70) ->
                      cmd- : T__72 val =
                        match this.call(s__38__74 : T__75).ret(a__37__76 : T__77) ->
                          cmd- : T__80 val =
                            match this.call(x__35__82 : T__83).ret(b__36__84 : T__85) ->
                              tuple(s__38__74, x__35__82).bind+ (tup__39__96 : T__95) ->
                                cmd- : T__97 val =
                                  match this.thunk().ret(a__48__99 : T__100) ->
                                    inj{1,2}(tup__39__96).ret(a__48__99)
                                stk =
                                  this.ret(b__36__84)
                        stk =
                          this.bind- (y__40__118 : T__117) ->
                            cmd- : T__119 val =
                              match this.thunk().ret(a__46__121 : T__122) ->
                                cmd- : T__124 val =
                                  match this.thunk().ret(a__33__126 : T__127) ->
                                    x__60.thunk().bind+ (xx__34__135 : T__134) ->
                                      cmd- : T__136 val =
                                        match this.call(ex__28__138 : T__139).ret(a__29__140 : T__141) ->
                                          ex__28__138.match
                                          | inj{0,2}(e__30__148 : T__147) ->
                                            cmd- : T__151 val =
                                              match this.thunk().ret(a__32__153 : T__154) ->
                                                inj{0,2}(e__30__148).ret(a__32__153)
                                            stk =
                                              this.ret(a__29__140)
                                          | inj{1,2}(xx__31__167 : T__166) ->
                                            cmd- : T__170 val =
                                              match this.call(x__25__172 : T__173).ret(a__26__174 : T__175) ->
                                                cmd- : T__178 val =
                                                  match this.thunk().ret(a__27__180 : T__181) ->
                                                    inj{1,2}(inj{1,2}(x__25__172)).ret(a__27__180)
                                                stk =
                                                  this.ret(a__26__174)
                                            stk =
                                              this.call(xx__31__167).ret(a__29__140)
                                          end
                                      stk =
                                        this.call(xx__34__135).thunk().ret(a__33__126)
                              stk =
                                this.thunk().bind+ (xx__47__229 : T__228) ->
                                  cmd- : T__230 val =
                                    match this.call(ex__41__232 : T__233).ret(a__42__234 : T__235) ->
                                      ex__41__232.match
                                      | inj{0,2}(e__43__242 : T__241) ->
                                        cmd- : T__245 val =
                                          match this.thunk().ret(a__45__247 : T__248) ->
                                            inj{0,2}(e__43__242).ret(a__45__247)
                                        stk =
                                          this.ret(a__42__234)
                                      | inj{1,2}(xx__44__261 : T__260) -> y__40__118.call(xx__44__261).ret(a__42__234)
                                      end
                                  stk =
                                    this.call(xx__47__229).thunk().ret(a__46__121)
                          stk =
                            this.ret(a__37__76)
                stk =
                  this.call(s__53__71).bind- (z__56__304 : T__303) ->
                    cmd- : T__305 val =
                      match this.thunk().ret(a__63__307 : T__308) ->
                        z__56__304.thunk().bind+ (xx__64__316 : T__315) ->
                          cmd- : T__317 val =
                            match this.call(ex__58__319 : T__320).ret(a__59__321 : T__322) ->
                              ex__58__319.match
                              | inj{0,2}(e__60__329 : T__328) ->
                                cmd- : T__332 val =
                                  match this.thunk().ret(a__62__334 : T__335) ->
                                    inj{0,2}(e__60__329).ret(a__62__334)
                                stk =
                                  this.ret(a__59__321)
                              | inj{1,2}(xx__61__348 : T__347) ->
                                cmd- : T__351 val =
                                  match this.call(tup__49__353 : T__354).ret(a__50__355 : T__356) ->
                                    cmd+ : T__359 val =
                                      bind/cc+ b__51__362 : T__361 ->
                                        tup__49__353.match tuple(s__53__367 : T__366, x__54__369 : T__368) -> x__54__369.ret(b__51__362)
                                    stk =
                                      this.bind+ (y__55__382 : T__381) ->
                                        cmd- : T__383 val =
                                          match this.thunk().ret(a__57__385 : T__386) ->
                                            inj{1,2}(y__55__382).ret(a__57__385)
                                        stk =
                                          this.ret(a__50__355)stk =this.call(xx__61__348).ret(a__59__321)
                            end
                        stk =
                          this.call(xx__64__316).thunk().ret(a__63__307)
                  stk =
                    this.ret(c__52__64)
      stk =
        this.ret(a24__47)stk =
  this.thunk().bind+ (xx__74__446 : T__445) ->
    cmd- : T__447 val =
      match this.call(sum__66__449 : T__450).ret(a__67__451 : T__452) ->
        cmd+ : T__455 val =
          bind/cc+ b__68__458 : T__457 ->
            sum__66__449.match
            | inj{0,2}(x__69__463 : T__462) -> x__69__463.ret(b__68__458)
            | inj{1,2}(y__70__471 : T__470) -> y__70__471.ret(b__68__458)
            end
        stk =
          this.bind+ (z__71__484 : T__483) ->
            cmd- : T__485 val =
              match this.thunk().ret(a__72__487 : T__488) ->
                z__71__484.ret(a__72__487)
            stk =
              this.ret(a__67__451)
  stk =
    this.call(xx__74__446).thunk().ret(a__73__41)stk =
  this.thunk().bind+ (x__21__519 : T__518) ->
    x__21__519.ret(a23__38)
cmd+ anon__531 ret a22__530 : T__529 =
  int{0}().ret(a22__530)
