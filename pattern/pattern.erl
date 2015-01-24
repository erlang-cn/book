-module(pattern).
-compile({no_auto_import, [apply/2]}).

-export([test/0]).

new_env() ->
    {_, Env} =
        func:eval_list(
          [[label, 'not',
            [lambda, [x],
             ['cond',
              [x,             [quote, false]],
              [[quote, true], [quote, true]]]
            ]
           ],

           [label, 'and',
            [lambda, [x,y],
             ['cond',
              [x,
               ['cond',
                [y,             [quote, true]],
                [[quote, true], [quote, false]]]
              ],
              [[quote, true], [quote, false]]]
            ]
           ],

           [label, subst,
            [lambda, [k,s],
             ['cond',
              [[eq, s, [quote, []]],
               [quote, none]],
              [[eq, k, [car, [car, s]]],
               [cons, [quote, ok], [cdr, [car, s]]]],
              [[quote, true],
               [subst, k, [cdr, s]]]]
            ]
           ],

           [label, member,
            [lambda, [x, l],
             ['cond',
              [[eq, l, [quote, []]],
               [quote, false]],
              [[eq, x, [car, l]],
               [quote, true]],
              [[quote, true],
               [member, x, [cdr, l]]]]
            ]
           ],

           [label, last,
            [lambda, [x],
             ['cond',
              [['not', [atom, x]],
               ['cond',
                [[eq, [cdr, x], [quote, []]],
                 [car, x]],
                [[quote, true],
                 [last, [cdr, x]]]
               ]]
             ]
            ]
           ],

           [label, equal,
            [lambda, [a,b],
             ['cond',
              [[eq, a, b],
               [quote, true]],
              [['and', ['not', [atom, a]], ['not', [atom, b]]],
               ['and',
                [equal, [car, a], [car, b]],
                [equal, [cdr, a], [cdr, b]]]
              ],
              [[quote, true],
               [quote, false]]]
            ]
           ],

           [label, make_pattern,
            [lambda, [expr, vars],
             ['cond',
              [[atom, expr],
               ['cond',
                [[eq, expr, [quote, '_']],
                 [quote, '_']],
                [[member, expr, vars],
                 [cons, [quote, var], expr]],
                [[quote, true],
                 [cons, [quote, atom], expr]]]
              ],
              [[eq, expr, [quote, []]],
               [cons, [quote, atom], [quote, []]]
              ],
              [[quote, true],
               [cons,
                [quote, cons],
                [cons,
                 [make_pattern, [car, expr], vars],
                 [make_pattern, [cdr, expr], vars]
                ]
               ]
              ]
             ]
            ]
           ],

           [label, match,
            [lambda, [value, pattern, table],
             ['cond',
              [[eq, table, [quote, false]],
               [quote, false]],
              [[eq, pattern, [quote, '_']],
               table],
              [[eq, [car, pattern], [quote, atom]],
               ['cond',
                [[eq, value, [cdr, pattern]],
                 table],
                [[quote, true],
                 [quote, false]]]
              ],
              [[eq, [car, pattern], [quote, var]],
               ['cond',
                [[eq, [subst, [cdr, pattern], table], [quote, none]],
                 [cons, [cons, [cdr, pattern], value], table]],
                [[quote, true],
                 ['cond',
                  [[equal, value, [cdr, [subst, [cdr, pattern], table]]],
                   table],
                  [[quote, true],
                   [quote, false]]
                 ]
                ]
               ]
              ],
              [[eq, [car, pattern], [quote, cons]],
               ['cond',
                [[atom, value],
                 [quote, false]],
                [[quote, true],
                 [match,
                  [cdr, value],
                  [cdr, [cdr, pattern]],
                  [match, [car, value], [car, [cdr, pattern]], table]]
                ]
               ]
              ]
             ]
            ]
           ],

           [label, apply_pattern,
            [lambda, [pattern, table],
             ['cond',
              [[eq, [car, pattern], [quote, atom]],
               [cdr, pattern]],
              [[eq, [car, pattern], [quote, var]],
               [cdr, [subst, [cdr, pattern], table]]],
              [[eq, [car, pattern], [quote, cons]],
               [cons,
                [apply_pattern, [car, [cdr, pattern]], table],
                [apply_pattern, [cdr, [cdr, pattern]], table]]
              ]
             ]
            ]
           ],

           [label, make_list,
            [lambda, [exprs],
             ['cond',
              [[eq, exprs, [quote, []]],
               [quote, [quote, []]]],
              [[quote, true],
               [cons,
                [quote, cons],
                [cons,
                 [car, exprs],
                 [cons,
                  [make_list, [cdr, exprs]],
                  [quote, []]
                 ]
                ]
               ]
              ]
             ]
            ]
           ],

           [label, list,
            [macro, [exprs],
             [make_list, exprs]
            ]
           ],

           [label, transform_expr,
            [lambda, [expr],
             ['cond',
              [[eq, [car, expr], [quote, p]],
               [list,
                [[quote, apply_pattern],
                 [list,
                  [[quote,make_pattern],
                   [list, [[quote, quote], [car, [cdr, expr]]]],
                   [quote, vars]]],
                 [quote, table]]]
              ],

              [[eq, [car, expr], [quote, call]],
               [cons,
                [car, [cdr, expr]],
                [transform_args, [cdr, [cdr, expr]]]
               ]
              ],

              [[eq, [car, expr], [quote, match]],
               [transform_expr,
                [list,
                 [[quote, 'case'],
                  [car, [cdr, [cdr, expr]]],
                  [list,
                   [[car, [cdr, expr]],
                    [quote, []]
                   ]
                  ]
                 ]
                ]
               ]
              ],

              [[eq, [car, expr], [quote, 'case']],
               [list,
                [[quote, last],
                 [list,
                  [[quote, list],
                   [list,
                    [[list,
                      [[quote, label],
                       [quote, expr],
                       [transform_expr, [car, [cdr, expr]]]]],
                     [cons, [quote, 'cond'],
                      [transform_clauses, [cdr, [cdr, expr]]]]
                    ]]
                  ]
                 ]
                ]]
              ]
             ]
            ]
           ],

           [label, transform_args,
            [lambda, [exprs],
             ['cond',
              [[eq, exprs, [quote, []]],
               [quote, []]],
              [[quote, true],
               [cons,
                [transform_expr, [car, exprs]],
                [transform_args, [cdr, exprs]]
               ]
              ]
             ]
            ]
           ],

           [label, transform_exprs,
            [lambda, [exprs],
             ['cond',
              [[eq, exprs, [quote, []]],
               [quote, [quote, []]]],
              [[quote, true],
               [list,
                [[quote, cons],
                 [transform_expr, [car, exprs]],
                 [transform_exprs, [cdr, exprs]]]]
              ]
             ]
            ]
           ],

           [label, transform_clause,
            [lambda, [clause],
             [apply_pattern,
              [make_pattern,
               [quote,
                [['not',
                  [eq,
                   [quote, false],
                   [label, table1,
                    [match, expr,
                     [make_pattern, [quote, 'P'], vars],
                     table]]
                  ]],
                 [last, [cons, [label, table, table1], 'E']]
                ]
               ],
               [quote, ['P', 'E']]],
              [list,
               [[cons, [quote, 'P'], [car, clause]],
                [cons, [quote, 'E'], [transform_exprs, [car, [cdr, clause]]]]]]
             ]
            ]
           ],

           [label, transform_clauses,
            [lambda, [clauses],
             ['cond',
              [[eq, clauses, [quote, []]],
               [quote, []]],
              [[quote, true],
               [cons,
                [transform_clause, [car, clauses]],
                [transform_clauses, [cdr, clauses]]
               ]
              ]
             ]
            ]
           ],

           [label, append,
            [lambda, [l1, l2],
             ['cond',
              [[eq, l1, [quote, []]],
               l2
              ],
              [['not', [atom, l1]],
               [cons,
                [car, l1],
                [append, [cdr, l1], l2]
               ]
              ]
             ]
            ]
           ],


           [label, zip,
            [lambda, [l1, l2],
             ['cond',
              [['and',
                [eq, l1, [quote, []]],
                [eq, l2, [quote, []]]
               ],
               [quote, []]
              ],

              [['and',
                ['not', [atom, l1]],
                ['not', [atom, l2]]],
               [cons,
                [cons, [car, l1], [car, l2]],
                [zip, [cdr, l1], [cdr, l2]]
               ]
              ]
              
             ]
            ]
           ],


           [label, transform_fn,
            [lambda, [args, vars, exprs],
             [list,
              [[quote, lambda],
               args,
               [list,
                [[quote, last],
                 [list,
                  [[quote, cons],
                   [list,
                    [[quote, label],
                     [quote, vars],
                     [list,[[quote, quote], [append, args, vars]]]]
                   ],
                   [list,
                    [[quote, cons],
                     [list,
                      [[quote, label],
                       [quote, table],
                       [list,
                        [[quote, match],
                         [list, [[quote, list], args]],
                         [list,
                          [[quote, make_pattern],
                           [list, [[quote, quote], args]],
                           [quote, vars]]],
                         [quote, [quote, []]]
                        ]]
                      ]],
                     [transform_exprs, exprs]
                    ]
                   ]
                  ]
                 ]
                ]
               ]
              ]]
            ]
           ],

           [label, fn,
            [macro, [args, vars, exprs],
             [transform_fn, args, vars, exprs]]
           ],

           [label, new_env,
            [lambda, [],
             [quote,
              [
               [quote|[fn|quote]],
               [atom|[fn|atom]],
               [eq|[fn|eq]],
               [car|[fn|car]],
               [cdr|[fn|cdr]],
               [cons|[fn|cons]],
               ['cond'|[fn|'cond']],
               [lambda|[fn|lambda]],
               [label|[fn|label]]
              ]
             ]]
           ],

           [label, apply,
            [fn, ['Expr', 'Env'], ['H','T','Fun','Env1','Value'],
             [['case', [p, ['Expr'|'Env']],
               [[['H'|'T']|'Env'],
                [[match, ['Fun'|'Env1'], [call, apply, [p, 'H'], [p, 'Env']]],
                 [call, call, [p, 'Fun'], [p, 'T'], [p, 'Env1']]]],
               [['Expr'|'Env'],
                [[match, [ok|'Value'], [call, subst, [p, 'Expr'], [p, 'Env']]],
                 [p, ['Value'|'Env']]]]
              ]]
            ]
           ],

           [label, eval_list,
            [fn, ['Expr', 'Env'], ['H','T','VH','VT','Env1','Env2'],
             [['case', [p, 'Expr'],
               [[],
                [[p, [[]|'Env']]]
               ],
               [['H'|'T'],
                [[match, ['VH'|'Env1'],
                  [call, apply, [p, 'H'], [p, 'Env']]],
                 [match, ['VT'|'Env2'],
                  [call, eval_list, [p, 'T'], [p, 'Env1']]
                 ],
                 [p, [['VH'|'VT']|'Env2']]
                ]
               ]
              ]
             ]
            ]
           ],

           [label, call,
            [fn, ['Fn', 'Expr', 'Env'],
             ['H','T','P','E','R','X','Y','X1','Y1','Env1','Args','Args1','V'],
             [['case', [p, ['Fn', 'Expr']],
               [[[fn|quote], ['X']],
                [[p, [[data|'X']|'Env']]]
               ],
               [[[fn|label], ['X', 'Y']],
                [[match, ['Y1'|'Env1'],
                  [call, apply, [p,'Y'], [p,'Env']]],
                 [p, ['Y1'|[['X'|'Y1']|'Env1']]]]
               ],
               [[[fn|car], ['X']],
                [[match, [[data|['H'|'_']]|'Env1'],
                  [call, apply, [p,'X'], [p,'Env']]],
                 [p, [[data|'H']|'Env1']]
                ]
               ],
               [[[fn|cdr], ['X']],
                [[match, [[data|['_'|'T']]|'Env1'],
                  [call, apply, [p,'X'], [p,'Env']]],
                 [p, [[data|'T']|'Env1']]
                ]
               ],
               [[[fn|cons], ['X','Y']],
                [[match, [[[data|'X1'],[data|'Y1']]|'Env1'],
                  [call, eval_list, [p,['X','Y']], [p,'Env']]],
                 [p, [[data|['X1'|'Y1']]|'Env1']]
                ]
               ],
               [[[fn|'cond'], [['P','E']|'T']],
                [[match, [[data|'R']|'Env1'],
                  [call, apply, [p, 'P'], [p, 'Env']]],
                 ['case', [p,'R'],
                  [false, [[call, apply, [p, ['cond'|'T']], [p, 'Env1']]]],
                  [true, [[call, apply, [p, 'E'], [p, 'Env1']]]]
                 ]
                ]
               ],
               [[[fn|atom], ['X']],
                [[match, [[data|'X1']|'Env1'],
                  [call, apply, [p,'X'], [p,'Env']]],
                 [match, 'R',
                  [call, atom, [p, 'X1']]],
                 [p, [[data|'R']|'Env1']]
                ]
               ],
               [[[fn|eq], ['X','Y']],
                [[match, [[[data|'X1'],[data|'Y1']]|'Env1'],
                  [call, eval_list, [p,['X','Y']], [p,'Env']]],
                 [match, 'R',
                  [call, eq, [p, 'X1'], [p, 'Y1']]],
                 [p, [[data|'R']|'Env1']]
                ]
               ],
               [[[fn|lambda], ['P','E']],
                [[p, [[lambda|['P'|'E']]|'Env']]]
               ],
               [[[lambda|['P'|'E']], 'Args'],
                [[match, ['Args1'|'Env1'],
                  [call, eval_list, [p,'Args'], [p,'Env']]],
                 [match, ['V'|'_'],
                  [call, apply, [p,'E'],
                   [call, append, [call, zip, [p, 'P'], [p, 'Args1']],
                    [p,'Env1']
                   ]
                  ]
                 ],
                 [p, ['V'|'Env1']]]
               ]
              ]
             ]
            ]
           ]

          ], func:new_env()),
    Env.


apply(Expr, Env) ->
    {{data, [Data|_]}, _} = func:apply([apply, [quote, Expr], [new_env]], Env),
    Data.

eval_list(Exprs, Env) ->
    {{data, [Data|_]}, _} =
         func:apply([eval_list, [quote, Exprs], [new_env]], Env),
     Data.

%% erl_equiv
%% change all apply
%% make_list/list as func exercise
%% missing tests

test(subst, Env) ->
    {{data,[ok|c]}, _} =
        func:apply([subst, [quote, a], [quote, [[b|d],[a|c]]]], Env),
    {{data,none}, _} =
        func:apply([subst, [quote, a], [quote, [[b|d]]]], Env),
    {{data,[ok|c]}, _} =
        func:apply([subst, [quote, a], [quote, [[a|c],[b|d],[a|e]]]], Env);
test(make_pattern,  Env) ->
    {{data,'_'}, _} =
        func:apply([make_pattern, [quote, '_'], [quote, []]], Env),
    {{data,[atom|a]}, _} =
        func:apply([make_pattern, [quote, a], [quote, []]], Env),
    {{data,[var|a]}, _} =
        func:apply([make_pattern, [quote, a], [quote, [a]]], Env),
    {{data,[cons|[[var|a]|[atom|b]]]}, _} =
        func:apply([make_pattern, [quote, [a|b]], [quote, [a]]], Env),
    {{data,[cons|[[atom|a]|[cons|[[atom|b]|[atom|[]]]]]]}, _} =
        func:apply([make_pattern, [quote, [a, b]], [quote, []]], Env);
test(match, Env) ->
    {{data, []}, _} =
        func:apply(
          [match, [quote, a], [quote, [atom|a]], [quote, []]],
          Env),
    {{data, [[a|a]]}, _} =
        func:apply(
          [match, [quote, a], [quote, [var|a]], [quote, []]],
          Env),
    {{data, [[a|a]]}, _} =
        func:apply(
          [match, [quote, a], [quote, [var|a]], [quote, [[a|a]]]],
          Env),
    {{data, false}, _} =
        func:apply(
          [match, [quote, b], [quote, [var|a]], [quote, [[a|a]]]],
          Env),
    {{data, []}, _} =
        func:apply(
          [match, [quote, a], [quote, '_'], [quote, []]],
          Env),
    {{data, []}, _} =
        func:apply(
          [match, [quote, [a|b]], [quote, [cons|[ [atom|a] | [atom|b] ]]],
           [quote, []]],
          Env);
test(apply_pattern, Env) ->
    {{data, a}, _} =
        func:apply(
          [apply_pattern, [quote, [atom|a]], [quote, []]],
          Env),
    {{data, b}, _} =
        func:apply(
          [apply_pattern, [quote, [var|a]], [quote, [[a|b]]]],
          Env),
    {{data, [a|b]}, _} =
        func:apply(
          [apply_pattern, [quote, [cons|[[atom|a]|[atom|b]]]], [quote, []]],
          Env);
test(make_list, Env) ->
    {{data, [cons, a, [cons, b, [cons, c, [quote, []]]]]}, _} =
        func:apply(
          [make_list, [quote, [a,b,c]]],
          Env),
    {{data, [a,b,c]}, _} =
        func:apply(
          [list, [[quote, a], [quote, b], [quote, c]]],
          Env);
test(transform, Env) ->
    {{data, [apply_pattern,[make_pattern,[quote,a],vars],table]}, _} =
        func:apply(
          [transform_expr, [quote, [p, a]]],
          Env),
    {{data, [a, [apply_pattern, [make_pattern,[quote,a],vars], table]]}, _} =
        func:apply(
          [transform_expr, [quote, [call, a, [p, a]]]],
          Env),
    {{data,
      [last,
       [list,
        [[label,expr,
          [apply_pattern,
           [make_pattern,[quote,a],vars],
           table]],
         ['cond',
          [['not',
            [eq,
             [quote,false],
             [label,table1,
              [match,expr,
               [make_pattern,[quote,a],vars],
               table]]]],
           [last,
            [cons,[label,table,table1],[quote,[]]]]]]]]]}, _} =
        func:apply(
          [transform_expr, [quote, [match, a, [p, a]]]],
          Env),
    {{data,
      [last,
       [list,
        [[label,expr,
          [apply_pattern,
           [make_pattern,[quote,a],vars],
           table]],
         ['cond',
          [['not',
            [eq, [quote,false],
             [label,table1,
              [match,expr,
               [make_pattern,[quote,a],vars],
               table]]]],
           [last,
            [cons,
             [label,table,table1],
             [cons,
              [apply_pattern,
               [make_pattern,[quote,a],vars],
               table],
              [quote,[]]]]]]]]]]}, _} =
        func:apply(
          [transform_expr, [quote, ['case', [p,a], [a,[[p,a]]]]]],
          Env),
    {{data,
      [lambda,
       ['A'],
       [last,
        [cons,
         [label,vars,[quote,['A']]],
         [cons,
          [label,table,
           [match,
            [list,['A']],
           [make_pattern, [quote,['A']], vars],
            [quote,[]]]],
          [cons,
           [apply_pattern,
            [make_pattern,[quote,'A'],vars],
           table],
           [quote,[]]]]]]]}, _} =
        func:apply(
          [transform_fn, [quote, ['A']], [quote, []], [quote, [[p, 'A']]]],
          Env);
test(quote, Env) ->
    [data|a] = apply([quote, a], Env),
    [data|[a,b,c]] = apply([quote, [a,b,c]], Env);
test(label, Env) ->
    [[data|a], [data|a]] =
        eval_list([[label, x, [quote, a]], x], Env),
    [[data|b], [data|b]] =
        eval_list([[label, x, [quote, b]], x], Env),
    [[data|a], [data|b], [data|b]] =
        eval_list(
          [[label, x, [quote, a]],
           [label, x, [quote, b]],
           x], Env);
test(list, Env) ->
    [data|a] = apply([car, [quote, [a]]], Env),
    [data|a] = apply([car, [quote, [a,b]]], Env),
    [data|[]] = apply([cdr, [quote, [a]]], Env),
    [data|[b]] = apply([cdr, [quote, [a,b]]], Env),
    [data|[a]] = apply([cons, [quote, a], [quote, []]], Env),
    [data|[a,b]] = apply([cons, [quote, a], [quote, [b]]], Env);
test('cond', Env) ->
    [data|a] =
        apply(['cond',
               [[quote, true],  [quote, a]],
               [[quote, false], [quote, b]],
               [[quote, false], [quote, c]]
              ], Env),
    [data|b] =
        apply(['cond',
               [[quote, false], [quote, a]],
               [[quote, true],  [quote, b]],
               [[quote, false], [quote, c]]
              ], Env),
    [data|c] =
        apply(['cond',
               [[quote, false], [quote, a]],
               [[quote, false], [quote, b]],
               [[quote, true],  [quote, c]]
              ], Env);
test(atom, Env) ->
    [data|true] =
        apply([atom, [quote, a]], Env),
    [data|false] =
        apply([atom, [quote, [a,b,c]]], Env);
test(eq, Env) ->
    [data|true] =
        apply([eq, [quote, []], [quote, []]], Env),
    [data|true] =
        apply([eq, [quote, a], [quote, a]], Env),
    [data|false] =
        apply([eq, [quote, []], [quote, a]], Env),
    [data|false] =
        apply([eq, [quote, a], [quote, []]], Env),
    [data|false] =
        apply([eq, [quote, [a]], [quote, [b]]], Env),
    [data|false] =
        apply([eq, [quote, [a,b,c]], [quote, [a,b,c]]], Env);
test(lambda, Env) ->
    [data|a] =
        apply([[lambda, [x], x],
               [quote, a]], Env),
    [data|a] =
        apply([[lambda, [x], [car, x]],
               [quote, [a,b,c]]], Env),
    [data|a] =
        apply([[lambda, [x], [car, [car, x]]],
               [quote, [[a]]]], Env),
    [data|true] =
        apply([[lambda, [x, y], [eq, x, y]],
               [quote, a],
               [quote, a]], Env),
    [data|a] =
        apply([[lambda, [x, x], x],
               [quote, a],
               [quote, b]], Env).


test() ->
    Env = new_env(),
    test(subst, Env),
    test(make_pattern, Env),
    test(match, Env),
    test(apply_pattern, Env),
    test(make_list, Env),
    test(transform, Env),
    test(quote, Env),
    test(label, Env),
    test(list, Env),
    test('cond', Env),
    test(atom, Env),
    test(eq, Env),
    test(lambda, Env),
    ok.
