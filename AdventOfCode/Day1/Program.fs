﻿open System

let input = """()()(()()()(()()((()((()))((()((((()()((((()))()((((())(((((((()(((((((((()(((())
                (()()(()((()()(()(())(()((((()((()()()((((())((((((()(()(((()())(()((((()))())((
                ))(()(()()))))))))((((((((((((()())()())())(())))(((()()()((((()(((()(()(()()(()
                (()()(()(((((((())(())(())())))((()())()((((()()((()))(((()()()())))(())))((((()
                )(((()())(())(()))(()((((()())))())((()(())(((()((((()((()(())())))((()))()()(()
                (()))))((((((((()())((((()()((((()(()())(((((()(()())()))())(((()))()(()(()(()((
                ((()(())(()))(((((()()(()()()(()(((())())(((()()(()()))(((()()(((())())(()(())()
                )()()(())()()()((()(((()(())((()()((())()))((()()))((()()())((((()(()()(()(((())
                )()(()))))((()(((()()()))(()(((())()(()((()())(()(()()(()())(())()(((()(()())()(
                (((()((()))))())()))((()()()()(())()())()()()((((()))))(()(((()()(((((((())()))(
                )((((()((())()(()())(())()))(()(()())(((((((())))(((()))())))))()))())((())(()()
                ((())()())()))))()((()()())(())((())((((()())())()()()(((()))())))()()))())(()()
                ()(()((((((()()))())()))()(((()(((())((((()()()(()))())()()))))())()))())((())()
                ())(((((())())((())())))(((())(((())(((((()(((((())(()(()())())(()(())(()))(()((
                ((()))())()))))())))((()(()))))())))(((((())()))())()))))()))))(((()))()))))((()
                ))((()((()(()(())()())))(()()()(())()))()((((())))))))(())(()((()()))(()))(()))(
                ()((()))))))()()((((()()))()())()))))))()()()))(()((())(()))((()()()())()(((()((
                ((())())))()((((()(()))))))())))()()())()))(()))))(()())()))))))((())))))))())()
                ))()((())())))(()((()))()))(())))))(()))()())()()))((()(()))()()()()))))())()())
                )())(())()()))()))((()))))()()(()())))))()()()))((((()))()))))(()(())))(()())))(
                (())())(()))()))))()())))()())()())))))))))()()))))())))((())((()))))())))(((()(
                ))))))))(()))()()))(()))()))))()())))))())((((()())))))))())))()()))))))))()))()
                ))))()))))))(())))))))))())))))))))))))))())())((())))))))))()))((())))())))))))
                )())()(()))))))())))))()()()())()(()()()(()())(()))()()()(()())))())())))()))))(
                ))))))))()()()()())(())())()())()))))(()()()()()))))()))())())))((()())()())))()
                ))()))))(()())))()))))))))(((()))()()))))))))))))))))))))(()))(()((()))())))())(
                ()))(()(()(())))))()(()))()))()()))))))))))))()((()())(())())()(())))))())()())(
                (()()))))(()()))))())()(())()))))))))))))))))))))()))(()(()())))))))()()((()))()
                ))))))((())))()))))))))((()))())()()))())()()))((()))())))))))))))(()())()))(())
                ((()(()()))(()())(())))()())(()(())()()))))()))()(()))))))(()))))))))))(()))()))
                )))))))())))))())))(())))))()))))(())())))))))))()(()))))()())))())(()))()()))))
                )))))))))())()()))))()))))))())))))()))))(())(()()()()((())()))())(()))((())()))
                ())())(())(()()))))()))(())()()((())(())))(())))()))())))))))))()(((((())())))((
                ))()))))(())))((()))()(((((((()))))()()))(())))))()(()))))(()()))()))())))))))((
                )())()))))))))())))(()))())()))(())()((())())()())())(()(()))))()))))))((()())((
                ))()()(()())))()()))(())(())(()))())))()))(()))()()))((((()))))()))((()()()))))(
                )))()))())))(()))()))))(())))()))())()(()))()())))())))))))())))())))()()))))))(
                ()))())())))()))()()())())))))))))))))())))()))(()()))))())))())()(())))()))))))
                )))))))))))()()())())))))()()()((()(()))()()(())()())()))()))))()()()))))))((())
                )))))))()(()(()((((((()()((()())))))))))))()))())))))((())())(()))())))())))))()
                )()()())(())))())))()())())(())))))))()()(())))()))())))())())())()))))))))()))(
                ()()()())())())))(())())))))))()()())()))))())))())()(())())))))))()())()))(()()
                (())())))()(()((()()((()()(((((())(()())()))(())()))(())))(())))))))()))()))((()
                ))()))()))))))))()))))))))((()()())(()))(((()))(())))()))((())(((())))()())))())
                ))))((())))))(())())((((((())())()(()))()(()((()())))((())()(()(()))))(())(()()(
                ))(())))())((()(((())())))(((()())())))())()(())())((((()()))))())((()))()()()()
                (())(((((((()()()((()))())(()())))(())())((((()()(()))))()((())))((())()))()(((()))())))()))((()(()))(())(()((((())((((()()(()()))(((())(()))))((((()(()))(())))))((()))(()))((()(((()(()))(()(()((()(())(()(()(()(()()((()))())(((())(()(()))))(()))()()))(())))(())()(((())(()))()((((()()))))())(()))))((())()((((()(((()))())())(((()))()())((())(())())(())()(())()(()()((((((()()))))()()(((()()))))()())()(((()(()))(()(()())(()(()))))(((((()(((())())))))(((((()((()()((())())((((((()(())(()()((()()()()()()()(()()))()(((()))()))(((((((())(((()((()())()((((())(((()(())))()((()(()()()((())((()())()))()))())))())((((((()))(()(()()()))(()((()(()(()))()((()(((()()()((())(((((())()(()))())())((()(())))(()(()())(())((())())())(((()()()(())))))())(()))))))()))))))())((()()()))((()((((((()))(((()((((()()()(((()))())()(()()(((()((()()()()())()()))()()()(()(())((()))))(()))())))))))()(()()(((((())()(()(((((()((()(()()())(()((((((((()((((((())()((((()()()((()((()((((((()))((())))))))())()))((()(()))()(()()(()((())((()()((((((((((((()())(()()()))((((()((((((())(()))())(()()((()()))()(((((((()((()()((((((()(((())))((())))((((((((()()(((((((())(((((()())(((())((())()((((()(((((((()(()(((()((((((()(((()(((((((((((()()((()()(()))((()()(((()(((())))((((())()(()(((())()(()(((())(((((((((((()))())))((((((())((()()((((()())())((((()()))((())(((((()(()()(()()()((())(()((()()((((()(((((()((()(()((((()())((((((()(((((()()(()(()((((())))(())(())(())((((()(()()((((()((((()()((()((((((())))(((((()))))()))(()((((((((()(((())())(((())))(()(()((())(((()((()()(((((()((()()(((())()(()))(((((((())(()(((((()))((()((()((()))(())())((((()((((())()(()))(((()(((((((((((((((())(((((((((()))(((()(()()()()((((((()((())()((((((((()(())(((((((((((()(()((())()((()()(()(()()((((()()((())(()((()()(()()((((()(((((((())))((((())(())()(((()()((()()((((()((()(((()((())(((()()()((((()((((()()(()(()((((((((())(()(((((())(()())(((((((()())()(()((((()((())(()()())((((()()(((()((((())(())(()()(((((((((()()))()(((())(()(()((((((())(()()())(()))()()(((()(((()((())(()(((((((()(()(()((()(((((()(()((()(()((((((()((((()()((((()(((()((())(()(()((()()((((()()(())()(())(((())(()((((((((()())(((((((((()(())()((((())))()))()()(((((()()((((((())(()()(((()(()(((((((()(()(((((((())(())((((()((()(())))((((()()())(()))((()())((((()(((((()(()(())(()(()()())(((((()(((((()((((()()((((((((()()))(()((((((())((((())()(()(((()()()(((()(()(())(())(((((()(())())((((())(())(()(((()(((((())((((())())((()(((((((()(((())(()(()))(((((((((()((()((()()(()((((())(((()((())((((())(()(((()(((()(()((((()(((())(()(((()(()()(()(()((()()(()())(())())((()(()(((()(((()(((()()(((((((((()(((((((((()()(((()(((()())((((()(()(((()()()((())((((((((((())(()(((()((((()())((((()((()))(((()()()(((((()(((((((())((()())(()((((())((((((((())(()((()((((((((((()()((()((()()))(((()())()())()(((()())()()(()(()(((((((())()))(())()))())()()((())()((()((((()((()((())(((((()((((((()(())))(()))())(((()))((()()(()(((()))((((())()(((()))))()(()(())()(((((())(()(()(())(())()((()()()((((()(())((()())(()(()))(()(()(()()(())()()(()((())()((()))))()))((()(()()()()((()())(()))())()(()(((((((((())())((()((()((((((())()((((())(((())((()(()()()((())(()((())(((()((((()()((()(()(((((())()))()((((((()))((())(((()()))(((())(())()))(((((((())(())())()(())(((((()))()((()))()(()()((()()()()()())((((((("""

let step = ref 0

for i in input do
    if i = '(' then incr step
    elif i = ')' then decr step


printfn "%d" !step

Console.ReadLine() |> ignore