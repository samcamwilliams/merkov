-module(mult).
-export([findB/4, getCorrespondingMultiplier/1, simEndowment/3]).

start() -> 
    B = 3-math:sqrt(5/63), 
    Prods = randproducts(B, 2, [], 10000),
    io:fwrite("List = ~w.~n", [Prods]),
    io:fwrite("Average Product ~w.~n", [avg(Prods)]).
   
randproducts(_Bound, _RVCount, List, 0) ->
    List;
randproducts(Bound, RVCount, List, N) ->
    NewList = lists:append(List, [randproduct(Bound, RVCount)]),
    randproducts(Bound, RVCount, NewList, N-1).

randproduct(Bound, RVCount) -> 
    multivar(Bound, 1, RVCount).
multivar(_Bound, X0, 0) ->
    X0;
multivar(Bound, X0, I) ->
    X1 = X0*(rand:uniform_real()*Bound),
    multivar(Bound, X1, I-1).

avg(List) ->
    lists:sum(List) / length(List).

cdf(Bound, RVCount, X) ->
    N = 10000,
    Distribution = randproducts(Bound, RVCount, [], N),
    LessCount = lists:filter(fun(Y)->Y<X end, Distribution),
    io:fwrite("mean of all values where product is below ~w = ~w.~n", [X, avg(LessCount)]),
    length(LessCount)/length(Distribution).

loguniformmean(Volatility, B) ->
    Distribution = randlogs(10000, [], Volatility, B),
    avg(Distribution). 

randlogs(0, List, _Volatility, _B) ->
    List;
randlogs(N, List, Volatility, B) ->
    NewList = lists:append(List, [math:log(1-Volatility+Volatility*B*rand:uniform_real())]),
    randlogs(N-1, NewList, Volatility, B).

mean(Bound, RVCount) ->
    N = 100000,
    Distribution = randproducts(Bound, RVCount, [], N),
    avg(Distribution).

calcmean(B, V) ->
    B*math:log(B)-B-V*math:log(V)+V.

findB(Up, Down, V, 0) ->
    Mid = (Up + Down)/2,
    Mid; 
findB(Up, Down, V, N) ->
    Mid = (Up + Down)/2,
    Mean = calcmean(Mid, V),
    if 
        (Mean>0) -> 
            findB(Mid, Down, V, N-1);
        (Mean<0) -> 
            findB(Up, Mid, V, N-1);
        (Mean==0) -> 
            Mid
    end.
        
getCorrespondingMultiplier(Volatility) ->
    Upper = findB(4,0,(1-Volatility),100),
    Multiplier = (Upper-(1-Volatility))/Volatility,
    Multiplier. 

simEndowment(_Volatility, Value, 0) ->
    Value;
simEndowment(Volatility, Value, Years) ->
    Multiplier = getCorrespondingMultiplier(Volatility),  
    NewValue = Value - (Value * Volatility) + rand:uniform_real()*Multiplier*(Value*Volatility),
    simEndowment(Volatility, NewValue, Years-1). 
