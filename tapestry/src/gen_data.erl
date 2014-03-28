%%------------------------------------------------------------------------------
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%-----------------------------------------------------------------------------
%%
%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox Inc
%% @doc Random data generator for Tapestry. 

-module(gen_data).


-compile([export_all]).



roll_dice(Number,Sides)->
    roll_dice(Number,Sides,0).
roll_dice(0,_Sides,Acc)->
    Acc;
roll_dice(Number,Sides,Acc) ->
    NewAcc = Acc + random:uniform(Sides),
    roll_dice(Number-1,Sides,NewAcc).





links(Id1,Id2,Number)->
    links(Id1,Id2,Number,[]).
links(_Id1,_Id2,0,Acc)->
    Acc;
links({F1,Args1} = Id1, {F2,Args2} = Id2,Number,Acc) ->
    NewId1 = apply(F1,Args1),
    NewId2 = apply(F2,Args2),
    NewAcc = [{NewId1,NewId2}|Acc],
    links(Id1,Id2,Number-1,NewAcc).

			    
make(Number)->
    make(Number,Number, []).
make(_MaxNumber,0,Acc)->
    Acc;
make(MaxNumber,Number,Acc) ->
    NumElems = random:uniform(Number),
    NumSides = random:uniform(MaxNumber),
    NumDice = random:uniform(MaxNumber div NumSides),
    GenFun = {fun(X,Y)->roll_dice(X,Y) end,[NumDice,NumSides]},
    NewElems = links(GenFun,GenFun,NumElems),
    NewAcc = Acc ++ NewElems,
    make(MaxNumber,Number-NumElems,NewAcc).

make2(Elements,Sizes,Groups)->
    make2(Elements,Sizes,Groups,[]).
make2(_E,_S,0,Acc)->
    Acc;
make2(E,S,G,Acc) ->
    NumElems = E,
    NumSides = S,
    NumDice = random:uniform(E),
    GenFun = {fun(X,Y)->roll_dice(X,Y) end,[NumDice,NumSides]},
    NewElems = links(GenFun,GenFun,NumElems),
    NewAcc = Acc ++ NewElems,
    make2(E,S,G-1,NewAcc).

    
test(Elements,Sizes,Groups)->
    L = make2(Elements,Sizes,Groups),
    nci:compute(L).


test_run(Num,Inc)->
    test_run(Num,Inc,Inc).
test_run(Num,_Inc,Acc) when Acc > Num ->
    ok;
test_run(Num,Inc,Acc)->
    NCI = test(Acc),
    io:format("Elments: ~p, Log(Elements): ~p, NCI: ~p~n",[Acc,math:log(Acc),NCI]),
    test_run(Num,Inc,Acc*Inc).
    

test(Num)->
    L = make(Num),
    NCI = nci:compute(L),
    io:format("Pid = ~p, Elments: ~p, Log(Elements): ~p, NCI: ~p~n",[self(),Num,math:log(Num),NCI]).

test_c(Num)->
    L = make(Num),
    G = nci_c:compute(L).

test_dups()->
    L = [{6,2},{2,2},{5,6},{2,2},{7,7},{8,10},{8,10},{10,8},{9,3},{2,9},{2,9},{2,9}],
    nci:compute(L).

test_star()->
    L = [{1,2},{1,3},{1,4},{1,5},{2,3},{2,4},{2,5},{3,4},{3,5},{4,5}],
    nci:compute(L).

    
    

			     
	   

    
   







