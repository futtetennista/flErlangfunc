-module(assignment1).
-export([area/1,perimeter/1,enclose/1,bitsR/1,bitsTR/1,tests/0]).

%% Read https://en.wikipedia.org/wiki/Triangle#Computing_the_sides_and_angles for inspiration about triangle representations. Investigations:
%% 1) No need for a base and a height but heavy use a trig funcs, which are very expensive:
%% {triangle,{Xa,Ya,A,Alpha},{Xb,Yb,B,Beta},{Xc,Yc,C,Gamma}}
%%
%% 2) Easier to calculate the area and no need to use trig funcs but has some duplicated data:
%% {triangle,{Xa,Ya,AB},{Xb,Yb,BC},{Xc,Yc,CA}, Base,Height}
%%
%% 3) Cheap enough to compute the area - sqrt is not as expesive as sin - without requiring data duplication. Moreover area re-uses perimeter which is also nice:
%% {triangle,{Xa,Ya},{Xb,Yb},{Xc,Yc},A,B,C}


tests() ->
    io:format("testArea:~w~ntestPerimeter:~w~ntestEnclose:~w~ntestBitsR:~w~ntestBitsTR:~w~n", [testArea(), testPerimeter(), testEnclose(), testBitsR(), testBitsTR()]).

testArea() ->
    area({triangle,{1,5},{3,6},{1,3},4,3,5}) == 6.0.
testPerimeter() ->
    perimeter({triangle,{1,1},{3,3},{1,3},2,3,3.6}) == 8.6.
testEnclose() ->
    TestCircle = enclose({circle,{2,2},1}) == {rectangle,{1,1},2,2},
    TestTriangle = enclose({triangle,{1,1},{3,1},{2,2},2,1.4142,1.4142}) == {rectangle,{1,1},1,2},
    TestRectangle = enclose({rectangle,{6,6},3,4}) == {rectangle,{6,6},3,4},
    TestCircle and TestRectangle and TestTriangle.
testBitsR() ->
    (bitsR(1111) == 6) and (bitsR(15) == 4) and (bitsR(0) == 0).

testBitsTR() ->
    (bitsTR(1111) == 6) and (bitsTR(15) == 4) and (bitsTR(0) == 0).


area({circle,{_X,_Y},Radius}) ->
    math:pi() * Radius * Radius;
area({rectangle,{_X,_Y},Heigth,Width}) ->
    Heigth * Width;
%% Using https://en.wikipedia.org/wiki/Heron%27s_formula
area({triangle,PointA,PointB,PointC,A,B,C}) ->
    Semiperimeter = perimeter({triangle,PointA,PointB,PointC,A,B,C}) / 2,
    math:sqrt(Semiperimeter * (Semiperimeter - A) * (Semiperimeter - B) * (Semiperimeter - C)).


perimeter({circle,{_X,_Y},Radius}) ->
    2 * math:pi() * Radius;
perimeter({rectangle,{_X,_Y},Heigth,Width}) ->
    2 * Heigth + 2 * Width;
perimeter({triangle,_,_,_,A,B,C}) ->
    A + B + C.

enclose({circle,{X,Y},Radius}) ->
    {rectangle,{X-Radius,Y-Radius},Radius*2,Radius*2};
enclose({rectangle,P,Heigth,Width}) ->
    {rectangle,P,Heigth,Width};
enclose({triangle,{Xa,Ya},{Xb,Yb},{Xc,Yc},_,_,_}) ->
    MinX = lists:min([Xa,Xb,Xc]),
    MinY = lists:min([Ya,Yb,Yc]),
    MaxX = lists:max([Xa,Xb,Xc]),
    MaxY = lists:max([Ya,Yb,Yc]),
    Heigth = MaxY - MinY,
    Width = MaxX - MinX,
    {rectangle,{MinX,MinY},Heigth,Width}.

bitsR(0) ->
    0;
bitsR(N) ->
    N rem 2 + bitsR(N div 2).

bitsTR(N) ->
    bits(N,0).

bits(0,Acc) ->
    Acc;
bits(N,Acc) ->
    bits(N div 2,Acc + N rem 2).
