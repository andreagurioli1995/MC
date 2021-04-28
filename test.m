(* ::Package:: *)

BeginPackage["mypck`"]


MyFun::usage="testOne"


CartesianMap::usage="secondOne"


Begin["Private`"]


MyFun[x_]:=Module[{y},Table[x+2+y,{y,1,5}]];


CartesianMap[func_,{y0_,x1_,dx_:Automatic},{y0_,y1_,dy_:Automatic}]:=Module[{x,y,coords,plotpoints,ndx=dx,ndy=dy}, plotpoints=PlotPoints/.Options[Plot];
]


End[]
