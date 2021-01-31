(* ::Package:: *)

BeginPackage["SingleNetworks`"];

snetworkdatasingle::usage = "description.";
snetworkdatabinned::usage = "description.";
snetworkgraph::usage = "description.";


Begin["`Private`"];


Clear[networkdatabinned]
networkdatabinned[feature_,step_]:=Module[{rawaim,aim,campaign,seri,min,max,binningamount,binning},
rawaim=Symbol["data"][[All,feature]];
aim=DeleteCases[rawaim,"NA"];
campaign=Delete[Symbol["data"][[All,118]],Position[rawaim,"NA"]];
seri=Delete[Symbol["data"][[All,1]],Position[rawaim,"NA"]];
min=Floor[Min[Sort[DeleteDuplicates[aim]]],0.1];
max=Ceiling[Max[Sort[DeleteDuplicates[aim]]]]+step;
binningamount=Length[DeleteCases[BinLists[aim,{min,max,step}],{}]];
binning=Table[Catch[Do[If[IntervalMemberQ[Interval[i],aim[[j]]]==True,Throw[i]],
{i,Partition[Range[min,max,step],2,1]}]],{j,Length[aim]}];
aim=DeleteCases[binning,Null];
{aim,campaign,seri}]


End[];
EndPackage[];
