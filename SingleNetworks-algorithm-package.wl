(* ::Package:: *)

BeginPackage["SingleNetworks`"];

snetworkdatasingle::usage = "description.";
snetworkdatabinnedintimewindows::usage = "description.";
snetworkgraph::usage = "description.";


Begin["`Private`"];


Clear[snetworkdatabinnedintimewindows]
snetworkdatabinnedintimewindows[feature_,step_,datadimension_]:=Module[{rawaim,aim,campaign,seri,
min,max,binningamount,binning},
rawaim=Table[Symbol["data"][[i]][[All,feature]],{i,Range@datadimension}];
aim=Table[DeleteCases[i,"NA"],{i,rawaim}];
campaign=Table[Delete[Symbol["data"][[i[[1]]]][[All,2]],Position[i[[2]],"NA"]],
{i,MapThread[{#1,#2}&,{Range@datadimension,rawaim}]}];
seri=Table[Delete[Symbol["data"][[i[[1]]]][[All,1]],Position[i[[2]],"NA"]],
{i,MapThread[{#1,#2}&,{Range@datadimension,rawaim}]}];
min=Table[Floor[Min[Sort[DeleteDuplicates[i]]],0.1],{i,aim}];
max=Table[Ceiling[Max[Sort[DeleteDuplicates[i]]]]+step,{i,aim}];
binningamount=Table[Length[DeleteCases[BinLists[i[[1]],{i[[2]],i[[3]],step}],{}]],
{i,MapThread[{#1,#2,#3}&,{aim,min,max}]}];
binning=Table[Table[Catch[Do[If[IntervalMemberQ[Interval[i],(k[[1]])[[j]]]==True,Throw[i]],
{i,Partition[Range[k[[2]],k[[3]],step],2,1]}]],{j,Length[k[[1]]]}],{k,MapThread[{#1,#2,#3}&,
{aim,min,max}]}];
aim=Table[DeleteCases[i,Null],{i,binning}];
{aim,campaign,seri}]


Clear[snetworkgraph]
snetworkgraph[aim_,campaign_,vertexsize_,vertexlabelsize_,imagesize_,vertexcolor_]:=Module[{
binningmembers,aimbaskets,singlesupportvalues,pairs,pairsupportvalues,liftvalues,
allmatrixelements,likelypairs,binarymatrix,graph},
binningmembers=Sort[DeleteDuplicates[aim]];
aimbaskets=Table[DeleteDuplicates[i],{i,Values@GroupBy[Thread[{aim,campaign}],Last->First]}];
singlesupportvalues=Table[N[Count[Table[MemberQ[i,j],{i,aimbaskets}],
True]/Length[aimbaskets]],{j,binningmembers}];
pairs=Subsets[binningmembers,{2}];
pairsupportvalues=Table[N[Count[Table[SubsetQ[i,j],{i,aimbaskets}],True]/
Length[aimbaskets]],{j,pairs}];
liftvalues=pairsupportvalues/(DeleteCases[Flatten@UpperTriangularize[Table[singlesupportvalues
[[j]]*singlesupportvalues[[k]],{j,Length[binningmembers]},{k,Length[binningmembers]}],1],0.]);
allmatrixelements=Sort[Join[pairs,Reverse[pairs,2],Table[{i,i},{i,binningmembers}]]];
likelypairs=Extract[pairs,Position[liftvalues,x_/;x>1]];
binarymatrix=ArrayReshape[Table[If[j==True,1,0],{j,Table[MemberQ[likelypairs,i],
{i,allmatrixelements}]}],{Length@binningmembers,Length@binningmembers}];
graph=AdjacencyGraph[binarymatrix,{GraphLayout->Automatic,DirectedEdges->False,
EdgeShapeFunction->"Line",VertexSize->vertexsize,VertexStyle->vertexcolor,
VertexLabelStyle->Directive[Black,Italic,vertexlabelsize],VertexLabels->Flatten[MapThread[
{#1->Placed[#2,Center]}&,{Range[1,Dimensions[binarymatrix][[1]]],Table[StringRiffle[i,"\n"],
{i,binningmembers}]}]]},ImageSize->imagesize];{graph,Length@binningmembers}]


End[];
EndPackage[];
