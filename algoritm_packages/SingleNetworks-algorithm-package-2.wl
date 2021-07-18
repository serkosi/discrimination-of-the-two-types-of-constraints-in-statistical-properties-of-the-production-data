(* ::Package:: *)

BeginPackage["SingleNetworks`"];

snetworkdatasingleintimewindows::usage = "description.";
snetworkgraphsinglenodes::usage = "description.";
snetworkdatabinned::usage = "description.";
snetworkdatafxdbucket::usage = "description.";
snetworkdatabinnedintimewindows::usage = "description.";
snetworkdatafxdbucketintimewindows::usage = "description.";
snetworkgraph::usage = "description.";
randomizinggraphdegfxd::usage = "description.";
randomizinggraphmod::usage = "description.";
zscorefunctionfortwonullmodels::usage = "description.";


Begin["`Private`"];


Clear[snetworkdatasingleintimewindows]
snetworkdatasingleintimewindows[feature_,datadimension_]:=Module[{rawaim,pos,aim,campaign,seri},
rawaim=Table[Symbol["data"][[i]][[All,feature]],{i,Range@datadimension}];
pos=Table[Partition[Flatten@Table[Position[k,i],{i,{"NA",0}}],1],{k,rawaim}];
aim=Table[Delete[i[[1]],i[[2]]],{i,MapThread[{#1,#2}&,{rawaim,pos}]}];
campaign=Table[Delete[Symbol["data"][[i[[1]]]][[All,2]],i[[2]]],
{i,MapThread[{#1,#2}&,{Range@datadimension,pos}]}];
seri=Table[Delete[Symbol["data"][[i[[1]]]][[All,1]],i[[2]]],
{i,MapThread[{#1,#2}&,{Range@datadimension,pos}]}];
{aim,campaign,seri}]


Clear[snetworkgraphsinglenodes]
snetworkgraphsinglenodes[aim_,campaign_,vertexsize_,vertexlabelsize_,imagesize_,vertexcolor_]:=
Module[{binningmembers,aimbaskets,singlesupportvalues,pairs,pairsupportvalues,liftvalues,
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
{#1->Placed[#2,Center]}&,{Range[1,Dimensions[binarymatrix][[1]]],binningmembers}]]},
ImageSize->imagesize];{graph,Length@binningmembers}]


Clear[snetworkdatabinned]
snetworkdatabinned[feature_,step_,data_]:=Module[{rawaim,pos,aim,campaign,seri,min,max,
binningamount,binning},
rawaim=data[[All,feature]];
pos=Partition[Flatten@Table[Position[rawaim,i],{i,{"NA",0}}],1];
aim=Delete[rawaim,pos];
campaign=Delete[data[[All,2]],pos];
seri=Delete[data[[All,1]],pos];
min=Floor[Min[Sort[DeleteDuplicates[aim]]],0.1];
max=Ceiling[Max[Sort[DeleteDuplicates[aim]]]]+step;
binningamount=Length[DeleteCases[BinLists[aim,{min,max,step}],{}]];
binning=Table[Catch[Do[If[IntervalMemberQ[Interval[i],aim[[j]]]==True,Throw[i]],
{i,Partition[Range[min,max,step],2,1]}]],{j,Length[aim]}];
aim=DeleteCases[binning,Null];
{aim,campaign,seri}]


Clear[snetworkdatafxdbucket]
snetworkdatafxdbucket[feature_,nodenumber_,data_]:=Module[{rawaim,pos,aim,
campaign,seri,bucketsize,aimlabeled,aimpartitioned,bins,repetitivesreport,repetitives,
labelgeneration,binsrearranged,aimbinned},
rawaim=data[[All,feature]];
pos=Partition[Flatten@Table[Position[rawaim,i],{i,{"NA",0}}],1];
aim=Delete[rawaim,pos];
campaign=Delete[data[[All,2]],pos];
seri=Delete[data[[All,1]],pos];
bucketsize=Ceiling@(N@(Dimensions@aim)/nodenumber);
aimlabeled=Thread[Range@Length@aim->aim];
aimpartitioned=Partition[Normal@Sort@Association@aimlabeled,UpTo@bucketsize];
bins=Table[MinMax[i],{i,Values@aimpartitioned}];
repetitivesreport=DeleteCases[Tally@bins,x_/;x[[2]]==1];
repetitives=repetitivesreport[[All,1]];

labelgeneration[x_]:=Table[{repetitives[[x]][[1]]+i*repetitives[[x]][[1]]
/(2*10^(RealDigits@repetitives[[x]][[1]]+1)[[2]]),repetitives[[x]][[2]]+
i*repetitives[[x]][[2]]/(2*10^(RealDigits@repetitives[[x]][[2]]+1)[[2]])},
{i,Range@repetitivesreport[[x]][[2]]}];

binsrearranged=ReplacePart[bins,Flatten[Table[MapThread[#1->#2&,
{Flatten@Position[bins,repetitives[[g]]],labelgeneration[g]}],
{g,Range@Length@repetitives}],1]];

aimbinned=Values@Sort[Flatten[Table[aimpartitioned[[i]]/.
Dispatch@Table[Values@aimpartitioned[[i]][[j]]->binsrearranged[[i]],
{j,Length@aimpartitioned[[i]]}],{i,Length@aimpartitioned}],1],#1[[1]]<#2[[1]]&];
{aimbinned,campaign,seri,bucketsize}]


Clear[snetworkdatabinnedintimewindows]
snetworkdatabinnedintimewindows[data_,feature_,step_,datadimension_]:=Module[{rawaim,pos,aim,
campaign,seri,min,max,binningamount,binning},
rawaim=Table[data[[i]][[All,feature]],{i,Range@datadimension}];
pos=Table[Partition[Flatten@Table[Position[k,i],{i,{"NA",0}}],1],{k,rawaim}];
aim=Table[Delete[i[[1]],i[[2]]],{i,MapThread[{#1,#2}&,{rawaim,pos}]}];
campaign=Table[Delete[data[[i[[1]]]][[All,2]],i[[2]]],
{i,MapThread[{#1,#2}&,{Range@datadimension,pos}]}];
seri=Table[Delete[data[[i[[1]]]][[All,1]],i[[2]]],
{i,MapThread[{#1,#2}&,{Range@datadimension,pos}]}];
min=Table[Floor[Min[Sort[DeleteDuplicates[i]]],0.1],{i,aim}];
max=Table[Ceiling[Max[Sort[DeleteDuplicates[i]]]]+step,{i,aim}];
binningamount=Table[Length[DeleteCases[BinLists[i[[1]],{i[[2]],i[[3]],step}],{}]],
{i,MapThread[{#1,#2,#3}&,{aim,min,max}]}];
binning=Table[Table[Catch[Do[If[IntervalMemberQ[Interval[i],(k[[1]])[[j]]]==True,Throw[i]],
{i,Partition[Range[k[[2]],k[[3]],step],2,1]}]],{j,Length[k[[1]]]}],{k,MapThread[{#1,#2,#3}&,
{aim,min,max}]}];
aim=Table[DeleteCases[i,Null],{i,binning}];
{aim,campaign,seri}]


Clear[snetworkdatafxdbucketintimewindows]
snetworkdatafxdbucketintimewindows[data_,feature_,nodenumber_,datadimension_]:=Module[{rawaim,
pos,aim,campaign,seri,bucketsize,aimlabeled,aimpartitioned,bins,repetitivesreport,repetitives,
labelgeneration,binsrearranged,aimbinned},
rawaim=Table[data[[i]][[All,feature]],{i,Range@datadimension}];
pos=Table[Partition[Flatten@Table[Position[k,i],{i,{"NA",0}}],1],{k,rawaim}];
aim=Table[Delete[i[[1]],i[[2]]],{i,MapThread[{#1,#2}&,{rawaim,pos}]}];
campaign=Table[Delete[data[[i[[1]]]][[All,2]],i[[2]]],
{i,MapThread[{#1,#2}&,{Range@datadimension,pos}]}];
seri=Table[Delete[data[[i[[1]]]][[All,1]],i[[2]]],
{i,MapThread[{#1,#2}&,{Range@datadimension,pos}]}];
bucketsize=Table[Ceiling@(N@(Dimensions@i[[1]])/(i[[2]])),
{i,MapThread[{#1,#2}&,{aim,nodenumber}]}];
aimlabeled=Table[Thread[Range@Length@i->i],{i,aim}];
aimpartitioned=Table[Partition[Normal@Sort@Association@i[[1]],UpTo@i[[2]]],
{i,MapThread[{#1,#2}&,{aimlabeled,bucketsize}]}];
bins=Table[Table[MinMax[i],{i,Values@k}],{k,aimpartitioned}];
repetitivesreport=Table[DeleteCases[Tally@i,x_/;x[[2]]==1],{i,bins}];
repetitives=Table[i[[All,1]],{i,repetitivesreport}];

labelgeneration[x_,dim_]:=Table[{repetitives[[dim]][[x]][[1]]+i*repetitives[[dim]][[x]][[1]]
/(2*10^(RealDigits@repetitives[[dim]][[x]][[1]]+1)[[2]]),repetitives[[dim]][[x]][[2]]+
i*repetitives[[dim]][[x]][[2]]/(2*10^(RealDigits@repetitives[[dim]][[x]][[2]]+1)[[2]])},
{i,Range@repetitivesreport[[dim]][[x]][[2]]}];

binsrearranged=Table[ReplacePart[bins[[o]],Flatten[Table[MapThread[#1->#2&,
{Flatten@Position[bins[[o]],repetitives[[o]][[g]]],labelgeneration[g,o]}],
{g,Range@Length@repetitives[[o]]}],1]],{o,Range@datadimension}];

aimbinned=Table[Values@Sort[Flatten[Table[aimpartitioned[[k]][[i]]/.
Dispatch@Table[Values@aimpartitioned[[k]][[i]][[j]]->binsrearranged[[k]][[i]],
{j,Length@aimpartitioned[[k]][[i]]}],{i,Length@aimpartitioned[[k]]}],1],#1[[1]]<#2[[1]]&],
{k,Range@datadimension}];{aimbinned,campaign,seri,bucketsize}]


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


Clear[randomizinggraphdegfxd]
randomizinggraphdegfxd[network_]:=Module[{networknodes,edgelist,pairing,edgelistdegreefxd,
rewiredgraph},
networknodes=VertexList@network;
edgelist=EdgeList[network];
pairing=Partition[RandomSample@edgelist,2];
edgelistdegreefxd=Flatten@Table[If[x[[1,1]]!=x[[2,2]]&&x[[1,2]]!=x[[2,1]],{x[[2,1]]\[UndirectedEdge]x[[1,2]],
x[[1,1]]\[UndirectedEdge]x[[2,2]]},{x[[1,2]]\[UndirectedEdge]x[[2,2]],x[[1,1]]\[UndirectedEdge]x[[2,1]]}],{x,pairing}];
rewiredgraph=Graph[networknodes,edgelistdegreefxd]]


Clear[randomizinggraphmod]
randomizinggraphmod[network_]:=
Module[{networknodes,communitylist,intralinks,interlinks,intralinkamount,intrapairing,
intradegreesfxd,interpairing,interdegreesfxd,rewiredgraph},
networknodes=VertexList@network;
communitylist=FindGraphCommunities[network];
intralinks=EdgeList[Subgraph[network,#]]&/@communitylist;
interlinks=Complement[EdgeList[network],Flatten@intralinks];
intralinkamount=Table[Length@intralinks[[i]],{i,Length@communitylist}];
intrapairing=Table[Partition[RandomSample@i,2],{i,intralinks}];
intradegreesfxd=Table[Flatten@Table[If[x[[1,1]]!=x[[2,2]]&&x[[1,2]]!=x[[2,1]],
{x[[2,1]]\[UndirectedEdge]x[[1,2]],x[[1,1]]\[UndirectedEdge]x[[2,2]]},{x[[1,2]]\[UndirectedEdge]x[[2,2]],x[[1,1]]\[UndirectedEdge]x[[2,1]]}],{x,i}],
{i,intrapairing}];
interpairing=Partition[RandomSample@interlinks,2];
interdegreesfxd=Flatten@Table[If[x[[1,1]]!=x[[2,2]]&&x[[1,2]]!=x[[2,1]],{x[[2,1]]\[UndirectedEdge]x[[1,2]],
x[[1,1]]\[UndirectedEdge]x[[2,2]]},{x[[1,2]]\[UndirectedEdge]x[[2,2]],x[[1,1]]\[UndirectedEdge]x[[2,1]]}],{x,interpairing}];
rewiredgraph=Graph[networknodes,Flatten[{intradegreesfxd,interdegreesfxd}]]]


Clear[zscorefunctionfortwonullmodels]
zscorefunctionfortwonullmodels[network_]:=
Module[{X,randomgraphsdegreesfxd,modularityfxd,\[Mu]degreesfxd,\[Sigma]degreesfxd,
randomgraphscomm,modularitycomm,\[Mu]comm,\[Sigma]comm,ZScores},

X=N@GraphAssortativity[network,FindGraphCommunities[network],"Normalized"->False];

randomgraphsdegreesfxd=Table[randomizinggraphdegfxd[network],1000];
modularityfxd=Table[N@GraphAssortativity[
randomgraphsdegreesfxd[[i]],FindGraphCommunities[randomgraphsdegreesfxd[[i]]],
"Normalized"->False],{i,1000}];
\[Mu]degreesfxd=Mean[Table[modularityfxd[[i]],{i,1000}]];
\[Sigma]degreesfxd=StandardDeviation[Table[modularityfxd[[i]],{i,1000}]];

randomgraphscomm=Table[randomizinggraphmod[network],1000];
modularitycomm=Table[N@GraphAssortativity[randomgraphscomm[[i]],
FindGraphCommunities[randomgraphscomm[[i]]],"Normalized"->False],{i,1000}];
\[Mu]comm=Mean[Table[modularitycomm[[i]],{i,1000}]];
\[Sigma]comm=StandardDeviation[Table[modularitycomm[[i]],{i,1000}]];

ZScores={(X-\[Mu]degreesfxd)/\[Sigma]degreesfxd,(X-\[Mu]comm)/\[Sigma]comm}]


End[];
EndPackage[];
