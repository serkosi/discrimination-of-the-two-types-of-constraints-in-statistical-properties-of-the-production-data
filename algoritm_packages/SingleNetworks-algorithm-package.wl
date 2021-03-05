(* ::Package:: *)

BeginPackage["SingleNetworks`"];

snetworkdatasingleintimewindows::usage = "description.";
snetworkgraphsinglenodes::usage = "description.";
snetworkdatabinnedintimewindows::usage = "description.";
snetworkdatafxdbucketintimewindows::usage = "description.";
snetworkgraph::usage = "description.";
correlationfunction::usage = "description.";
randomnessfunction::usage = "description.";
GirwanNewmanmodularity::usage = "description.";
randomnessfunctionformodularity::usage = "description.";


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


Clear[snetworkdatabinnedintimewindows]
snetworkdatabinnedintimewindows[feature_,step_,datadimension_]:=Module[{rawaim,pos,aim,campaign,
seri,min,max,binningamount,binning},
rawaim=Table[Symbol["data"][[i]][[All,feature]],{i,Range@datadimension}];
pos=Table[Partition[Flatten@Table[Position[k,i],{i,{"NA",0}}],1],{k,rawaim}];
aim=Table[Delete[i[[1]],i[[2]]],{i,MapThread[{#1,#2}&,{rawaim,pos}]}];
campaign=Table[Delete[Symbol["data"][[i[[1]]]][[All,2]],i[[2]]],
{i,MapThread[{#1,#2}&,{Range@datadimension,pos}]}];
seri=Table[Delete[Symbol["data"][[i[[1]]]][[All,1]],i[[2]]],
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
snetworkdatafxdbucketintimewindows[feature_,nodenumber_,datadimension_]:=Module[{rawaim,pos,aim,
campaign,seri,bucketsize,aimlabeled,aimpartitioned,bins,repetitivesreport,repetitives,
labelgeneration,binsrearranged,aimbinned},
rawaim=Table[Symbol["data"][[i]][[All,feature]],{i,Range@datadimension}];
pos=Table[Partition[Flatten@Table[Position[k,i],{i,{"NA",0}}],1],{k,rawaim}];
aim=Table[Delete[i[[1]],i[[2]]],{i,MapThread[{#1,#2}&,{rawaim,pos}]}];
campaign=Table[Delete[Symbol["data"][[i[[1]]]][[All,2]],i[[2]]],
{i,MapThread[{#1,#2}&,{Range@datadimension,pos}]}];
seri=Table[Delete[Symbol["data"][[i[[1]]]][[All,1]],i[[2]]],
{i,MapThread[{#1,#2}&,{Range@datadimension,pos}]}];
bucketsize=Table[Ceiling@(N@(Dimensions@i)/nodenumber),{i,aim}];
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
Table[Values@aimpartitioned[[k]][[i]][[j]]->binsrearranged[[k]][[i]],
{j,Length@aimpartitioned[[k]][[i]]}],{i,Length@aimpartitioned[[k]]}],1],#1[[1]]<#2[[1]]&],
{k,Range@datadimension}];{aimbinned,campaign,seri}]


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


Clear[correlationfunction]
correlationfunction[network_,choice_]:=Module[{De,CC,BC,DeBC,CCBC,out},
De=VertexDegree[network];
CC=LocalClusteringCoefficient[network];
BC=BetweennessCentrality[network];
DeBC=Which[choice==1,SpearmanRho[De,BC],choice==2,Correlation[De,BC],choice==3,
KendallTau[[De,BC]]];
CCBC=Which[choice==1,SpearmanRho[CC,BC],choice==2,Correlation[CC,BC],choice==3,
KendallTau[[CC,BC]]];
out={DeBC,CCBC}]


Clear[randomnessfunction]
randomnessfunction[network_,choice_]:=
Module[{randomgraphs,MuDeBC,MuCCBC,XDeBC,XCCBC,SigmaDeBC,SigmaCCBC,ZDeBC,ZCCBC,final},
SeedRandom[17];
randomgraphs=RandomGraph[{VertexCount[network],EdgeCount[network]},1000];
MuDeBC=Mean[Table[correlationfunction[randomgraphs[[i]],choice][[1]],{i,1000}]];
MuCCBC=Mean[Table[correlationfunction[randomgraphs[[i]],choice][[2]],{i,1000}]];
XDeBC=correlationfunction[network,choice][[1]];
XCCBC=correlationfunction[network,choice][[2]];
SigmaDeBC=StandardDeviation[Table[correlationfunction[randomgraphs[[i]],choice][[1]],{i,1000}]];
SigmaCCBC=StandardDeviation[Table[correlationfunction[randomgraphs[[i]],choice][[2]],{i,1000}]];
ZDeBC=(XDeBC-MuDeBC)/SigmaDeBC;
ZCCBC=(XCCBC-MuCCBC)/SigmaCCBC;
final={ZDeBC,ZCCBC}];


Clear[GirwanNewmanmodularity]
GirwanNewmanmodularity[xx_]:=Module[{network,m,Aij,B,u,\[Beta],s,Q,div1,div2,moditer,r1,r11,r12,r2,
r21,r22,subfinal1Q,subfinal2Q,finalQ},
network=xx;
m=EdgeCount@network;
Aij=AdjacencyMatrix@network;
B=N@(Aij-Outer[Times,VertexDegree@network,VertexDegree@network]/(2*m));
u=Eigenvectors@B;
\[Beta]=Eigenvalues@B;
s=(u/.x_/;x<0->-1)/.x_/;x>=0->1;
Q=(Total@(Table[(u[[i]] . Flatten@s[[Flatten@Position[\[Beta],Max@\[Beta]][[1]]]])^2,
{i,VertexList@network}]*\[Beta]))/(4*m);
div1=Flatten@Position[Flatten@s[[Flatten@Position[\[Beta],Max@\[Beta]][[1]]]],_?Negative];
div2=Flatten@Position[Flatten@s[[Flatten@Position[\[Beta],Max@\[Beta]][[1]]]],_?Positive];
moditer[pos_,B1_]:=Module[{Bg,ug,\[Beta]g,sg,deltaQ,divv1,divv2,Bg1,Bg2},
Bg=(Table[B1[[i,j]]-KroneckerDelta[i,j]*Total@B1[[i,pos]],{i,Length@B1},{j,Length@B1}])
[[pos,pos]];
ug=Eigenvectors@Bg;
\[Beta]g=Eigenvalues@Bg;
sg=(ug/.x_/;x<0->-1)/.x_/;x>=0->1;
deltaQ=(Total@(Table[(ug[[i]] . Flatten@sg[[Flatten@Position[\[Beta]g,Max@\[Beta]g][[1]]]])^2,
{i,Range@Length@pos}]*\[Beta]g))/(4*m);
divv1=Flatten@Position[Flatten@sg[[Flatten@Position[\[Beta]g,Max@\[Beta]g][[1]]]],_?Negative];
divv2=Flatten@Position[Flatten@sg[[Flatten@Position[\[Beta]g,Max@\[Beta]g][[1]]]],_?Positive];
{deltaQ,divv1,divv2,Bg}];
r1=moditer[div1,B];
r11=If[r1[[1]]>0.001,moditer[r1[[2]],r1[[4]]],{0,0,0,0}];
r12=If[r1[[1]]>0.001,moditer[r1[[3]],r1[[4]]],{0,0,0,0}];
r2=moditer[div2,B];
r21=If[r2[[1]]>0.001,moditer[r2[[2]],r2[[4]]],{0,0,0,0}];
r22=If[r2[[1]]>0.001,moditer[r2[[3]],r2[[4]]],{0,0,0,0}];
subfinal1Q=If[r1[[1]]>0.001,If[r11[[1]]>0.001,If[r12[[1]]>0.001,r12[[1]]+r11[[1]]+r1[[1]],
r11[[1]]+r1[[1]]],If[r12[[1]]>0.001,r12[[1]]+r1[[1]],r1[[1]]]],0];
subfinal2Q=If[r2[[1]]>0.001,If[r21[[1]]>0.001,If[r22[[1]]>0.001,r22[[1]]+r21[[1]]+r2[[1]],
r21[[1]]+r2[[1]]],If[r22[[1]]>0.001,r22[[1]]+r2[[1]],r2[[1]]]],0];
finalQ=Q+subfinal1Q+subfinal2Q]


Clear[randomnessfunctionformodularity]
randomnessfunctionformodularity[network_]:=
Module[{randomgraphs,RndmMumodularity,Xmodularityvalues,XWolfmodularityvalues,
RndSigmamodularity,ZScore},
randomgraphs=RandomGraph[{VertexCount[network],EdgeCount[network]},1000];
RndmMumodularity=Mean[Table[GirwanNewmanmodularity[randomgraphs[[i]]],{i,1000}]];
Xmodularityvalues=GirwanNewmanmodularity[network];
XWolfmodularityvalues=N@GraphAssortativity[network,FindGraphCommunities[network],
"Normalized"->False];
RndSigmamodularity=StandardDeviation[Table[GirwanNewmanmodularity[randomgraphs[[i]]],{i,1000}]];
ZScore=Table[(i-RndmMumodularity)/RndSigmamodularity,{i,{Xmodularityvalues,
XWolfmodularityvalues}}]];


End[];
EndPackage[];
