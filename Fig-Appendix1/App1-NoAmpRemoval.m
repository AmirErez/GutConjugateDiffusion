Clear["Global`*"]
(*----parameters specification---*)
 bm=1;beta=0.4;k=1;w=Pi/12;fia=3;mu1=1;mu2=mu1;v1=1;v2=v1;
xout=1; lm=xout;dd=1;nu=1;
(*---define the time shift---*)


bm1=5.5 ;
bm2=5.5 ;
(*---*)
cm=bm1+bm2;
(*----*)
a0[x_]=0;b01[x_]=0.0001;b02[x_]=0.0001;c0[x_]=0;
Do[vcheck=c0[0];
{a0[x_],b01[x_],b02[x_],c0[x_]}={a[x],b1[x],b2[x],c[x]}/.NDSolve[{dd a''[x]==0,v1 b1'[x]- mu1 a[x]b01[x]-mu1  a0[x](b1[x]-b01[x])==0,
v2   b2'[x]-mu2 a[x] b02[x]-mu2 a0[x] (b2[x]-b02[x])==0 ,dd c''[x]+ b1[x]+b2[x]==0,a[1]==0,a[0]==nu beta c0[0],c[lm]==cm,c'[xout]==0c0'[0]-0(1/dd)NIntegrate[b01[y]+b02[y],{y,0,lm},Method->{"LocalAdaptive","SymbolicProcessing"->0}],b1[lm]==bm1,b2[lm]==bm2},{a[x],b1[x],b2[x],c[x]},{x,0,xout},Method->"ExplicitEuler","StartingStepSize"->1/250][[1]];If[Abs[c[0]-vcheck]<10^(-5),Break[]],{i,1,50}];
a0[0]
{b01[0],b02[0]}
(*case where both mu's=1 and both velocities='--*)
Plot[{a0[x],b01[x],b02[x],c0[x]},{x,0,lm},PlotStyle->{Blue,Red,{Red,Dashed},Gray},Mesh->50,MeshShading->{None}]



ta=Table[{x,a0[x]},{x,0,1,0.05}];
tb1=Table[{x,b01[x]},{x,0,1,0.05}];
tb2=Table[{x,b02[x]},{x,0,1,0.05}];
tc=Table[{x,c0[x]},{x,0,1,0.05}];
StringReplace[ToString[ta],{"}, {"->"),(","{{"->"(","}}"-> ")"}]
StringReplace[ToString[tb1],{"}, {"->"),(","{{"->"(","}}"-> ")"}]
StringReplace[ToString[tb2],{"}, {"->"),(","{{"->"(","}}"-> ")"}]
StringReplace[ToString[tc],{"}, {"->"),(","{{"->"(","}}"-> ")"}]
