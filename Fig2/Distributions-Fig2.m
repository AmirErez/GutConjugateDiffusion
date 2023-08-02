(* ::Package:: *)

(* ::Input:: *)
(*Clear["Global`*"]*)
(*(*----parameters specification---*)*)
(* bm=10;beta=0.4;k=1;w=Pi/12;mu1=1;mu2=mu1;v1=1;v2=v1;fib=3;*)
(*xout=1; lm=xout;dd=1;*)
(*(*---define the time shift---*)*)
(**)
(**)
(*f[nu_,ts_]:={vecinit=RandomReal[10,ts];*)
(*tf=23;*)
(*Do[res=Table[*)
(*ct[ti_]=If[ti<ts,vecinit[[ti+1]],cp[ti-ts]];*)
(**)
(*bm1=bm Cos[w (t-fib)/2]^2;*)
(*(*---*)*)
(*fia=ts;*)
(*csfb=bm Cos[w (t)/2]^2;*)
(**)
(*cm=bm1;*)
(*(*----*)*)
(*a0[x_]=0;b01[x_]=0.0001;c0[x_]=0;*)
(*Do[vcheck=c0[0];*)
(*{a0[x_],b01[x_],c0[x_]}={a[x],b1[x],c[x]}/.NDSolve[{dd a''[x]==0,v1 b1'[x]- mu1 a[x]b01[x]-mu1  a0[x](b1[x]-b01[x])==0,*)
(*dd c''[x]+ b1[x]==0,a[1]==0,a[0]==(1-nu)beta bm Cos[w (t-fia)/2]^2+*)
(*nu beta ct[t],c[lm]==cm,c'[xout]==0,b1[lm]==bm1},{a[x],b1[x],c[x]},{x,0,xout},Method->"ExplicitEuler","StartingStepSize"->1/200][[1]];If[Abs[c[0]-vcheck]<10^(-5),Break[]],{i,1,50}];*)
(*(*case where both mu's=1 and both velocities='--*)*)
(*cp[t]=c0[0];*)
(*vr={nu,ts,t,ct[t],c0[0],a0[0],b01[0],bm1//N,csfb//N,c0[0]};*)
(*vr,{t,0,tf,1}];*)
(*vecinit=Table[cp[j],{j,tf-ts+1,tf,1}];,{h,1,3,1}];res};*)


(* ::Input:: *)
(*fib*)


(* ::Input:: *)
(*res0=f[0,1]*)


(* ::Input:: *)
(*res05=f[0.5,1]*)


(* ::Input:: *)
(*res1=f[1,1]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)
(**)


(* ::Input:: *)
(*res=Flatten[res1,1];*)
(*listb0=Table[{Part[res,i][[3]],Part[res,i][[7]]},{i,1,Length[res],1}];*)
(*lista0=Table[{Part[res,i][[3]],Part[res,i][[6]]},{i,1,Length[res],1}];*)
(*intB=Integrate[Interpolation[listb0][x],{x,0,Length[listb0]-1}];*)
(*intA=Integrate[Interpolation[lista0][x],{x,0,Length[listb0]-1}];*)
(*output={nu,beta,intA,intB};*)
(*tgt=listb0;*)
(*listR=Join[tgt,Table[{Part[tgt,i][[1]]+24,Part[tgt,i][[2]]},{i,1,Length[tgt]}],Table[{Part[tgt,i][[1]]+48,Part[tgt,i][[2]]},{i,1,1}]];*)
(*StringReplace[ToString[listR],{"}, {"->"),(","{{"->"(","}}"-> ")"}]*)


(* ::Input:: *)
(*ListLinePlot[listR,PlotStyle->Blue]*)


(* ::Input:: *)
(*Show[%42,%52,%62,PlotRange->All]*)


(* ::Input:: *)
(*Show[ListPlot[listR],ListPlot[lista0],Plot[bm Cos[w t/2]^2,{t,0,48}],PlotRange->All]*)


(* ::Input:: *)
(*listR*)


(* ::Input:: *)
(*ListPlot[%44,Joined->True]*)


(* ::Input:: *)
(*csfb*)


(* ::Input:: *)
(*tx=Chop[Table[{t,bm Cos[w (t)/2]^2//N},{t,0,48,1}],10^-6];*)
(*StringReplace[ToString[tx],{"}, {"->"),(","{{"->"(","}}"-> ")"}]*)


(* ::Input:: *)
(*bm Cos[w (t)/2]^2;*)
