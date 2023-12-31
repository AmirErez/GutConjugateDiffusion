(* ::Package:: *)

(* ::Input:: *)
(**)
(*(*--README: this program first collect the contour line from the bacterial load. Here, we pick the nu and beta coordinates that yield a load of 12. This set of data, ie., fia and fib, and the nu and beta that make the contour, enter a .m file that is then read in a second block of this program. From this reading, we call the function g[nu,beta] which, along the contour and for each fia and fib, calculate the minimun in the cost to host, the amp b0 integral. This calculation results in an output of the form, fia fib, nu and beta, which we then use in a python program to calculate the heatmaps of fig5----*)*)
(**)
(**)
(*Clear["Global`*"]*)
(*(*----parameters specification---*)*)
(* bm=1;k=1;mult=6;w=Pi/(8.90 mult) (*--for bacteria--*);tf=23  mult;dt=7.2  mult;wc=w;(*--period for the csfb 12h--*)*)
(*resultado=Table[Table[*)
(*ts=fia;*)
(*mu1=1;mu2=mu1;v1=1;v2=v1;*)
(*xout=1; lm=xout;dd=1;*)
(*(*---define the time shift---*)*)
(**)
(*f[nu_,beta_]:={*)
(**)
(*vecinit=RandomReal[30,ts];*)
(**)
(*Do[res=Table[*)
(*bm1=bm Cos[w (t-dt-fib)/2]^2;*)
(*(*---*)*)
(*csfb=bm Cos[wc (t-dt)/2]^2;*)
(**)
(*cm=bm1;*)
(*aa[x_]=a1(1-x);*)
(*b01[y_]=bm1 Exp[-(a1/2)(1-y)^2];*)
(*cc[x_]=(bm1+a1 cm-(bm1) E^(-(1/2) a1 (-1+x)^2))/a1-((bm1) Sqrt[\[Pi]/2] (-1+x) Erf[(Sqrt[a1] (-1+x))/Sqrt[2]])/Sqrt[a1];*)
(*(*--as of July 2022, ts equals Phi_a--*)*)
(*If[ts==0,a1=a1/.FindRoot[a1==(1-nu)beta bm Cos[w (t-dt)/2]^2+*)
(*nu beta cc[0],{a1,1,20}][[1]];*)
(*bm1=bm Cos[w (t-dt-fib)/2]^2;*)
(*(*---*)*)
(*csfb=bm Cos[w (t-dt)/2]^2;*)
(**)
(*cm=bm1;*)
(*aa[x_]=a1(1-x);*)
(*b01[y_]=bm1 Exp[-(a1/2)(1-y)^2];*)
(*cc[x_]=(bm1+a1 cm-(bm1) E^(-(1/2) a1 (-1+x)^2))/a1-((bm1) Sqrt[\[Pi]/2] (-1+x) Erf[(Sqrt[a1] (-1+x))/Sqrt[2]])/Sqrt[a1];*)
(*];*)
(*If[ts!=0,*)
(*ct[ti_]=If[ti<ts,vecinit[[ti+1]],cp[ti-ts]];*)
(*bm1=bm Cos[w (t-dt-fib)/2]^2;*)
(*(*---*)*)
(*csfb=bm Cos[wc ( t-dt)/2]^2;*)
(**)
(*cm=bm1;*)
(*(*----*)*)
(*a1=(1-nu)beta bm Cos[wc (t-dt-fia)/2]^2+*)
(*nu beta ct[t];*)
(*(*----*)*)
(*aa[x_]=a1(1-x);*)
(*b01[y_]=bm1 Exp[-(a1/2)(1-y)^2];*)
(*cc[x_]=(bm1+a1 cm-(bm1) E^(-(1/2) a1 (-1+x)^2))/a1-((bm1) Sqrt[\[Pi]/2] (-1+x) Erf[(Sqrt[a1] (-1+x))/Sqrt[2]])/Sqrt[a1];*)
(**)
(*cp[t]=cc[0];];*)
(*vr={nu,ts,t,ct[t],cc[0],aa[0],b01[0],bm1//N,csfb//N,cc[0]};*)
(*Clear[a1,b1,b2,c1,c2];*)
(*vr,{t,0,tf,1}];*)
(*vecinit=Table[cp[j],{j,tf-ts+1,tf,1}];,{h,1,2,1}];*)
(*listb0=Table[{Part[res,i][[3]],Part[res,i][[7]]},{i,1,Length[res],1}];*)
(*intB=Integrate[Interpolation[listb0][x],{x,0,Length[listb0]-1}];*)
(*output={nu,beta,intB};*)
(*output*)
(*};*)
(*master=Flatten[Table[f[i,j][[1]],{j,0.01,20,.5},{i,0.0,1,0.1}],1];*)
(*tabB=master;*)
(*data=tabB;*)
(*plt=ListContourPlot[data,Contours->{12}];*)
(*lst=Cases[plt[[1]]//Normal,Line[z_]->z,Infinity][[1]];*)
(*{fia,fib,lst},{fia,0,6  mult,1}],{fib,0 mult,6  mult,1}];*)
(*Export["/home/theoden/Downloads/listanova.m",resultado]*)
(**)
(**)
(**)


(* ::Input:: *)
(*Clear["Global`*"]*)
(*in0=Import["/home/theoden/Downloads/listanova.m"];*)
(*r1=in0;*)
(*out=Table[bm=1;k=1;mult=6;w=Pi/(8.90 mult) (*--for bacteria--*);tf=23  mult;dt=7.2  mult;wc=w;(*--period for the csfb 12h--*)*)
(**)
(*fia=r1[[i]][[1]];*)
(*fib=r1[[i]][[2]];*)
(*ts=fia;*)
(*mu1=1;mu2=mu1;v1=1;v2=v1;*)
(*xout=1; lm=xout;dd=1;*)
(*(*---define the time shift---*)*)
(*(*----*)*)
(*g[nu_,beta_]:={vecinit=RandomReal[30,ts];*)
(*Do[res=Table[*)
(*ct[ti_]=If[ti<ts,vecinit[[ti+1]],cp[ti-ts]];*)
(*bm1=bm Cos[w (t-dt-fib)/2]^2;*)
(*(*---*)*)
(*csfb=bm Cos[w (t-dt)/2]^2;*)
(**)
(*cm=bm1;*)
(*(*----*)*)
(*a1=(1-nu)beta bm Cos[w (t-dt-fia)/2]^2+*)
(*nu beta ct[t];*)
(*aa[x_]=a1(1-x);*)
(*b01[y_]=bm1 Exp[-(a1/2)(1-y)^2];*)
(*cc[x_]=(bm1+a1 cm-(bm1) E^(-(1/2) a1 (-1+x)^2))/a1-((bm1) Sqrt[\[Pi]/2] (-1+x) Erf[(Sqrt[a1] (-1+x))/Sqrt[2]])/Sqrt[a1];*)
(*cp[t]=cc[0];*)
(*vr={nu,ts,t,ct[t],cc[0],aa[0],b01[0],bm1//N,csfb//N,cc[0]};*)
(*Clear[a1,b1,b2,c1,c2];*)
(*vr,{t,0,tf,1}];*)
(*vecinit=Table[cp[j],{j,tf-ts+1,tf,1}];,{h,1,2,1}];*)
(*listb0=Table[{Part[res,i][[3]],Part[res,i][[7]]},{i,1,Length[res],1}];*)
(*lista0=Table[{Part[res,i][[3]],Part[res,i][[6]]},{i,1,Length[res],1}];*)
(*intB=Integrate[Interpolation[listb0][x],{x,0,Length[listb0]-1}];*)
(*intA=Integrate[Interpolation[lista0][x],{x,0,Length[listb0]-1}];*)
(*prod=intA intB;*)
(*prod*)
(*};*)
(*lst=r1[[i]][[3]];*)
(*tr=Table[{Part[lst,i][[1]],Part[lst,i][[2]],g[Part[lst,i][[1]],Part[lst,i][[2]]][[1]]},{i,1,Length[lst],1}];*)
(*final={fia,fib,SortBy[tr,Last][[1]][[1]],SortBy[tr,Last][[1]][[2]]};final,{i,1,Length[r1],1}]*)
(**)
(**)


(* ::Input:: *)
(*outBeta=Table[{Part[%4,i][[1]],Part[%4,i][[2]],Part[%4,i][[3]]},{i,1,Length[%4],1}];*)


(* ::Input:: *)
(*ListDensityPlot[outBeta,PlotLegends->{Placed[BarLegend[Automatic,None,LegendMarkerSize->250,LegendLabel->"b(0)"],Right]},ColorFunction->ColorData["SunsetColors"],FrameLabel->{Style["\[Delta]",Bold,16],Style["\[Nu]",Bold,16]},LabelStyle->{FontSize->22}]*)


(* ::Input:: *)
(*Export["/home/theoden/Downloads/output.txt",ExportString[%4,"Table"]]*)
