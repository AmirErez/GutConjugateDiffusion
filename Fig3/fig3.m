(* ::Package:: *)

(* ::Input:: *)
(*Clear["Global`*"]*)
(*(*----parameters specification---*)(*--units of half an hour---*)*)
(* bm=1;k=1;*)
(*fia=1;*)
(*fib=3;*)
(*ts=fia;*)
(*mu1=1;mu2=mu1;v1=1;v2=v1;*)
(*xout=1; lm=xout;dd=1;*)
(*(*---define the time shift---*)*)
(*w=Pi/ 8.9;(*--for bacteria--*);tf=24 ;dt=7.2;wc=w;*)
(*f[nu_,beta_]:={vecinit=RandomReal[10,ts];*)
(*tf=23;*)
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
(*output={nu,beta,intA,intB};*)
(*output*)
(*};*)
(*master=Flatten[Table[f[i,j][[1]],{j,0.01,10,0.1},{i,0.01,1,0.02}],1];*)
(**)


(* ::Input:: *)
(*master2=master;*)
(*tabA=Table[{master2[[i]][[1]],master2[[i]][[2]],master2[[i]][[3]]},{i,1,Length[master2],1}];*)
(*tabB=Table[{master2[[i]][[1]],master2[[i]][[2]],master2[[i]][[4]]},{i,1,Length[master2],1}];*)
(*tabProd=Table[{master2[[i]][[1]],master2[[i]][[2]],master2[[i]][[3]]master2[[i]][[4]]},{i,1,Length[master2],1}];*)
(*t1=ListContourPlot[tabB,ColorFunction->"Rainbow",PlotLegends->Automatic,FrameLabel->{"\[Nu]","\[Beta]"},Contours->{1,2,3,4,5,6},ContourLabels->(Text[Framed[#3],{#1,#2},Background->White]&)]*)
(*(*Export["/home/turgon/Dropbox/Israel/Project-III/Fig4/amp11.dat",ExportString[tabA,"Table"]]*)
(*Export["/home/turgon/Dropbox/Israel/Project-III/Fig4/bac11.dat",ExportString[tabB,"Table"]]*)
(*Export["/home/turgon/Dropbox/Israel/Project-III/Fig4/prod11.dat",ExportString[tabProd,"Table"]]*)*)


(* ::Input:: *)
(*Export["/home/turgon/Dropbox/Israel/Project-III/Fig3/bac13.dat",ExportString[tabB,"Table"]]*)


(* ::Input:: *)
(*ListContourPlot[tabA,ColorFunction->"SunsetColors",PlotLegends->Automatic,FrameLabel->{"\[Nu]","\[Beta]"},ContourLabels->(Text[Framed[#3],{#1,#2},Background->White]&)]*)


(* ::Input:: *)
(*data=tabB;*)
(*(*-function g will be the function that calculates the product,aka cost to host, along the chosen contour--*)*)
(*g[nu_,beta_]:={vecinit=RandomReal[10,ts];*)
(*tf=23;*)
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
(*output=intA intB;*)
(*output*)
(*};*)
(*plt=ListContourPlot[data,MeshStyle->Thick,Contours->{6}];*)
(*lst=Cases[plt[[1]]//Normal,Line[z_]->z,Infinity][[1]];*)
(*tr=Table[{Part[lst,i][[1]],Part[lst,i][[2]],g[Part[lst,i][[1]],Part[lst,i][[2]]][[1]]},{i,1,Length[lst],1}];*)
(*tw=Table[Part[tr,i][[3]],{i,1,Length[tr],1}];*)
(*res={SortBy[tr,Last][[1]][[1]],SortBy[tr,Last][[1]][[2]]}*)
(*tr*)
(*Min[tw]*)
(*SortBy[tr,Last][[1]]*)
(**)


(* ::Input:: *)
(*Export["/home/turgon/Dropbox/Israel/Project-III/Fig4/prod13.dat",ExportString[tabProd,"Table"]]*)


(* ::Input:: *)
(*ListPlot[{%485,%495,%505,%515}]*)


(* ::Input:: *)
(*SortBy[tr,Last][[1]]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*tw*)


(* ::Input:: *)
(*Min[tw]*)


(* ::Input:: *)
(*ListContourPlot[%122]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*t1=ListContourPlot[tabProd,ColorFunction->"SunsetColors",PlotLegends->Automatic,FrameLabel->{"\[Nu]","\[Beta]"},ContourLabels->(Text[Framed[#3],{#1,#2},Background->White]&)]*)
