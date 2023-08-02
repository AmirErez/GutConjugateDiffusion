
Clear["Global`*"]
 bm=1;k=1;
fia=1;
fib=6;
ts=fia;
mu1=1;mu2=mu1;v1=1;v2=v1;
xout=1; lm=xout;dd=1;
(*---define the time shift---*)
w=Pi/ 8.9;(*--for bacteria--*);tf=24 ;dt=7.2;wc=w;
f[nu_,beta_]:={vecinit=RandomReal[10,ts];
tf=23;
Do[res=Table[
ct[ti_]=If[ti<ts,vecinit[[ti+1]],cp[ti-ts]];
bm1=bm Cos[w (t-dt-fib)/2]^2;
(*---*)
csfb=bm Cos[w (t-dt)/2]^2;

cm=bm1;
(*----*)
a1=(1-nu)beta bm Cos[w (t-dt-fia)/2]^2+
nu beta ct[t];
aa[x_]=a1(1-x);
b01[y_]=bm1 Exp[-(a1/2)(1-y)^2];
cc[x_]=(bm1+a1 cm-(bm1) E^(-(1/2) a1 (-1+x)^2))/a1-((bm1) Sqrt[\[Pi]/2] (-1+x) Erf[(Sqrt[a1] (-1+x))/Sqrt[2]])/Sqrt[a1];
cp[t]=cc[0];
vr={nu,ts,t,ct[t],cc[0],aa[0],b01[0],bm1//N,csfb//N,cc[0]};
Clear[a1,b1,b2,c1,c2];
vr,{t,0,tf,1}];
vecinit=Table[cp[j],{j,tf-ts+1,tf,1}];,{h,1,2,1}];
listb0=Table[{Part[res,i][[3]],Part[res,i][[7]]},{i,1,Length[res],1}];
lista0=Table[{Part[res,i][[3]],Part[res,i][[6]]},{i,1,Length[res],1}];
intB=Integrate[Interpolation[listb0][x],{x,0,Length[listb0]-1}];
intA=Integrate[Interpolation[lista0][x],{x,0,Length[listb0]-1}];
output={nu,beta,intA,intB};
output
};
master=Flatten[Table[f[i,j][[1]],{j,0.01,10,0.1},{i,0.01,1,0.02}],1];



master2=master;
tabA=Table[{master2[[i]][[1]],master2[[i]][[2]],master2[[i]][[3]]},{i,1,Length[master2],1}];
tabB=Table[{master2[[i]][[1]],master2[[i]][[2]],master2[[i]][[4]]},{i,1,Length[master2],1}];
tabProd=Table[{master2[[i]][[1]],master2[[i]][[2]],master2[[i]][[3]]master2[[i]][[4]]},{i,1,Length[master2],1}];
t1=ListContourPlot[tabB,ColorFunction->"SunsetColors",PlotLegends->Automatic,FrameLabel->{"\[Nu]","\[Beta]"},ContourLabels->(Text[Framed[#3],{#1,#2},Background->White]&)]
Export["/home/theoden/Desktop/Figs-HostMicrobiome/Fig-Appendix2/bac16.dat",ExportString[tabB,"Table"]]

