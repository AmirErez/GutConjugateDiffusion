Clear["Global`*"]
(*CSFB data from Brooks spreadsheet----*)
csfb[0] = 
  DeleteCases[{1, 4, 3, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 8, 5, 6, 5, 11, 7, 7, 6, 9, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0}, 0];
csfb[6] = 
  DeleteCases[{3, 4, 1, 0, 0, 0, 0, 0, 0, 0, 1, 8, 6, 4, 6, 4, 4, 6, 
    7, 6, 1, 2, 0, 0, 0, 0, 0, 2, 0, 0, 19, 15, 20, 12, 27, 7, 25, 34,
     3, 18}, 0];
csfb[12] = 
  DeleteCases[{7, 6, 7, 5, 6, 2, 7, 8, 10, 6, 25, 27, 24, 30, 21, 22, 
    29, 30, 20, 24, 18, 10, 12, 19, 23, 10, 30, 11, 32, 15, 6, 14, 7, 
    9, 13, 17, 8, 11, 7, 9}, 0];
csfb[18] = 
  DeleteCases[{1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 2, 0, 0, 0, 0, 0, 
    0, 0, 5, 4, 4, 5, 4, 7, 4, 4, 4, 5, 7, 9, 9, 5, 5, 9, 7, 6, 9, 4},
    0];
    
    
(*-----------------------------*)
hlist = {0, 6, 12, 18};
l0 = Table[i, {i, 0, 24, 2}];
dataCsfb = Table[{i, Mean[csfb[i]]}, {i, hlist}];
errorCsfb = Table[StandardDeviation[csfb[i]], {i, hlist}];
(*fulldataCsfb=Table[{{i,Mean[csfb[i]]},ErrorBar[errorCsfb[[i/6+1]]//\
N]},{i,hlist}];*)
(*----we can rescale everything to be in the 0-1 interval---*)
rcsfb = Rescale[Table[dataCsfb[[i]][[2]], {i, 1, Length[hlist]}]];
(*recsfb=Table[(errorCsfb[[i]])/(Max[errorCsfb]-Min[errorCsfb]),{i,1,\
4,1}];*)
recsfb = Rescale[errorCsfb, {0, Max[errorCsfb]}];
fulldataCsfb = 
  Table[{{i, rcsfb[[i/6 + 1]]}, ErrorBar[recsfb[[i/6 + 1]] // N]}, {i,
      hlist}] // N;
csm[t_] := Cos [Pi (t - dt1)/(2 jb)]^2;

(*--------Eubacteria from Frazier Figure 2C-------------*)
fraBm = {{2, 1.8 10^5}, {6, 3.6 10^5}, {10, 1.7 10^5}, {14, 
    1.1 10^5}, {18, 1.3 10^5}, {22, 3.2 10^5}};
errorBm = {{2, 0.1 10^5}, {6, 0.4 10^5}, {10, 0.6 10^5}, {14, 
    1.0 10^5}, {18, .2 10^5}, {22, 1. 10^5}};
Needs["ErrorBarPlots`"]
dataBm = {{{2, 1.8 10^5}, ErrorBar[0.1 10^5]}, {{6, 3.6 10^5}, 
    ErrorBar[0.4 10^5]}, {{10, 1.7 10^5}, 
    ErrorBar[0.6 10^5]}, {{14, 1.1 10^5}, 
    ErrorBar[1.0 10^5]}, {{18, 1.3 10^5}, 
    ErrorBar[.2 10^5]}, {{22, 3.2 10^5}, ErrorBar[1. 10^5]}};
l0 = Table[i, {i, 0, 24, 4}];
frahlist = {2, 6, 10, 14, 18, 22};
rfraBm = Rescale[Table[fraBm[[i]][[2]], {i, 1, Length[frahlist]}]];
eBm = Table[errorBm[[i]][[2]], {i, 1, Length[frahlist]}];
refraBm = Rescale[eBm, {0, Max[eBm]}];
fulldataBm = 
  Table[{{2 (2 i - 1), rfraBm[[i]]}, ErrorBar[refraBm[[i]] // N]}, {i,
      1, Length[frahlist]}] // N;
(*--jb is how the period. For Brooks, for example,we took it as Pi/12----*)
bac[t_] := Cos [Pi (t - dt1 - fib)/(2 jb)]^2;


indatacsfb = Table[{i, rcsfb[[i/6 + 1]]}, {i, hlist}] // N;
indataBm = 
  Table[{2 (2 i - 1), rfraBm[[i]]}, {i, 1, Length[frahlist]}] // N;

allData = 
  Join[{1, Sequence @@ #} & /@ indatacsfb, {2, Sequence @@ #} & /@ 
    indataBm];
myF[index_, t_] := 
  KroneckerDelta[index - 1] csm[t] + KroneckerDelta[index - 2] bac[t];

csbacnlm = 
  NonlinearModelFit[
   allData, {myF[index, t], dt1 > 0, jb > 6, fib > .5}, {dt1, jb, 
    fib}, {index, t}, Weights -> 1/Join[recsfb, refraBm]^2, 
   ConfidenceLevel -> 0.95, 
   VarianceEstimatorFunction -> (Mean[#^2] &)];
csbacnlm["ParameterTable"] // Quiet
{dt1, jb, fib} = {dt1, jb, fib} /. csbacnlm["BestFitParameters"];



(*----period for the eubacteria  wb=Pi/jb-----*)
wb = Pi/jb;

Needs["ErrorBarPlots`"]
Show[
 ErrorListPlot[fulldataCsfb, Joined -> False, 
  PlotMarkers -> Automatic, PlotStyle -> {Black}, 
  PlotRange -> {{-1, 24}, {-1, 2.5}}, 
  GridLines -> {{0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24}, 
    Automatic}, FrameTicks -> {l0, Automatic}, Frame -> True, 
  FrameLabel -> {"Zeitgeber Time (ZT)", 
    "Attaching SFB per \!\(\*SubscriptBox[\(villusC\), \(SFB\)]\) - Brooks- 6D"}, 
  PlotLegends -> {"\!\(\*SubscriptBox[\(C\), \(SFB\)]\) - Data from Brooks- 6D"}], 
 Plot[csm[t], {t, 0, 24}, PlotStyle -> Darker[Green], 
  PlotLegends -> {"NonLinearModelFit: Cos [Pi(t-dt1)/24]^2"}]]
(*---------------*)



Show[
 ErrorListPlot[fulldataBm, Joined -> False, PlotMarkers -> Automatic, 
  PlotStyle -> {Red}, PlotRange -> {{-1, 24}, {-1, 2.5}}, 
  GridLines -> {{0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24}, 
    Automatic}, FrameTicks -> {l0, Automatic}, Frame -> True, 
  FrameLabel -> {"Zeitgeber Time (ZT)", "16S rRNA gene abundace"}, 
  PlotLegends -> {"Bacteria - 16SrRNA -Frazier 2C"}], 
 Plot[bac[t], {t, 0, 24}, PlotStyle -> Darker[Red], 
  PlotLegends -> {"NonLinearModelFit: Cos [Pi(t-dt1-fib)/(2jb)]^2"}]] 
  
  
  
  
  (*----REG3G from Frazier---*)
framp = {{2, 4}, {6, 5.945075757575758}, {10, 10.672348484848484},
   {14, 8.16098484848485},
   {18., 2.645833333333332}, {22., 3.3106060606060606}};
erroramp = {{2, 1.80}, {6, 1.76}, {10, 2.25}, {14, 1}, {18, 1.2}, {22,
     1.90}};
Needs["ErrorBarPlots`"]
dataamp = {{{2, 4}, ErrorBar[1.8]}, {{6, 5.945075757575758}, 
    ErrorBar[1.76]}, {{10, 10.672348484848484}, ErrorBar[2.25]},
   {{14, 8.16098484848485}, ErrorBar[1]}, {{18., 2.645833333333332}, 
    ErrorBar[1.2]}, {{22., 3.3106060606060606}, ErrorBar[1.9]}};
    frahlist = {2, 6, 10, 14, 18, 22};
rfraAmp = Rescale[Table[framp[[i]][[2]], {i, 1, Length[frahlist]}]];
eAmp = Table[erroramp[[i]][[2]], {i, 1, Length[frahlist]}];
refraAmp = Rescale[eAmp, {0, Max[eAmp]}];
fulldataAmp = 
  Table[{{2 (2 i - 1), rfraAmp[[i]]}, 
     ErrorBar[refraAmp[[i]] // N]}, {i, 1, Length[frahlist]}] // N;
(*--jb is how the period. For Brooks, for example,we took it as Pi/12----*)
indataAmp = 
  Table[{2 (2 i - 1), rfraAmp[[i]]}, {i, 1, Length[frahlist]}] // N;


Clear[fia, beta, nu, ct, a1, av, bc, x1, y1, z1]

av[t_] =  x1+y1 Cos [Pi (t - z1)/(2 jb)]^2;

famp[t_] = av[t];
ampnlm = 
  NonlinearModelFit[indataAmp, {famp[t], x1 > 0}, {x1, y1, z1}, {t}, 
   Weights -> 1/Join[refraAmp]^2, ConfidenceLevel -> 0.95, 
   VarianceEstimatorFunction -> (Mean[#^2] &)];
{x1, y1, z1} = {x1, y1, z1} /. ampnlm["BestFitParameters"];

bac[t_] := Cos [Pi (t - dt1 - fib)/(2 jb)]^2;
csm[t_] := Cos [Pi (t - dt1)/(2 jb)]^2;
cc[x_, t_] = (bac[t] + a1 bac[t] - bac[t] E^(-(1/2) a1 (-1 + x)^2))/
   a1 - (bac[t] Sqrt[\[Pi]/
    2] (-1 + x) Erf[(Sqrt[a1] (-1 + x))/Sqrt[2]])/Sqrt[a1];
bc[t_] = (beta (1 - nu) csm[t - fia] + beta nu cc[0, t - fia]) /. 
   a1 -> 2 av[t];
dt = 0.1; Clear[t];
res = Table[{t,
    {nu, beta, fia} /. 
     FindRoot[{2 av[t] == bc[t], 2 av[t + dt] == bc[t + dt], 
       2 av[t - dt] == bc[t - dt]}, {{nu, .3, 0.1, 1}, {beta, 5, 1, 
        10}, {fia, 3, 0, 6}}]}, {t, 0, 24, .1}]//Quiet;
        tabnu = Table[res[[i]][[2]][[1]], {i, 1, Length[res], 1}];
tabbeta = Table[res[[i]][[2]][[2]], {i, 1, Length[res], 1}];
tabfia = Table[res[[i]][[2]][[3]], {i, 1, Length[res], 1}];
{Mean[tabnu],StandardDeviation[tabnu]}
{Mean[tabbeta],StandardDeviation[tabbeta]}
{Mean[tabfia],StandardDeviation[tabfia]} 
