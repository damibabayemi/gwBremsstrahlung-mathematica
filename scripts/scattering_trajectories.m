(* ::Package:: *)

(* ::Title:: *)
(*scattering trajectories*)


(* ::Section:: *)
(*numerically integrate trajectories based on Hamiltonian*)


(* ::Subsection:: *)
(*nonspinning 1PM Hamiltonian*)


(* ::Text:: *)
(*from https://arxiv.org/abs/1808.02489 (extension to spin in https://arxiv.org/abs/2005.03071 and https://arxiv.org/abs/2102.10137 )*)


x = { x1[t], x2[t], x3[t] };
p = { p1[t], p2[t], p3[t] };


abbrev = { u -> M / r, M -> m1 + m2, \[Nu] -> m1 m2 / M^2, \[Mu] -> m1 m2 / M, r -> Sqrt[x . x] };
pSq = p . p / \[Mu]^2;
E1 = Sqrt[m1^2 / \[Mu]^2 + pSq];
E2 = Sqrt[m2^2 / \[Mu]^2 + pSq];
Etot = E1 + E2;
\[Xi] = E1 E2 / Etot^2;
p1p2 = E1 E2 + pSq;


c1 = 1 / (Etot^2 \[Xi]) ( m1^2 m2^2 / \[Mu]^4 - 2 p1p2^2 );
c2 = 1 / (32 Etot^2 \[Xi]) (
		2 Etot (\[Xi]-1) c1^2 - 16 Etot p1p2 c1 + 3 / \[Nu] (m1^2 m2^2 / \[Mu]^4 - 5 p1p2^2)
	);

(* 2PM part dropped *)
V = c1 \[Nu] u + 0 8 c2 \[Nu]^2 u^2;
H[parameters___] := \[Mu] (Etot + V) //. abbrev /. Flatten[{parameters}];


H[]


(* ::Subsection:: *)
(*equations of motion*)


(* vector derivative *)
vD[e_, v_] := D[e, #] & /@ v;


(* Hamilton's equations in sperical coordinates *)
eom = {
	D[p, t] == - vD[H[], x],
	D[x, t] == vD[H[], p]
};


(* ::Subsection:: *)
(*trajectories*)


(* numerically solve Hamilton's equations *)
scatteringTrajectory[b_?NumericQ, \[Gamma]_?NumericQ, m1n_?NumericQ, m2n_?NumericQ, x0_:10^4] := Module[
{
	pinf = m1n m2n Sqrt[\[Gamma]^2-1] / Sqrt[m1n^2 + m2n^2 + 2 \[Gamma] m1n m2n],
	params = { m1 -> m1n, m2 -> m2n },
	M = m1n + m2n,
	E1, E2, E,
	initial, sol, sol1, sol2, t0
},
    (* initial conditions *)
	initial = { x == {x0 M, b, 0}, p == {-pinf, 0, 0} } /. t -> 0;

	(* numerical solution, save t0 ar time of closest approach *)
	sol = NDSolve[Join[eom, initial] /. params, Join[x, p], {t, 0, 10 x0 M},
		 Method -> {"EventLocator", "Event" -> x . p == 0, "EventAction" :> (t0 = t)}];
	(* shift time by t0 *)
	sol = sol /. InterpolatingFunction[d___][t] :> InterpolatingFunction[d][t + t0] // First;
	
	(* compute the two trajectories from the relative sepatation, assuming center of mass system *)
	E1 = Sqrt[m1n^2 + pSq];
	E2 = Sqrt[m2n^2 + pSq];
	E = E1 + E2;
	sol1 = - E2/E x /. sol;
	sol2 = E1/E x /. sol;

	(* return solutions *)
	{ Thread[x -> sol1], Thread[x -> sol2], Prepend[sol[[1,2,0,1,1]] - t0, t]}
];


(* compute an example trajectory *)
params = {b -> 100, \[Gamma] -> 2, m1 -> 1, m2 -> 1};
solution = scatteringTrajectory @@ ({b, \[Gamma], m1, m2} /. params);


x2D = Drop[x, -1];


(* have a look at the two trajectories *)
ParametricPlot[{x2D /. solution[[1]], x2D /. solution[[2]]}, {t, -3000, 3000}, PlotRange -> 2000]


(* write trajectory to hdf5 file *)
tmax = 3000;
steps = 1000;
range = {t, -tmax, tmax, 2 tmax/steps };
Export["trajectory.h5",
	{
	{b, \[Gamma], m1, m2} /. params,
	Table[t, range // Evaluate],
	Table[x /. solution[[1]], range // Evaluate],
	Table[x /. solution[[2]], range // Evaluate]
	}, 
	{"Datasets", {"meta", "times", "trajectory1", "trajectory2"}}
];
