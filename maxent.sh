#!/Applications/Mathematica.app/Contents/MacOS/MathematicaScript

maxentr[pdf1_, pdf2_, norm_] := Block[{n, i, j, p, sol, outer, trace}, 
   outer = Outer[Times, pdf1, pdf2]; trace = Tr[outer]; n = Length[pdf1]; 
    For[i = 1, i <= n, i++, Subscript[p, i, i] = 0]; 
    sol = FindMaximum[{Sum[If[j == i, 0., (-Subscript[p, i, j])*
          Log[Subscript[p, i, j]]], {i, 1, n}, {j, 1, n}], 
       Flatten[{Table[Sum[Subscript[p, k, i], {i, 1, n}] == pdf1[[k]], {k, 1, n}], 
         Table[Sum[Subscript[p, i, k], {i, 1, n}] == pdf2[[k]], {k, 1, n}], 
         Sum[Subscript[p, i, j], {i, 1, n}, {j, 1, n}] == norm, 
         Flatten[Table[Subscript[p, i, j] >= 0, {i, 1, n}, {j, 1, n}]]}]}, 
      DeleteCases[Flatten[Table[{Subscript[p, i, j], outer[[i,j]]/(1 - Tr[outer])}, 
         {i, 1, n}, {j, 1, n}], 1], {x_, _} /; x == 0], MaxIterations -> 30]; 
    Array[Subscript[p, #1, #2] & , {n, n}] /. sol[[2]]]


scl = Rest[$ScriptCommandLine]; 

pdf1 = ToExpression[scl[[1]]]; 
pdf2 = ToExpression[scl[[2]]]; 
norm = ToExpression[scl[[3]]]; 
output = maxentr[pdf1, pdf2, norm]; 
Print[output]