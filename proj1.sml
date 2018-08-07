val AGL = 330000.00;    (*A1*)
val AGP = 0.020;        (*ag(t)*)
val DGL = 200000.00;    (*A2*)
val p = 1.50;           (*p*)
val ADT = 0.050;        (*a_dT*)
val ATL = 0.075;        (*a_aT*)
val Wmax = 20.00;       (*W_max*)
val DWith = 0.0;          (*W(t)*)
val Da = 300.00;        (*D_a*)
val Ada = 0.05;         (*a_da*)
val Sd = 1.50;          (*S_d*)
val Kd = 0.50;          (*K_d*)
val Aa = 250.00;        (*A_a*)
val Aaa = 0.05;         (*a_aa*)
val Aat = 0.0;          (*a_a_(t) (A7)*)
val Ka = 0.25;          (*K_a*)
val Sa = 1.00;          (*S_a*)
val V = 1200.00;        (*V*)
val L = 47490.00;       (*L*)
val Agt = 0.0;          (*a_g_(t) (A6)*)
val DCAS = 0.0;         (*DCAS*)
val ACAS = 0.0;         (*ACAS*)  
val a = 0.0;             (*a(t) (A3)*) 
val Dleth = 0.0;          (*a_d_(t) (A5)*)  
val day = 2;  
val totalDisplacement = 0.0;  
open Math;

(*A3 calculates Attacker's ground-to-ground lethality attrition*)
fun A3 (DWith, AGP, Wmax) = 
    AGP * (1.0 - (DWith / Wmax));

val a = A3 (DWith, AGP, Wmax);

(*Attacker's ground lethality killed by Defender's CAS*)
fun A10 (L, V, Da, Ada, Sd, Kd) = (
let
	val c = (1.0 - Ada)
	val d = pow(c, Sd)
	val e = Sd + 1.0
	val f = pow (c, e)
in
	(L / V) * Da * d * Kd * (((1.0 - f) / Ada) -1.0)
end);

val DCAS = A10 (L, V, Da, Ada, Sd, Kd);

(*Defender's ground lethality killed by Attacker's CAS*)
fun A11 (L, V, Aa, Aaa, Sa, Ka) = (
let
	val c = (1.0 - Aaa)
	val d = pow(c, Sa)
	val e = Sa + 1.0
	val f = pow (c, e)
in
	(L / V) * Aa * d * Ka * (((1.0 - f) / Aaa) -1.0)
end);

val ACAS = A11 (L, V, Aa, Aaa, Sa, Ka);

(*Surviving defensive aircraft*)
fun A12 (Da, Ada, Sd) = 
let
    val c = (1.0 - Ada)
    val d = pow(c, Sd)
in
    Da * d
end;

val Da = A12 (Da, Ada, Sd);

(*Surviving attacker aircraft*)
fun A13 (Aa, Aaa, Sa) = 
let
    val c = (1.0 - Aaa)
    val d = pow(c, Sa)
in
    Aa * d
end;

val Aa = A13 (Aa, Aaa, Sa);

(*To calculate attackers grond lethatlity survival rate*)
fun A1(AGL,a,DCAS) =
	AGL*(1.0-a)-DCAS; 

val AGL = A1 (AGL, a, DCAS);

(*This function will take*)
(*3 variables, beginning AGL, beginning AGGL,*)
(*and beginning Defender's CAS and return the *)
(*attacker's total ground lethality attrition / day*)
fun A7 (AGL, AGP, L, V, Da, Ada, Sd, Kd) = (
let
    val k = A10(L, V, Da, Ada, Sd, Kd)   
	val v = A1 (AGL, AGP, k)
in
	
	(AGL - v) / AGL
end);

val Aat = A7(AGL, AGP, L, V, Da, Ada, Sd, Kd);

(*This will calculate the defender's ground lethality*)
fun A2(DGL, a, p, AGL, ACAS) = 
    DGL - (a / p) * AGL - ACAS;

val DGL = A2 (DGL, a, p , AGL, ACAS);

(* A6 will determine the attacker's ground prosecution rate / day *)
fun A6 (AGP, ATL, Aat) = 
    AGP - (((ATL - AGP) / ATL) * (Aat - ATL));

val AGP = A6 (AGP, ATL, Aat);

(*A5 calculates Defender's total ground-lethality attrition*)
fun A5 (DGL, a, p, AGL, ACAS) = (
let
    val c = A2 (DGL, a, p, AGL, ACAS)
in  
    (DGL - c) / DGL
end);

val Dleth = A5 (DGL, a, p, AGL, ACAS);

(*A4 calculates the Defender's rate of withdrawal each day*)
fun A4 (DWith, Wmax, ADT, Dleth) = 
if 
    Dleth > ADT
then
    DWith + ((Wmax - DWith) / (1.0 - ADT)) * (Dleth - ADT)
else
    0.0;

val DWith = A4 (DWith, Wmax, ADT, Dleth);



