(*Func que lee archivo y inserta en lista *)
fun readlist (file : string) = let
  val sni = TextIO.openIn file
  fun loop sni =
   case TextIO.inputLine sni of
      SOME line => line :: loop sni
    | NONE      => []
in
  loop sni before TextIO.closeIn sni
end

(*Func que toma la lista y la explota en lista de char*)
fun explode_list (result: char list , list: string list )=
    if null list
    then result
    else explode_list(result@explode(hd list),tl list)

(*Func que convierte char -> int*)
fun chartoInt (x:char)= Option.getOpt(Int.fromString(str(x)),0 )
fun strtoInt (x:string)= Option.getOpt(Int.fromString(x),0)

(*Func que busca char en lista y convierte a int*)
fun int_in_list (lista: char list)=
    if null lista
    then 0
    else
	if Char.isDigit(hd lista)
	then chartoInt(hd lista)
	else int_in_list(tl lista)

(*Func que busca fechas*)

fun buscar (res:int list list, temp: int list, t_num:string, lista: char list, c: int  )=
    if null lista
    then res
    else
	if  c=10
        then buscar(rev(temp)::res,[],"",lista,0 )
	else
	    if Char.isDigit(hd lista)
	    then
		if c=0 orelse c=3 orelse c=6
		then buscar(res, temp, str(hd lista),tl lista,c+1 )
		else
		    if c=1 orelse c=4 orelse c=9
		    then buscar(res,strtoInt(t_num^str( hd lista))::temp,"",tl lista,c+1 )
		    else
			if c=7 orelse c=8
			then buscar(res,temp,t_num^str(hd lista),tl lista,c+1 )		    
			else buscar(res,[],"",tl lista,0)
	    else
		if hd lista = #"/"
		then
		    if c=2 orelse c=5
		    then buscar(res,temp,"",tl lista, c+1 )
		    else buscar(res,[],"",tl lista, 0)
		else
		    if c=8
		    then buscar(res,strtoInt(t_num)::temp,"", tl lista,10)
		    else buscar(res,[],"",tl lista,0 )


(* List -> Tupla  *)

fun listuple (a,b,c,lista)=
    if null lista
    then (a,b,c)
    else 
	if length lista = 3
	then listuple(hd lista,b,c,tl lista)
	else
	    if length lista = 2
	    then listuple(a, hd lista,c,tl lista)
	    else listuple(a,b,hd lista,tl lista)


(*LIsta de listas -> LIsta de TUplas  *)

fun list2tuple (res,lista)=
    if null lista
    then res
    else list2tuple(listuple(0,0,0,hd lista)::res,tl lista )



(*conjunto de meses*)
val meses = ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre"];

(*Funcion que se encarga de identifcar los meses*)
fun get_month (strings: string list, n: int):string =
  if null(strings) then ""
  else if n = 1 then hd(strings)
  else get_month(tl(strings), n-1);

(*Funcion que se encarga de transformar la fecha en el formato correspondienda y convertirla a String*)
fun dateToString (fecha: int*int*int):string =  Int.toString(#1 fecha) ^ " de "  ^ get_month(meses, (#2 fecha))^" del " ^ Int.toString(#3 fecha);

fun constring (L1 :  string list, L2 : string list) =
    if null L1
    then L2
    else
    hd (L1) :: constring (tl(L1), L2);


fun conv_aux ( lista : (int*int*int) list, hilera : string list ) =
    if null lista
    then hilera
    else
    conv_aux( tl lista, constring( hilera , [ dateToString(hd lista)] ) );


(*Funcion que recibe la lista de tuplas con las fechas *)
fun convertirFechas ( lista : (int*int*int) list ) = 
    if null lista
    then ["NO FECHAS"]
    else 
    conv_aux(lista,[]);

(*Funcion que ejecuta las otras funciones*)
fun main(dir)=
  convertirFechas( list2tuple([], buscar([],[],"",explode_list([],readlist(dir)),0)))

