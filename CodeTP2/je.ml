(***************************************************************************)
(* Jeu d'essai pour TP2 - HIVER 2022                                       *)
(***************************************************************************)

(* On charge le fichier ml du Tp après avoir implanté
les méthodes demandées pour realiser les tests  *)
#use "TP2-H2022.ml";;

(* Résultat:
module type TP2H22 =
  sig
    class indicateur :
      string list ->
      object
        val code_indicateur : string
        val code_pays : string
        val nom_indicateur : string
        val nom_pays : string
        val valeurs_annees : (int * float) list
        method afficher_indicateur : unit
        method get_code_indicateur : string
        method get_code_pays : string
        method get_nom_indicateur : string
        method get_nom_pays : string
        method get_valeurs_annees : (int * float) list
      end
    class sysindicateurs :
      string ->
      string ->
      object
        val origine_donnees : string
        val theme_donnees : string
        method get_origine_donnees : string
        method get_theme_donnees : string
      end
    class sysind_education :
      string ->
      string ->
      object
        val mutable map_indicateurs : indicateur IndicateursMap.t ref
        val origine_donnees : string
        val theme_donnees : string
        method afficher_indicateurspays : indicateur list -> unit
        method afficher_valeur_indicateur : indicateur -> int -> unit
        method ajouter_indicateur : string list -> unit
        method ajouter_liste_indicateurs : string list list -> unit
        method charger_donnees : string -> unit
        method get_map_indicateurs : indicateur IndicateursMap.t ref
        method get_origine_donnees : string
        method get_theme_donnees : string
        method indicateur_existe : indicateur -> bool
        method retourner_donnees : indicateur list list
        method retourner_indicateur : string * string -> indicateur
        method retourner_indicateurs_pays : string -> indicateur list
        method retourner_nbr_indicateurs : int
        method set_map_indicateurs : indicateur IndicateursMap.t ref -> unit
        method supprimer_indicateur : string * string -> unit
        method supprimer_liste_indicateurs : (string * string) list -> unit
      end
    class app_sysindicateurs :
      string ->
      bool ->
      object
        val interface : bool
        val nom_fichier : string
        method lancer_interface_sindicateurs : unit
        method lancer_systeme_indicateurs : unit
        method sauvegarder_indicateur : indicateur -> out_channel -> unit
      end
  end
module Tp2h22 : TP2H22
*)

(* On ouvre le module du TP *)
open Tp2h22;;

(* On exécute maintenant les méthodes une à une *)

let si = new sysind_education "les donnees ouvertes de la banque mondiale" "l'education";;

(* Résultat:
Recherche dans les donnees ouvertes de la banque mondiale pour le theme de l'education.
val si : Tp2e16.sysind_education = <obj>
*)

let ind1 = new indicateur ["Canada";
                          "CAN";
                          "Population agee de 15 à 64 ans";
                          "SSP.POP.1564.TO.ZS";
                          "58.6";"58.5";"";"";"";"65.4";"";"";"";"";"";
                          "66.2";"";"";"";"";"";"68.1"];;

(* Résultat:
val ind : Tp2e16.indicateur = <obj>
*)

ind1#afficher_indicateur;;

(* Résultat:
Code de l'indicateur: SSP.POP.1564.TO.ZS.
Nom de l'indicateur: Population agee de 15 à 64 ans.
Code du pays: CAN.
Nom du pays: Canada.
- : unit = ()
*)

let ind2 = new indicateur ["France";
                            "FRA";
                            "Ratio filles/garcons des inscriptions au primaire";
                            "SE.ENR.PRIM.FM.ZS"; 
                            "";"";"";"";"";"1.00414";"";"";"";"";"";
                            "0.99473";"";"";"";"";"";"0.986241"];;

(* Résultat:
val ind2 : Tp2e16.indicateur = <obj>
*)

si#afficher_indicateurspays [ind1;ind2];;

(* Résultat:
Code de l'indicateur: SSP.POP.1564.TO.ZS.
Nom de l'indicateur: Population agee de 15 à 64 ans.
Code du pays: CAN.
Nom du pays: Canada.

Code de l'indicateur: SE.ENR.PRIM.FM.ZS.
Nom de l'indicateur: Ratio filles/garcons des inscriptions au primaire.
Code du pays: FRA.
Nom du pays: France.

- : unit = ()
*)


si#ajouter_indicateur ["Canada";
                       "CAN";
                       "Population agee de 15 à 64 ans";
                       "SSP.POP.1564.TO.ZS";
                       "58.6";"58.5";"";"";"";"65.4";"";"";"";"";"";
                       "66.2";"";"";"";"";"";"68.1"];;

(* Résultat:
- : unit = ()
*)

let ni = si#retourner_nbr_indicateurs;;

(* Résultat:
val ni : int = 1
*)

si#ajouter_liste_indicateurs [["France";
                               "FRA";
                               "Ratio filles/garcons des inscriptions au primaire";
                               "SE.ENR.PRIM.FM.ZS";
                               "";"";"";"";"";"1.00414";"";"";"";"";"";
                               "0.99473";"";"";"";"";"";"0.986241"];
                              ["Argentine";
                               "ARG";
                               "Total des inscriptions a l'ecole primaire (%net)";
                               "SE.PRM.TENR";
                               "";"";"";"99.15396";"";"";"";"";"";"";"";
                               "99.37068";"";"";"";"";"99.85297";""]];;

(* Résultat:
- : unit = ()
*)

let ni = si#retourner_nbr_indicateurs;;

(* Résultat:
val ni : int = 3
*)

si#supprimer_indicateur ("CAN","SSP.POP.1564.TO.ZS");;

(* Résultat:
- : unit = ()
*)

let ni = si#retourner_nbr_indicateurs;;

(* Résultat:
val ni : int = 2
*)


si#supprimer_liste_indicateurs [("FRA","SE.ENR.PRIM.FM.ZS"); 
                              ("ARG","SE.PRM.TENR")];;

(* Résultat:
- : unit = ()
*)

let ni = si#retourner_nbr_indicateurs;;

(* Résultat:
val ni : int = 0
*)

si#charger_donnees "data.csv";;


(* Résultat:
- : unit = ()
*)

let ni = si#retourner_nbr_indicateurs;;

(* Résultat:
val ni : int = 35424
*)

si#retourner_donnees;;

(* Résultat:
- : Tp2e16.indicateur list list =
[[<obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
  <obj>; <obj>; <obj>; <obj>];
 [<obj>; <obj>; <obj>; <obj>; ...]; ...]
*)

let ind = si#retourner_indicateur ("FRA","SE.ENR.PRIM.FM.ZS");;

(* Résultat:
val ind : Tp2e16.indicateur = <obj>
*)


si#afficher_valeur_indicateur ind 2012;;

(* Résultat:
Valeur pour l'annee 2012: 0.99933
- : unit = ()
*)

let lp = si#retourner_indicateurs_pays "ARG";;
let np = List.length lp;;

(* Résultat:
val lp : Tp2e16.indicateur list =
  [<obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>]
val np : int = 144
*)

let asi = new app_sysindicateurs "data.csv" false;;

(* Résultat:
Recherche dans les donnees ouvertes de la banque mondiale pour le theme de l'education.
Chargement des donnees en cours ...
Chargement termine dans un temps de: 3.906291008 secondes
Veuillez entrer le code du pays qui vous interesse:
DEU
;;
Nombre d'indicateurs trouves pour ce pays: 144
Veuillez entrer le numero de l'indicateur dont vous voulez sauvegarder (de 1 a 144)
15
;;

Veuillez consulter le fichier 'Resultats.txt' dans votre repertoire courant!

Merci et au revoir!
val asi : Tp2e16.app_sysindicateurs = <obj>
*)

let asi = new app_sysindicateurs "data.csv" true;;

(* Résultat:
Recherche dans les donnees ouvertes de la banque mondiale pour le theme de l'education.
Chargement des donnees en cours ...
Chargement termine dans un temps de: 4.09433102608 secondes
Merci et au revoir!

val asi : Tp2e16.app_sysindicateurs = <obj>
Affichage d'une fenêtre graphique.
*)



(***************************************)
(* Verification des messages d'erreurs *)
(***************************************)

try
  ignore (new indicateur ["";"";""])
with
  Failure s -> print_endline s;;

(* Résultat:
La longueur de la liste est incorrecte
- : unit = ()
*)

try
  ignore (si#charger_donnees "existe_pas.cvs")
with
  Failure s -> print_endline s;;

(* Résultat:
Fichier inacessible
- : unit = ()
*)

try
  ignore (si#retourner_indicateur ("",""))
with
  Not_found -> print_endline "l'indicateur n'existe pas";;

(* Résultat:
l'indicateur n'existe pas
- : unit = ()
*)

let asi = new app_sysindicateurs "API_4_DS2_fr_csv_v2.csv" false;;

(* Résultat:
Recherche dans les donnees ouvertes de la banque mondiale pour le theme de l'education.
Chargement des donnees en cours ...
Chargement termine dans un temps de: 5.03168106079 secondes
Veuillez entrer le code du pays qui vous interesse:
EXP
;;
Aucune donnee trouvee, veuillez verifier le code du pays
val asi : Tp2e16.app_sysindicateurs = <obj>
*)
