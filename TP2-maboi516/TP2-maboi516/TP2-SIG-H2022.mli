(*********************************************************************)
(* Langages de Programmation: IFT 3000 NRC 15997                     *)
(* TP1 HIVER 2022. Date limite: Mardi 19 avril à 17h                 *)
(* Implanter un système d'indicateurs de développement               *)
(* en utilisant les données ouvertes de la banque mondiale           *)
(*********************************************************************)
(* Signature du TP2                                                  *)
(*********************************************************************)

module type TP2H22 = sig

  class indicateur : string list ->
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

  class sysindicateurs : string -> string ->
  object
    val origine_donnees : string
    val theme_donnees : string
    method get_origine_donnees : string
    method get_theme_donnees : string
  end

  class sysind_education : string -> string ->
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

  class app_sysindicateurs : string -> bool ->
  object
    val interface : bool
    val nom_fichier : string
    method sauvegarder_indicateur : indicateur -> out_channel -> unit
    method lancer_systeme_indicateurs : unit
    method lancer_interface_sindicateurs : unit
  end

end
