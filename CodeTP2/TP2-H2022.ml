(*********************************************************************)
(* Langages de Programmation: IFT 3000 NRC 15997                     *)
(* TP1 HIVER 2022. Date limite: Mardi 19 avril à 17h                 *)
(* Implanter un système d'indicateurs de développement               *)
(* en utilisant les données ouvertes de la banque mondiale           *)
(*********************************************************************)
(*********************************************************************)
(* Étudiant(e):                                                      *)
(* NOM: _______________________ PRÉNOM:_____________________________ *)
(* MATRICULE: _________________ PROGRAMME: _________________________ *)
(*                                                                   *)
(*********************************************************************)

(* Chargement de modules, fonctions et librairies utiles pour le TP2 *)
#use "utiles.ml";;

(* Chargement de la signature du TP2 *)
#use "TP2-SIG-H2022.mli";;

(********************************************************************)
(* Implantation du système en utilisant                             *)
(* la programmation orientée objet                       	    *)
(********************************************************************)

(* Module du TP *)

module Tp2h22 : TP2H22 = struct

  (* Classes du TP *)

  class indicateur (lch:string list) =
    object(self)
      val nom_pays : string = nth lch 0
      val code_pays : string = nth lch 1
      val nom_indicateur : string = nth lch 2
      val valeurs_annees : (int * float) list =
        if (length lch) < 4 then failwith "La longueur de la liste est incorrecte"
        else let rl = tl(tl(tl(tl lch))) in
          mapi (fun i x -> if (x="") then (i+1960,-1.0) else (i+1960,float_of_string x)) rl
      val code_indicateur : string = nth lch 3

      method get_nom_pays = nom_pays
      method get_code_pays = code_pays
      method get_nom_indicateur = nom_indicateur
      method get_valeurs_annees = valeurs_annees
      method get_code_indicateur = code_indicateur

      (* Méthode à implanter *)

      (* afficher_indicateur : unit *)
      (* -- À IMPLANTER (6 PTS) ------------------------------------------*)
      (* @Méthode : afficher_indicateur : unit                            *)
      (* @Description : afficher un indicateur selon un certain formatage *)
      method afficher_indicateur =
       (* A corriger *)
	   ()
    end


  class sysindicateurs (od:string) (td:string) =
    object
	val origine_donnees : string = od
	val theme_donnees : string = td
	method get_origine_donnees = origine_donnees
	method get_theme_donnees = theme_donnees
    end


  class sysind_education (od:string) (td:string) =
    object(self)
      inherit sysindicateurs od td as parent
      val mutable map_indicateurs : indicateur IndicateursMap.t ref = ref (IndicateursMap.empty)
      method get_map_indicateurs = map_indicateurs
      method set_map_indicateurs (mi: indicateur IndicateursMap.t ref) = map_indicateurs <- mi

      method ajouter_indicateur (lch: string list) =
        map_indicateurs := IndicateursMap.add (nth lch 1,nth lch 3) (new indicateur lch) !map_indicateurs

      method ajouter_liste_indicateurs (llch:string list list) =
        iter (fun lch -> (self#ajouter_indicateur lch)) llch

      method charger_donnees (fichier:string) =
        let ic =  try open_in fichier with _ -> failwith "Fichier inacessible" in
        let _ = input_line ic in (* ignorer la première ligne *)
        let liste_lignes = lire_fichier ic ";" in
        close_in ic; map_indicateurs := IndicateursMap.empty; self#ajouter_liste_indicateurs liste_lignes

      method retourner_donnees : indicateur list list =
        let li = IndicateursMap.bindings !map_indicateurs in
        let lp = uniques(map (fun (k,i) -> fst k) li) in
        map (fun p -> IndicateursMap.fold (fun _ i acc -> acc@[i])
           (IndicateursMap.filter (fun (x,y) ind -> x = p) !map_indicateurs) []) lp

      method indicateur_existe (ind:indicateur)  =
        IndicateursMap.exists (fun k d -> d = ind) !map_indicateurs

      method retourner_nbr_indicateurs =
        IndicateursMap.cardinal !map_indicateurs

      (* Méthodes à implanter *)

      (* -- À IMPLANTER (6 PTS) --------------------------------------------*)
      (* @Méthode : retourner_indicateur : string * string -> indicateur    *)
      (* @Description : retourner un indicateur se trouvant dans le map     *)

      method retourner_indicateur (cle:string*string) =
       (* A corriger *)
       new indicateur ["";"";"";"";""]

      (* -- À IMPLANTER (6 PTS) -----------------------------------------*)
      (* @Méthode : supprimer_indicateur : string * string -> unit       *)
      (* @Description : supprimer un indicateur dans le map              *)

      method supprimer_indicateur (cle:string*string) =
       (* A corriger *)
	   ()

      (* -- À IMPLANTER (6 PTS) -------------------------------------------------*)
      (* @Méthode : supprimer_liste_indicateurs : (string * string) list -> unit *)
      (* @Description : supprimer une liste d'indicateurs dans le map            *)
      method supprimer_liste_indicateurs (lcles:(string*string) list) =
       (* A corriger *)
	   ()

      (* -- À IMPLANTER (6 PTS) ------------------------------------------*)
      (* @Méthode : afficher_indicateurspays : indicateur list -> unit    *)
      (* @Description : afficher les indicateurs d'un pays                *)

      method afficher_indicateurspays (ip:indicateur list) =
       (* A corriger *)
	   ()

      (* -- À IMPLANTER (7 PTS) -------------------------------------------------*)
      (* @Méthode : afficher_valeur_indicateur : indicateur -> int -> unit       *)
      (* @Description : afficher la valeur d'un indicateur pour une année donnée *)
      (* Doit afficher: "donnee manquante" si la valeur n'existe pas (= -1.0)    *)

      method afficher_valeur_indicateur (ind:indicateur)(annee:int)=
       (* A corriger *)
	   ()

      (* -- À IMPLANTER (7 PTS) --------------------------------------------*)
      (* @Méthode : retourner_indicateurs_pays : string -> indicateur list  *)
      (* @Description : retourner les données d'un pays sous la forme       *)
      (*                de liste d'indicateurs à partir du Map              *)

      method retourner_indicateurs_pays (cp:string): indicateur list =
       (* A corriger *)
       [new indicateur ["";"";"";"";""]]

      initializer print_string ("Recherche dans " ^ (parent#get_origine_donnees) ^
				" pour le theme de " ^ (self#get_theme_donnees) ^ ".");
				print_newline();

    end

  class app_sysindicateurs (nf:string) (i:bool) =
    object(self)
      val nom_fichier = nf
      val interface = i

      method private retourner_chi (i:indicateur) =
        let s = "Code de l'indicateur: " ^ i#get_code_indicateur ^ ".\n" in
        let s = s ^ "Nom de l'indicateur: " ^ i#get_nom_indicateur ^ ".\n" in
        let s = s ^ "Code du pays: " ^ i#get_code_pays ^ ".\n" in
        let s = s ^ "Nom du pays: " ^ i#get_nom_pays ^ ".\n" in
        let s = s ^ "\nValeurs de l'indicateur selon les années:\n\n" in
        let vi = i#get_valeurs_annees in
        let c = fold_left (fun accu paire -> (string_of_int (fst paire)) ^ ": "
                                             ^ (string_of_float (snd paire)) ^ "\n" ^ accu) "" vi in
        s ^ c

      (* Méthodes à implanter *)

      (* -- À IMPLANTER (6 PTS) ------------------------------------------------*)
      (* @Méthode : sauvegarder_indicateur : indicateur -> out_channel -> unit  *)
      (* @Description : écrire les informations d'un indicateur sur un flux     *)

      method sauvegarder_indicateur (i:indicateur) (flux:out_channel) =
       (* A corriger *)
	   ()

      (* -- À IMPLANTER (15 PTS) -----------------------------------------------*)
      (* @Méthode : lancer_systeme_indicateurs : unit                           *)
      (* @Description : Lancer le syteme d'indicateurs                          *)

      method lancer_systeme_indicateurs =
       (* A corriger *)
	   ()

      (* -- À IMPLANTER (25 PTS) -----------------------------------------------*)
      (* @Méthode : lancer_interface_sindicateurs : unit                        *)
      (* @Description : affiche une interface graphique                         *)

      method lancer_interface_sindicateurs =
	(* À compléter *)
	let si = new sysind_education "les donnees ouvertes de la banque mondiale" "l'education" in
        print_string "Chargement des donnees en cours ...\n";
        flush stdout;
        let _,_ = timeRun si#charger_donnees nom_fichier in
        (* ... *)
        let top = openTk () in
	Wm.title_set top "Système d'indicateurs";
        Wm.geometry_set top "680x580";
        let l1 = Label.create ~text:"Bienvenue a l'outil de recherche d'indicateurs" top in
        pack [l1];
        (* ... *)
        let _ = Printexc.print mainLoop () in
	print_endline "Merci et au revoir!\n"


      initializer if interface then self#lancer_interface_sindicateurs else self#lancer_systeme_indicateurs

    end

end
