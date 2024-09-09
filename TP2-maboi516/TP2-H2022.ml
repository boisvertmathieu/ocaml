(*********************************************************************)
(* Langages de Programmation: IFT 3000 NRC 15997                     *)
(* TP1 HIVER 2022. Date limite: Mardi 19 avril à 17h                 *)
(* Implanter un système d'indicateurs de développement               *)
(* en utilisant les données ouvertes de la banque mondiale           *)
(*********************************************************************)
(*********************************************************************)
(* Étudiant(e):                                                      *)
(* NOM: Boisvert_______________ PRÉNOM: Mathieu_____________________ *)
(* MATRICULE: maboi516_________ PROGRAMME: BAC Informatique_________ *)
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
      method afficher_indicateur  =
        print_string ("Code de l'indicateur: " ^ code_indicateur ^ ".\n");
        print_string ("Nom de l'indicateur: " ^ nom_indicateur ^ ".\n");
        print_string ("Code du pays: " ^ code_pays ^ ".\n");
        print_string ("Nom du pays: " ^ nom_pays ^ ".\n")
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
        IndicateursMap.find (cle) !map_indicateurs


      (* -- À IMPLANTER (6 PTS) -----------------------------------------*)
      (* @Méthode : supprimer_indicateur : string * string -> unit       *)
      (* @Description : supprimer un indicateur dans le map              *)

      method supprimer_indicateur (cle:string*string) =
        map_indicateurs := IndicateursMap.remove cle !map_indicateurs

      (* -- À IMPLANTER (6 PTS) -------------------------------------------------*)
      (* @Méthode : supprimer_liste_indicateurs : (string * string) list -> unit *)
      (* @Description : supprimer une liste d'indicateurs dans le map            *)
      method supprimer_liste_indicateurs (lcles:(string*string) list) =
        iter (fun cle -> self#supprimer_indicateur cle) lcles

      (* -- À IMPLANTER (6 PTS) ------------------------------------------*)
      (* @Méthode : afficher_indicateurspays : indicateur list -> unit    *)
      (* @Description : afficher les indicateurs d'un pays                *)

      method afficher_indicateurspays (ip:indicateur list) =
        iter (fun ind -> ind#afficher_indicateur; print_newline()) ip

      (* -- À IMPLANTER (7 PTS) -------------------------------------------------*)
      (* @Méthode : afficher_valeur_indicateur : indicateur -> int -> unit       *)
      (* @Description : afficher la valeur d'un indicateur pour une année donnée *)
      (* Doit afficher: "donnee manquante" si la valeur n'existe pas (= -1.0)    *)

      method afficher_valeur_indicateur (ind:indicateur)(annee:int)=
        print_string ("Valeur pour l'annee " ^ (string_of_int annee ) ^ ": " 
                      ^ (let v = (assoc annee ind#get_valeurs_annees) in 
                         if (v = -1.0) then "Donnee manquante." else (string_of_float v)) ^ "\n")

      (* -- À IMPLANTER (7 PTS) --------------------------------------------*)
      (* @Méthode : retourner_indicateurs_pays : string -> indicateur list  *)
      (* @Description : retourner les données d'un pays sous la forme       *)
      (*                de liste d'indicateurs à partir du Map              *)

      method retourner_indicateurs_pays (cp:string): indicateur list =
        IndicateursMap.fold (fun _ i acc -> acc @ [i]) 
          (IndicateursMap.filter (fun (x,y) ind -> x = cp) !map_indicateurs) []

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
        let chaine = self#retourner_chi i in
        output_string flux chaine

      (* -- À IMPLANTER (15 PTS) -----------------------------------------------*)
      (* @Méthode : lancer_systeme_indicateurs : unit                           *)
      (* @Description : Lancer le syteme d'indicateurs                          *)

      method lancer_systeme_indicateurs =
        let si = new sysind_education "les donnees ouvertes de la banque mondiale" "l'education" in
        print_string "Chargement des donnees en cours ...\n";
        flush stdout;
        let _,tr = timeRun si#charger_donnees nom_fichier in
        print_string ("Chargement termine dans une temps de: " ^ (string_of_float tr) ^ " secondes\n");
        print_string "Veuille entrer le code du pays qui vous interesse:\n";
        flush stdout;
        let choix = read_line () in 
        let inds = si#retourner_indicateurs_pays choix in
        if (inds = []) then print_string "Aucune donnee trouvee, veuillez verifier le code du pays" else 
          let nb = string_of_int (length inds) in
          print_string ("Nombre d'indicateurs trouves pour ce pays : " ^ nb ^ "\n");
          print_string ("Veuillez entrer le numero de l'indicateur que vous voulez sauvegarder (1 a " ^ nb ^ ")\n");
          flush stdout;
          let i = read_int() in
          let fichier = open_out "Resultats.txt" in
          self#sauvegarder_indicateur (nth inds (i-1)) (fichier);
          close_out fichier;
          print_string "\nVeuillez consulter le fichier 'Resultats.txt' dans votre repertoire courant!\n\n";
          print_string "Merci et au revoir!\n"

      (* -- À IMPLANTER (25 PTS) -----------------------------------------------*)
      (* @Méthode : lancer_interface_sindicateurs : unit                        *)
      (* @Description : affiche une interface graphique                         *)

      method lancer_interface_sindicateurs =
        let si = new sysind_education "les donnees ouvertes de la banque mondiale" "l'education" in
        print_string "Chargement des donnees en cours ...\n";
        flush stdout;
        let _,tr = timeRun si#charger_donnees nom_fichier in
        print_string ("Chargement termine dans un temps de " ^ (string_of_float tr) ^ " secondes\n");

        let top = openTk () in
        Wm.title_set top "Système d'indicateurs";
        Wm.geometry_set top "680x580";
        let l1 = Label.create ~text:"Bienvenue a l'outil de recherche d'indicateurs" top in
        pack [l1];

        (* Création des labels *)
        (* Création du label permettant d'afficher l'indicateur sélectionné *)
        let indTxtVar = Textvariable.create() in
        Textvariable.set indTxtVar "";
        let indLbl1 = Label.create ~textvariable: indTxtVar ~background:(`Color "#FFFFD4") top in
        let indLbl2 = Label.create ~text: "Indicateur selectionné:" top in

        (* Création du label permettant d'afficher le pays sélectionné *)
        let paysTxtVar = Textvariable.create() in
        Textvariable.set paysTxtVar "";
        let paysLbl1 = Label.create ~textvariable: paysTxtVar ~background:(`Color "#FFFFD4") top in
        let paysLbl2 = Label.create ~text: "Pays selectionné:" top in 

        (* Récupération des données *)
        let bindings = IndicateursMap.bindings !(si#get_map_indicateurs) in
        (* On récupère les pays et les indicateurs dans un format permettant l'affichage *)
        let inds = uniques(map (fun (k,i) -> i#get_code_indicateur ^ ": " ^ i#get_nom_indicateur) bindings) in
        let pays = uniques(map (fun (k,i) -> (fst k) ^ ": " ^ i#get_nom_pays) bindings) in

        (* Création des listboxes permettant l'affichage des pays et des indicateurs *)
        (* Création du premier listbox contenant les pays *)
        let lstBoxPays = Listbox.create ~selectmode:`Single top in
        Listbox.insert ~index:`End ~texts:pays lstBoxPays;

        (* Création du deuxième listbox contenant les indicateurs *)
        let lstBoxInds = Listbox.create ~selectmode:`Single top in
        Listbox.insert ~index:`End ~texts:inds lstBoxInds;

        (* Définissions d'une fonction exécuté lors du click sur un élément des listboxes *)
        let set_val_pays ev = 
          match Listbox.curselection lstBoxPays with
          | [] -> ()
          | index :: _ -> Textvariable.set paysTxtVar (Listbox.get lstBoxPays index)
        in
        let set_val_inds ev = 
          match Listbox.curselection lstBoxInds with
          | [] -> ()
          | index :: _ -> Textvariable.set indTxtVar (Listbox.get lstBoxInds index)
        in

        (* Ajout de events lors du click sur les listboxes *)
        bind ~events: [`ButtonReleaseDetail 1] ~action: set_val_pays lstBoxPays;
        bind ~events: [`ButtonReleaseDetail 1] ~action: set_val_inds lstBoxInds;

        (* Création du bouton Afficher Pays *)
        let b1 = Button.create ~text:"Afficher le pays" top in
        (* Création du bouton Afficher Indicateur *)
        let b2 = Button.create ~text:"Afficher l'indicateur" top in
        (* Création du bouton permettant l'affichage des résultats *)
        let b3 = Button.create ~text:"Afficher les résultats"
            ~command:(fun () -> 
                (* Création d'une nouvelle fenêtre fille de la fenêtre principale *)
                let w  = Toplevel.create top in
                begin
                  Wm.title_set w "Résultat";
                  Wm.geometry_set w "680x580";
                  try
                    (* Récupération de l'indicateur sélectionné *)
                    let indSelectionne = Textvariable.get indTxtVar in
                    let codeInd = nth (String.split_on_char ':' indSelectionne) 0 in
                    (* Récupération du pays sélectionné *)
                    let paysSelectionne = Textvariable.get paysTxtVar in
                    let codePays = nth (String.split_on_char ':' paysSelectionne) 0 in
                    (* Construction de la clé de recherche et recherche de l'indicateur correspondant à cette clé dans le map indicateurs *)
                    let cle = (codePays, codeInd) in
                    let ind = IndicateursMap.find (cle) !(si#get_map_indicateurs) in
                    (* On sépare chacune des lignes en éléments d'une liste afin d'afficher le résultat dans un listbox *)
                    (* C'est la seule manière que j'ai pu trouver pour imprimer dans le même format que l'énoncé les détails d'un indicateur *)
                    let resultat = String.split_on_char '\n' (self#retourner_chi ind) in 

                    let scrl = Scrollbar.create w in
                    let l = Listbox.create ~width:100 ~height:50 ~yscrollcommand:(Scrollbar.set scrl) w in
                    Listbox.insert ~texts:resultat ~index:`End l;
                    Scrollbar.configure ~command:(Listbox.yview l) scrl;
                    pack ~side:`Left ~fill:`X [coe scrl; coe l];
                  with _ -> (print_endline "Erreur lors de la récupération du pays et de l'indicateur sélectionné"; flush stdout)
                end
              ) top
        in
        pack [lstBoxPays;lstBoxInds] ~fill:`X;
        pack [paysLbl1;paysLbl2];
        pack [indLbl1;indLbl2];
        pack [b1;b2;b3] ~side:`Bottom ~fill:`X;

        let _ = Printexc.print mainLoop () in
        print_endline "Merci et au revoir!\n"

      initializer if interface then self#lancer_interface_sindicateurs else self#lancer_systeme_indicateurs
    end
end
