open Printf

(* Fonction pour convertir un caractère en chaîne binaire de 8 bits *)
let char_to_binary_string c =
  let n = Char.code c in
  let rec aux n acc count =
    if count = 8 then acc
    else aux (n lsr 1) ((string_of_int (n land 1)) ^ acc) (count + 1)
  in
  aux n "" 0

(* Fonction pour convertir une chaîne de caractères en chaîne binaire *)
let string_to_binary_string s =
  let rec aux chars acc =
    match chars with
    | [] -> acc
    | c::cs -> aux cs (acc ^ (char_to_binary_string c))
  in
  aux (String.to_seq s |> List.of_seq) ""

(* Fonction pour convertir un entier en chaîne binaire de 8 bits *)
let int_to_binary_string n =
  let rec aux n acc count =
    if count = 8 then acc
    else aux (n lsr 1) ((string_of_int (n land 1)) ^ acc) (count + 1)
  in
  aux n "" 0


(* Fonction pour lire l'entête PPM (P6) et retourner les dimensions et la valeur max de couleur *)
let lire_entete ic =
  ignore (input_line ic);  (* Ignore la ligne de type "P6" *)
  let dimensions = input_line ic in
  let (largeur, hauteur) = Scanf.sscanf dimensions "%d %d" (fun w h -> (w, h)) in
  let max_val = input_line ic in  (* Lit la valeur maximale des couleurs *)
  (largeur, hauteur, max_val)

(* Fonction pour convertir un entier en chaîne binaire de longueur fixe (30 bits ici) *)
let int_to_fixed_length_binary_string_30 n =
  let rec aux n acc count =
    if count = 30 then acc
    else aux (n lsr 1) ((string_of_int (n land 1)) ^ acc) (count + 1)
  in
  aux n "" 0

(* Fonction pour afficher les valeurs RGB d'un pixel en binaire *)
let afficher_pixel ic =
  let r = input_byte ic in
  let g = input_byte ic in
  let b = input_byte ic in
  printf "R: %s, G: %s, B: %s\n" (int_to_binary_string r) (int_to_binary_string g) (int_to_binary_string b)

(* Fonction pour écraser le dernier bit d'un octet avec un bit de message *)
let ecraser_dernier_bit octet bit_message =
  match bit_message with
  | '1' -> octet lor 1   (* Met le dernier bit à 1 *)
  | '0' -> octet land 0xFE (* Met le dernier bit à 0 *)
  | _ -> failwith "Le bit de message doit être '0' ou '1'"


(* Fonction pour convertir un entier en chaîne binaire de longueur fixe *)
let int_to_fixed_length_binary_string n length =
  let binary = int_to_binary_string n in
  String.make (length - String.length binary) '0' ^ binary

(* Fonction pour insérer le message dans l'image *)
let inserer_message chemin_fichier message =
  let ic = open_in_bin chemin_fichier in
  let oc = open_out_bin "output.ppm" in
  let (largeur, hauteur, max_val) = lire_entete ic in
  Printf.fprintf oc "P6\n%d %d\n%s\n" largeur hauteur max_val;  (* Réécrit l'entête complète *)


  let binary_message = string_to_binary_string message in
  let message_length = String.length binary_message in
  let binary_length = int_to_fixed_length_binary_string_30 message_length in
  let full_message = binary_length ^ binary_message in
  let message_index = ref 0 in

  try
    for _ = 1 to hauteur do
      for _ = 1 to largeur do
        for _ = 1 to 3 do
          let octet = input_byte ic in
          let new_octet =
            if !message_index < String.length full_message then
              let bit_message = full_message.[!message_index] in
              incr message_index;
              ecraser_dernier_bit octet bit_message
            else
              octet
          in
          output_byte oc new_octet;
        done;
      done;
    done;
    close_in ic;
    close_out oc;
  with End_of_file ->
    close_in_noerr ic;
    close_out_noerr oc;
    failwith "Fin de fichier atteinte prématurément."

(* Exemple d'utilisation *)
let () =
  let message = "30:82:04:bd:02:01:00:30:0d:06:09:2a:86:48:86:f7:0d:01:01:01:05:00:04:82:04:a7:30:82:04:a3:02:01:00:02:82:01:01:00:a9:79:99:40:c4:5d:e5:f0:ff:dd:e1:fb:fd:0b:38:7a:15:f4:dc:22:f5:15:ff:66:62:5b:9f:70:68:77:61:49:4a:52:be:25:fa:c8:19:e9:cd:b9:e9:4b:9a:53:a0:cc:f3:93:62:07:bb:39:9f:28:ef:70:11:aa:60:10:f4:07:3c:e6:63:46:a3:bb:a2:14:ee:dd:93:71:5e:0e:0c:21:2b:67:a0:1c:ab:b1:24:e9:33:25:ac:e9:a5:6e:79:a0:c3:01:1c:8a:33:ca:ad:ba:1a:cf:ac:50:fd:50:74:4a:8e:ff:a3:45:2a:92:91:d1:82:7f:02:a2:2a:4a:aa:10:76:31:b1:16:34:6b:f2:23:8a:34:0e:89:a6:da:58:bf:8b:95:76:c4:26:cf:d8:85:50:f0:c4:74:cf:80:ca:e3:85:e2:d5:b9:4d:5c:a9:2a:3f:1b:0f:6b:47:33:0f:0f:b6:8e:08:74:20:a1:98:e8:14:4f:a7:2b:aa:17:00:2c:77:bf:ce:50:92:7a:4c:d7:3d:85:d3:e1:10:01:65:05:c2:f9:f6:7a:e4:d5:0e:1a:9a:10:0a:f2:55:c1:07:c9:c2:b3:05:41:01:66:89:af:90:27:86:73:29:35:a7:0c:b4:3b:66:99:d0:91:63:21:cf:c6:e8:32:6d:b8:fb:db:02:03:01:00:01:02:82:01:00:1f:78:e3:f3:69:a4:10:60:15:7c:14:8b:f1:bd:84:32:f3:e1:e1:81:a2:66:ff:e3:79:0f:8f:c7:d5:b6:99:bb:fd:0f:e9:9c:ee:f9:fb:09:f6:b8:f8:30:a0:8b:b8:38:67:61:6e:da:85:b3:d9:31:46:69:d6:f5:ff:5a:16:9f:43:79:7b:65:32:61:d5:63:32:ca:b6:27:5e:f2:97:e1:0f:08:f2:ec:70:1b:f9:01:4f:8c:c7:14:2a:b3:1d:e6:33:60:80:f6:cf:50:6b:bb:bc:5f:e5:9d:b2:b5:1e:ab:7b:eb:d0:5b:b7:60:b9:b0:a1:1d:ca:14:e8:6a:ae:33:0b:fa:d8:54:92:e0:36:1a:8b:01:4b:4e:55:8b:3c:f6:e6:77:4b:ef:af:80:1b:d1:42:c4:66:c6:dc:13:bf:f0:91:90:15:5e:5d:75:bf:eb:63:d2:da:ff:ca:02:03:16:53:84:da:0f:ce:0c:92:a3:fc:4a:4d:4d:d0:3f:be:66:38:fb:a3:75:63:cd:fa:05:cb:4e:d1:95:c1:55:44:f7:ab:be:19:fe:7c:4e:21:6c:7b:06:f7:b3:dc:26:c6:cc:6f:b3:c0:9e:a5:8b:c7:8d:54:eb:f5:d3:1c:ce:1f:fb:4d:a8:06:cc:e1:1b:23:9d:94:e9:a7:d0:bc:f5:09:61:02:81:81:00:d8:ed:48:1c:0a:b3:9f:a5:ee:d2:15:d1:a8:31:7f:ab:e5:77:72:64:5f:c3:7d:9e:1d:e9:72:e3:f5:be:17:11:de:1d:77:26:61:47:70:33:2f:dd:cd:e1:8e:d8:55:7f:07:4f:18:e3:54:28:32:90:cd:f4:1e:33:84:6f:91:9c:92:7e:a3:c8:2a:d7:ae:70:80:b0:2e:1b:cc:75:c2:50:7f:6e:9b:49:5e:a9:ae:02:64:46:7d:b5:9a:af:c7:3e:65:9a:64:9e:30:6e:c5:35:7a:64:32:60:cd:11:e9:b7:5a:32:74:7d:a2:6d:38:12:66:6c:04:49:fb:7e:36:bb:02:81:81:00:c8:00:43:2b:61:19:07:54:42:ec:f8:a7:26:39:50:00:b2:c3:ed:b3:08:85:ba:51:76:b4:1d:a8:64:2a:41:37:6b:89:1b:6c:26:15:41:b1:02:77:58:ae:f0:c3:33:79:4d:71:3c:f0:07:aa:f3:f6:fb:01:8c:2f:67:4d:28:ca:9d:9f:e5:67:07:31:eb:71:e3:b4:31:c8:f2:54:b2:9f:34:6c:e4:b1:a6:36:87:82:c5:91:cb:86:1f:81:0d:27:17:a2:ec:1e:cc:eb:96:22:b8:09:ac:49:eb:73:f5:81:e9:0a:96:5c:15:bd:ac:4f:f7:3a:40:20:16:50:4d:61:02:81:80:30:28:35:0a:ec:a1:45:be:db:8e:ce:03:36:38:8e:e4:45:53:c8:14:5a:62:16:c0:04:59:f0:04:85:68:86:cc:93:a7:ac:ec:db:49:b5:b1:d7:5d:81:38:22:b3:09:ff:e2:4a:7f:f6:ef:96:e0:ea:de:9d:e4:1a:81:bb:16:f3:50:5e:46:d3:c8:17:0b:85:b4:3b:e8:70:89:6a:57:11:f1:c8:47:36:f9:89:e7:b4:66:38:73:ed:de:02:49:d4:23:50:2c:28:26:0e:61:3f:67:05:59:5d:be:08:cc:fe:c4:4a:51:2c:16:e3:f2:3f:bb:77:1d:58:a3:39:38:c3:02:81:81:00:ab:af:e5:f9:60:21:ff:33:ab:3c:07:fc:5d:08:c8:0f:c1:66:06:30:31:79:b2:c6:e9:d2:96:bf:0e:35:63:c4:b2:70:81:74:e6:80:4a:e3:2c:0d:cd:b8:67:7f:fa:39:00:6c:db:f5:e5:6f:30:7c:ed:5e:97:bb:bd:be:f3:ec:73:41:66:c9:79:d8:b5:98:0f:f3:88:e5:4a:79:c0:d8:80:45:96:50:96:31:ab:44:6c:5b:ca:cd:f9:4f:6c:3e:09:c1:57:56:dd:6c:c3:76:4c:6c:29:55:4f:65:3c:f4:e7:5b:aa:55:a4:68:2d:7a:7b:ed:aa:4c:a1:3b:10:21:02:81:80:4e:d6:bd:87:0d:b0:36:26:3d:05:05:e8:89:d4:05:b2:49:8e:84:45:5a:be:54:b5:e8:64:0b:cb:70:90:e5:d5:e6:14:8a:af:a6:ed:d0:07:50:1c:ab:78:1a:b7:b5:78:ef:6a:fc:95:14:06:b9:c9:f6:01:00:ce:40:33:42:fc:4c:8a:d0:b6:5c:e8:93:8e:86:24:ca:75:a1:05:5d:3a:62:d7:c3:bf:61:e2:85:9e:18:5b:58:e2:a0:1e:3c:d5:ae:b2:c7:3b:4c:2e:b9:00:ed:57:25:b2:e7:03:5f:6a:03:63:7a:1b:37:b2:ea:01:b1:5e:fb:6f:1c:cc:8e:47" in
  inserer_message "input.ppm" message
