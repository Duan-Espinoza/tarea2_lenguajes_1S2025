(*
Nombre del curso: Lenguajes de Programación
Semestre y año: I, 2025
Estudiante: [Duan Antonio Espinoza Olivares]
Carnet: [2019079490]
Tarea: #2
Fecha de entrega: [9/4/2025]
*)

(* Funciones comunes *)
fun obtenerEntradaUsuario () = Option.getOpt (TextIO.inputLine TextIO.stdIn, "");
fun eliminarSaltosDeLinea s = String.translate (fn #"\n" => "" | c => String.str c) s;

(* Función para validar números enteros *)
fun esEntero s = 
    case Int.fromString s of
        SOME _ => true
      | NONE => false;

(* Solicitar campo numérico válido *)
fun solicitarNumero mensaje = let
    val _ = print (mensaje ^ ": ")
    val entrada = eliminarSaltosDeLinea (obtenerEntradaUsuario ())
in
    if esEntero entrada then entrada
    else (print "¡Ingrese un número válido!\n"; solicitarNumero mensaje)
end;


(* Leer archivo CSV *)
fun leerCSV ruta = let
    val canal = TextIO.openIn ruta
    fun leerLineas canal =
        case TextIO.inputLine canal of
            SOME linea => eliminarSaltosDeLinea linea :: leerLineas canal
          | NONE => []
    val lineas = leerLineas canal
    val _ = TextIO.closeIn canal
in
    List.map (String.tokens (fn c => c = #",")) lineas
end;

(* Mostrar libros en formato de tabla *)
fun imprimirLibros libros = let
    val _ = print "\nCódigo\t| Fecha\t\t| Autor\t\t| Género\t| Copias\n"
    val _ = print "--------------------------------------------------------\n"
    fun imprimir libro =
        print (List.nth(libro,0) ^ "\t| " ^
        List.nth(libro,1) ^ "\t| " ^
        List.nth(libro,2) ^ "\t| " ^
        List.nth(libro,3) ^ "\t| " ^
        List.nth(libro,4) ^ "\n")
in
    List.app imprimir libros
end;

(* a. Libros populares por rango de copias *)
fun mostrarPopulares libros = let
    val min = valOf (Int.fromString (solicitarNumero "Mínimo de copias"))
    val max = valOf (Int.fromString (solicitarNumero "Máximo de copias"))
    val filtrados = List.filter (fn libro =>
        let 
            val copias = valOf (Int.fromString (List.nth(libro,4)))
        in 
            copias >= min andalso copias <= max end) libros
    val ordenados = ListMergeSort.sort (fn (a,b) =>
        valOf (Int.fromString (List.nth(a,4))) > valOf (Int.fromString (List.nth(b,4)))) filtrados
in
    imprimirLibros ordenados
end;

(* b. Autores con más de 5 libros *)
fun autoresPopulares libros = let
    val conteo = foldl (fn (libro, dict) =>
        let val autor = List.nth(libro,2)
        in case List.find (fn (a,_) => a = autor) dict of
            SOME (_, count) => (autor, count+1) :: List.filter (fn (a,_) => a <> autor) dict
          | NONE => (autor, 1) :: dict
        end) [] libros
    val autoresFiltrados = List.filter (fn (_, count) => count >= 5) conteo
in
    print "Autores con 5+ libros:\n";
    List.app (fn (a,c) => print (a ^ " (" ^ Int.toString c ^ ")\n")) autoresFiltrados
end;


(* Función para buscar libros por código o autor *)
fun buscarLibros libros = let
    val _ = print "Ingrese código (LIBXXXX) o autor: "
    val consulta = eliminarSaltosDeLinea (obtenerEntradaUsuario ())
    val esCodigo = String.isPrefix "LIB" (String.map Char.toUpper consulta)
    val resultados = List.filter (fn libro =>
        if esCodigo then
            List.nth(libro, 0) = consulta
        else
            String.map Char.toUpper (List.nth(libro, 2)) = String.map Char.toUpper consulta
    ) libros
in
    if List.null resultados then
        print "No se encontraron resultados.\n"
    else
        imprimirLibros resultados
end;


(* Función para contar libros por género *)
fun contarPorGenero libros = let
    val _ = print "Ingrese género: "
    val genero = eliminarSaltosDeLinea (obtenerEntradaUsuario ())
    val conteo = List.length (List.filter (fn libro =>
        String.map Char.toUpper (List.nth(libro, 3)) = String.map Char.toUpper genero
    ) libros)
in
    print ("Cantidad de libros en " ^ genero ^ ": " ^ Int.toString conteo ^ "\n")
end;



(* Funciones auxiliares para el resumen *)

(* 1. Cantidad de libros por género *)
fun contarLibrosPorGenero libros = let
    val generos = List.map (fn libro => List.nth(libro, 3)) libros
    val conteo = foldl (fn (g, dict) =>
        case List.find (fn (gen, _) => gen = g) dict of
            SOME (_, count) => (g, count + 1) :: List.filter (fn (gen, _) => gen <> g) dict
          | NONE => (g, 1) :: dict
    ) [] generos
in
    conteo
end;

(* Determina la cantidad máxima de copias *)
fun libroMaxCopias libros = 
    hd (ListMergeSort.sort (fn (a, b) =>
        valOf (Int.fromString (List.nth(a, 4))) < valOf (Int.fromString (List.nth(b, 4)))
    ) libros);


(* Determina la cantidad minima de copias *)
fun libroMinCopias libros = 
    hd (ListMergeSort.sort (fn (a, b) =>
        valOf (Int.fromString (List.nth(a, 4))) > valOf (Int.fromString (List.nth(b, 4)))
    ) libros);


(* 4. Autor con más libros *)
fun autorMasLibros libros = let
    val conteoAutores = foldl (fn (libro, dict) =>
        let val autor = List.nth(libro, 2)
        in case List.find (fn (a, _) => a = autor) dict of
            SOME (_, count) => (autor, count + 1) :: List.filter (fn (a, _) => a <> autor) dict
          | NONE => (autor, 1) :: dict
        end) [] libros
    val (autorMax, countMax) = foldl (fn ((a, c), (aMax, cMax)) =>
        if c > cMax then (a, c) else (aMax, cMax)
    ) ("", 0) conteoAutores
in
    (autorMax, countMax)
end;

(* 5. Género con más libros *)
fun generoMasLibros libros = let
    val conteoGeneros = contarLibrosPorGenero libros
    val (generoMax, countMax) = foldl (fn ((g, c), (gMax, cMax)) =>
        if c > cMax then (g, c) else (gMax, cMax)
    ) ("", 0) conteoGeneros
in
    (generoMax, countMax)
end;

(* Función para mostrar el resumen *)
fun mostrarResumen libros = let
    val porGenero = contarLibrosPorGenero libros
    val maxCopias = libroMaxCopias libros
    val minCopias = libroMinCopias libros
    val (autorTop, countAutor) = autorMasLibros libros
    val (generoTop, countGenero) = generoMasLibros libros
in
    print "\n=== Resumen General ===\n";
    print "1. Cantidad de libros por género:\n";
    List.app (fn (g, c) => print (g ^ ": " ^ Int.toString c ^ "\n")) porGenero;
    print "\n2. Libro con más copias:\n";
    imprimirLibros [maxCopias];
    print "\n3. Libro con menos copias:\n";
    imprimirLibros [minCopias];
    print ("\n4. Autor con más libros: " ^ autorTop ^ " (" ^ Int.toString countAutor ^ ")\n");
    print ("5. Género más popular: " ^ generoTop ^ " (" ^ Int.toString countGenero ^ ")\n")
end;



(* Menú principal del Analizador *)
(* Menú principal del Analizador *)
fun menuAnalizador libros = let
    val _ = print "\n--- Menú Analizador ---\n"
    val _ = print "a. Libros populares\n"
    val _ = print "b. Autores con 5+ libros\n"
    val _ = print "c. Buscar por código/autor\n"
    val _ = print "d. Cantidad por género\n"
    val _ = print "e. Resumen general\n"
    val _ = print "f. Salir\n"
    val opcion = eliminarSaltosDeLinea (obtenerEntradaUsuario ())
in
    case opcion of
        "a" => (mostrarPopulares libros; menuAnalizador libros)
      | "b" => (autoresPopulares libros; menuAnalizador libros)
      | "c" => (buscarLibros libros; menuAnalizador libros)
      | "d" => (contarPorGenero libros; menuAnalizador libros)
      | "e" => (mostrarResumen libros; menuAnalizador libros)
      | "f" => OS.Process.exit OS.Process.success
      | _ => (print "Opción inválida.\n"; menuAnalizador libros)
end;

(* Función principal *)
fun main () = let
    val _ = print "Ruta del archivo CSV: "
    val ruta = eliminarSaltosDeLinea (obtenerEntradaUsuario ())
    val libros = leerCSV ruta
in
    menuAnalizador (tl libros) (* Ignorar encabezado *)
end;