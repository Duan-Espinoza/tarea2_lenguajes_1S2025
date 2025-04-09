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



(* 
leerCSV
Entrada: ruta -> dirección del archivo CSV
Salida: Lista de listas con los datos del CSV
Restricciones: El archivo debe existir y tener formato válido
Objetivo: Convierte un archivo CSV en una estructura de datos. 
*)

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


(* 
imprimirLibros
Entrada: libros -> lista de libros (cada libro es una lista de strings)
Salida: Ninguna (imprime una tabla en consola)
Restricciones: Cada libro debe tener 5 campos
Objetivo: Muestra los libros en formato tabular. 
*)

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


(* 
mostrarPopulares
Entrada: libros -> lista de libros
Salida: Ninguna (imprime libros filtrados y ordenados)
Restricciones: min y max deben ser enteros no negativos
Objetivo: Filtra libros por rango de copias y los ordena descendente. 
*)
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

(* 
autoresPopulares
Entrada: libros -> lista de libros
Salida: Ninguna (imprime autores con 5+ libros)
Restricciones: Ninguna
Objetivo: Identifica autores con al menos 5 libros registrados. 
*)
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



(* 
buscarLibros
Entrada: libros -> lista de libros
Salida: Ninguna (imprime resultados de búsqueda)
Restricciones: La consulta debe ser un código (LIBXXXX) o nombre de autor
Objetivo: Busca libros por código o autor (insensible a mayúsculas). 
*)

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



(* 
contarPorGenero
Entrada: libros -> lista de libros
Salida: Ninguna (imprime el conteo)
Restricciones: El género debe existir en el catálogo
Objetivo: Cuenta los libros de un género específico. 
*)
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


(* 
contarLibrosPorGenero
Entrada: libros -> lista de libros
Salida: Lista de tuplas (género, cantidad)
Restricciones: Ninguna
Objetivo: Calcula la cantidad de libros por cada género. 
*)
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


(* 
libroMaxCopias 
Entrada: libros -> lista de libros
Salida: Libro con más copias (lista de strings)
Restricciones: La lista no debe estar vacía
Objetivo: Determina el libro con la cantidad máxima/mínima de copias. 
*)
fun libroMaxCopias libros = 
    hd (ListMergeSort.sort (fn (a, b) =>
        valOf (Int.fromString (List.nth(a, 4))) < valOf (Int.fromString (List.nth(b, 4)))
    ) libros);



(* 
libroMinCopias
Entrada: libros -> lista de libros
Salida: Libro con menos copias (lista de strings)
Restricciones: La lista no debe estar vacía
Objetivo: Determina el libro con la cantidad máxima/mínima de copias. 
*)
fun libroMinCopias libros = 
    hd (ListMergeSort.sort (fn (a, b) =>
        valOf (Int.fromString (List.nth(a, 4))) > valOf (Int.fromString (List.nth(b, 4)))
    ) libros);



(* 
autorMasLibros 
Entrada: libros -> lista de libros
Salida: Tupla (autor, cantidad)
Restricciones: Ninguna
Objetivo: Identifica al autor con más libros registrados. 
*)
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

(* 
generoMasLibros
Entrada: libros -> lista de libros
Salida: Tupla (género, cantidad)
Restricciones: Ninguna
Objetivo: Identifica al género con más libros registrados. 
*)
fun generoMasLibros libros = let
    val conteoGeneros = contarLibrosPorGenero libros
    val (generoMax, countMax) = foldl (fn ((g, c), (gMax, cMax)) =>
        if c > cMax then (g, c) else (gMax, cMax)
    ) ("", 0) conteoGeneros
in
    (generoMax, countMax)
end;


(* 
mostrarResumen
Entrada: libros -> lista de libros
Salida: Ninguna (imprime un informe detallado)
Restricciones: Ninguna
Objetivo: Genera un resumen estadístico de la biblioteca. 
*)
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




(* 
menuAnalizador
Entrada: libros -> lista de libros
Salida: Ninguna (interacción por consola)
Restricciones: Opciones válidas: a, b, c, d, e, f
Objetivo: Menú interactivo para análisis de datos. 
*)
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


(* 
main
Entrada: Ninguna
Salida: Ninguna (ejecuta el programa)
Restricciones: El archivo CSV debe existir
Objetivo: Punto de entrada del programa Analizador. 
*)

fun main () = let
    val _ = print "Ruta del archivo CSV: "
    val ruta = eliminarSaltosDeLinea (obtenerEntradaUsuario ())
    val libros = leerCSV ruta
in
    menuAnalizador (tl libros) (* Ignorar encabezado *)
end;