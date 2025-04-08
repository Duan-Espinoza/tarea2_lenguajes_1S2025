(*
Nombre del curso: Lenguajes de Programación
Semestre y año: I, 2025
Estudiante: [Duan Antonio Espinoza Olivares]
Carnet: [2019079490]
Tarea: #2
Fecha de entrega: [9/4/2025]
Estatus: Excelente
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

(* Menú principal del Analizador *)
fun menuAnalizador libros = let
    val _ = print "\n--- Menú Analizador ---\n"
    val _ = print "a. Libros populares\nb. Autores con 5+ libros\nc. Buscar\n... [Resto de opciones]\n"
    val opcion = eliminarSaltosDeLinea (obtenerEntradaUsuario ())
in
    case opcion of
        "a" => (mostrarPopulares libros; menuAnalizador libros)
      | "b" => (autoresPopulares libros; menuAnalizador libros)
      | "c" => (print "Función de búsqueda no implementada.\n"; menuAnalizador libros)
      | "d" => (print "Función de estadísticas no implementada.\n"; menuAnalizador libros)
      | "e" => OS.Process.exit OS.Process.success
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