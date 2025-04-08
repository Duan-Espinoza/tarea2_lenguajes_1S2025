(*
Nombre del curso: Lenguajes de Programación
Semestre y año: I, 2025
Estudiante: [Duan Antonio Espinoza Olivares] 
Carnet: [2019079490]
Tarea: #2
Fecha de entrega: [9/4/2025]
Estatus: Excelente
*)

(* Función para obtener entrada del usuario *)
fun obtenerEntradaUsuario () =
    Option.getOpt (TextIO.inputLine TextIO.stdIn, "");

(* Eliminar saltos de línea *)
fun eliminarSaltosDeLinea s =
    String.translate (fn #"\n" => "" | c => String.str c) s;

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

(* Limpiar el archivo CSV (restablecer encabezados) *)
fun limpiarCatalogo ruta = let
    val fd = TextIO.openOut ruta
    val _ = TextIO.output(fd, "codigo,fecha_publicacion,autor,genero,copias_disponibles\n")
    val _ = TextIO.closeOut fd
in
    print "Catálogo limpiado exitosamente.\n"
end;

(* Agregar un nuevo libro al archivo *)
fun agregarLibro ruta = let
    val fd = TextIO.openAppend ruta
    val codigo = eliminarSaltosDeLinea (solicitarNumero "Código del libro (ej: LIB1234)")
    val fecha = eliminarSaltosDeLinea (solicitarNumero "Fecha de publicación (YYYY-MM-DD)")
    val autor = eliminarSaltosDeLinea (print "Autor: "; obtenerEntradaUsuario ())
    val genero = eliminarSaltosDeLinea (print "Género: "; obtenerEntradaUsuario ())
    val copias = solicitarNumero "Copias disponibles"
    val _ = TextIO.output(fd, codigo ^ "," ^ fecha ^ "," ^ autor ^ "," ^ genero ^ "," ^ copias ^ "\n")
    val _ = TextIO.closeOut fd
in
    print "Libro agregado exitosamente.\n"
end;

(* Menú principal del Creador *)
fun menuCreador ruta = let
    val _ = print "\n--- Menú Creador ---\n"
    val _ = print "1. Agregar libro\n"
    val _ = print "2. Limpiar catálogo\n"
    val _ = print "3. Salir\n"
    val opcion = eliminarSaltosDeLinea (obtenerEntradaUsuario ())
in
    case opcion of
        "1" => (agregarLibro ruta; menuCreador ruta)
      | "2" => (limpiarCatalogo ruta; menuCreador ruta)
      | "3" => OS.Process.exit OS.Process.success
      | _ => (print "Opción inválida.\n"; menuCreador ruta)
end;

(* Función principal *)
fun main () = let
    val _ = print "Ingrese la ruta del archivo CSV: "
    val ruta = eliminarSaltosDeLinea (obtenerEntradaUsuario ())
in
    menuCreador ruta
end;