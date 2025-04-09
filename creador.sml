(*
Nombre del curso: Lenguajes de Programación
Semestre y año: I, 2025
Estudiante: [Duan Antonio Espinoza Olivares] 
Carnet: [2019079490]
Tarea: #2
Fecha de entrega: [9/4/2025]
*)

(* 
obtenerEntradaUsuario
Entrada: Ninguna
Salida: Cadena de texto ingresada por el usuario
Restricciones: Ninguna
Objetivo: Captura una línea de entrada desde la consola. 
*)
fun obtenerEntradaUsuario () =
    Option.getOpt (TextIO.inputLine TextIO.stdIn, "");



(* 
eliminarSaltosDeLinea
Entrada: s -> cadena de texto con saltos de línea
Salida: Cadena sin saltos de línea
Restricciones: s debe ser de tipo string
Objetivo: Elimina caracteres de nueva línea (\n) de una cadena. 
*)
fun eliminarSaltosDeLinea s =
    String.translate (fn #"\n" => "" | c => String.str c) s;


(* 
esEntero
Entrada: s -> cadena de texto
Salida: true si s es un entero válido, false en caso contrario
Restricciones: s debe ser no vacía
Objetivo: Determina si una cadena representa un número entero. 
*)
fun esEntero s = 
    case Int.fromString s of
        SOME _ => true
      | NONE => false;


(* 
solicitarNumero
Entrada: mensaje -> texto a mostrar al usuario
Salida: Cadena válida que representa un entero
Restricciones: El usuario debe ingresar un número válido
Objetivo: Solicita repetidamente un número hasta que se ingrese uno válido. 
*)

fun solicitarNumero mensaje = let
    val _ = print (mensaje ^ ": ")
    val entrada = eliminarSaltosDeLinea (obtenerEntradaUsuario ())
in
    if esEntero entrada then entrada
    else (print "¡Ingrese un número válido!\n"; solicitarNumero mensaje)
end;


(* 
limpiarCatalogo
Entrada: ruta -> dirección del archivo CSV
Salida: Ninguna (efecto secundario: restablece el archivo)
Restricciones: El archivo debe existir o ser creado
Objetivo: Vacía el catálogo y restablece los encabezados. 
*)

fun limpiarCatalogo ruta = let
    val fd = TextIO.openOut ruta
    val _ = TextIO.output(fd, "codigo,fecha_publicacion,autor,genero,copias_disponibles\n")
    val _ = TextIO.closeOut fd
in
    print "Catálogo limpiado exitosamente.\n"
end;


(* 
agregarLibro
Entrada: ruta -> dirección del archivo CSV
Salida: Ninguna (efecto secundario: añade una línea al archivo)
Restricciones: Los campos deben seguir el formato especificado
Objetivo: Registra un nuevo libro en el catálogo. 
*)

fun agregarLibro ruta = let
    val fd = TextIO.openAppend ruta

    val _ = print "Código del libro (ej: LIB1234): "
    val codigo = eliminarSaltosDeLinea (obtenerEntradaUsuario ())

    val _ = print "Fecha de publicación (YYYY-MM-DD): "
    val fecha = eliminarSaltosDeLinea (obtenerEntradaUsuario ())

    val _ = print "Autor: "
    val autor = eliminarSaltosDeLinea (obtenerEntradaUsuario ())

    val _ = print "Género: "
    val genero = eliminarSaltosDeLinea (obtenerEntradaUsuario ())

    val copias = solicitarNumero "Copias disponibles"

    val _ = TextIO.output(fd, codigo ^ "," ^ fecha ^ "," ^ autor ^ "," ^ genero ^ "," ^ copias ^ "\n")
    val _ = TextIO.closeOut fd
in
    print "Libro agregado exitosamente.\n"
end;



(* 
menuCreador
Entrada: ruta -> dirección del archivo CSV
Salida: Ninguna (interacción por consola)
Restricciones: Opciones válidas: 1, 2, 3
Objetivo: Menú interactivo para gestionar el catálogo. 
*)
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


(* 
main
Entrada: Ninguna
Salida: Ninguna (ejecuta el programa)
Restricciones: El usuario debe proporcionar una ruta válida
Objetivo: Punto de entrada del programa Creador. 
*)
fun main () = let
    val _ = print "Ingrese la ruta del archivo CSV: "
    val ruta = eliminarSaltosDeLinea (obtenerEntradaUsuario ())
in
    menuCreador ruta
end;