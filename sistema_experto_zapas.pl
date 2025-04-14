% Codificación UTF-8 para soportar caracteres especiales.
:- encoding(utf8).
% Importa la librería para leer entradas desde el usuario.
:- use_module(library(readutil)).
% Define el hecho 'usuario/4' para almacenar la información del usuario.
:- dynamic usuario/4.
% Define el hecho 'feedback/3' para almacenar los comentarios de los usuarios.
:- dynamic feedback/3.
% Define el hecho 'oferta/2' para almacenar las ofertas de zapatillas.
:- dynamic oferta/2.
% Define el hecho 'preferencia/2' para almacenar las preferencias de los usuarios.
:- dynamic preferencia/2.
% Define el hecho 'complemento/2' para almacenar productos complementarios.
:- dynamic complemento/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% BASE DE CONOCIMIENTO %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Zapatillas (con precio exacto)
% zapatilla(Nombre, Tipo, Categoria, Valoración, Características, Colores, Género, Precio)

zapatilla(nike_air_max, running, premium, 4.5, [amortiguacion, ligereza, tecnologia_air], [negro, blanco, rojo], hombre, 200).
zapatilla(adidas_ultraboost, running, premium, 4.7, [confort, responsive, boost], [blanco, negro, azul], unisex, 180).
zapatilla(puma_rsx, urbano, medio, 4.2, [estilo_retro, comodidad, suela_goma], [negro, blanco, gris], unisex, 120).
zapatilla(reebok_nano, crossfit, premium, 4.3, [estabilidad, durabilidad, soporte], [negro, rojo, azul], unisex, 160).
zapatilla(new_balance_574, casual, economico, 4.0, [confort, estilo_clasico, versatilidad], [gris, azul, marron], unisex, 70).
zapatilla(asics_gel_kayano, running, premium, 4.6, [soporte_arco, amortiguacion, estabilidad], [negro, azul, plateado], unisex, 170).
zapatilla(vans_old_skool, skate, economico, 4.1, [durabilidad, estilo_urbano, suela_antideslizante], [negro, blanco, rojo], unisex, 65).
zapatilla(converse_all_star, casual, economico, 4.0, [estilo_iconico, versatilidad, suela_goma], [negro, blanco, rojo], unisex, 60).
zapatilla(hoka_one_one, trail, premium, 4.4, [amortiguacion_maxima, agarre, soporte], [negro, naranja, verde], unisex, 190).
zapatilla(salomon_speedcross, trail, medio, 4.3, [traction, proteccion, resistencia_agua], [negro, azul, amarillo], unisex, 140).
zapatilla(nike_metcon, crossfit, medio, 4.2, [estabilidad, flexibilidad, durabilidad], [negro, rojo, blanco], unisex, 130).
zapatilla(adidas_superstar, urbano, medio, 4.1, [estilo_iconico, confort, suela_goma], [blanco, negro, dorado], unisex, 110).
zapatilla(brooks_ghost, running, medio, 4.5, [amortiguacion, transicion_suave, confort], [negro, azul, blanco], unisex, 150).
zapatilla(saucony_guide, running, medio, 4.4, [soporte, amortiguacion, durabilidad], [negro, verde, gris], unisex, 145).
zapatilla(under_armour_charged, entrenamiento, economico, 4.0, [respuesta, soporte, flexibilidad], [negro, rojo, blanco], unisex, 75).
zapatilla(nike_react_infinity, running, premium, 4.6, [amortiguacion, soporte, estabilidad], [blanco, azul, verde], unisex, 195).
zapatilla(adidas_nmd_r1, urbano, medio, 4.3, [estilo_moderno, confort, boost], [negro, rojo, blanco], unisex, 130).
zapatilla(puma_calibrate, running, premium, 4.4, [estilo_futurista, amortiguacion_xetic, confort], [negro, gris, azul], hombre, 160).
zapatilla(reebok_floatride, running, premium, 4.5, [ligereza, respuesta, soporte], [blanco, azul, lima], mujer, 175).
zapatilla(new_balance_fresh_foam, entrenamiento, medio, 4.2, [amortiguacion, estabilidad, comodidad], [gris, azul, naranja], unisex, 120).
zapatilla(asics_gel_nimbus, running, premium, 4.7, [amortiguacion, soporte_arco, durabilidad], [azul, blanco, negro], unisex, 180).
zapatilla(vans_sk8_hi, skate, economico, 4.2, [estilo_clasico, durabilidad, suela_antideslizante], [negro, rojo, blanco], unisex, 70).
zapatilla(converse_run_star_hike, casual, medio, 4.1, [estilo_moderno, suela_elevada, confort], [blanco, negro, beige], mujer, 110).
zapatilla(hoka_clifton, running, premium, 4.6, [amortiguacion, ligereza, transicion_suave], [negro, gris, azul], unisex, 175).
zapatilla(salomon_xt6, trail, premium, 4.5, [agarre, proteccion, estabilidad], [negro, rojo, verde], unisex, 185).
zapatilla(nike_blazer_mid, urbano, medio, 4.2, [estilo_retro, soporte, versatilidad], [blanco, gris, negro], unisex, 100).
zapatilla(adidas_ozweego, urbano, medio, 4.1, [estilo_moderno, confort, amortiguacion], [verde, blanco, negro], unisex, 120).
zapatilla(puma_cali, casual, economico, 4.0, [estilo_fresco, suela_goma, ligereza], [blanco, beige, negro], mujer, 85).
zapatilla(reebok_classic_leather, casual, economico, 4.3, [comodidad, estilo_clasico, durabilidad], [blanco, azul, gris], unisex, 75).
zapatilla(new_balance_990, urbano, premium, 4.6, [confort, soporte, calidad_materiales], [gris, azul, rojo], unisex, 200).
zapatilla(asics_gel_cumulus, running, medio, 4.4, [amortiguacion, suavidad, soporte], [negro, blanco, celeste], unisex, 145).
zapatilla(vans_era, skate, economico, 4.0, [estilo_simple, ligereza, traccion], [negro, rojo, blanco], unisex, 60).
zapatilla(converse_chuck_70, casual, medio, 4.3, [estilo_retro, confort, suela_goma], [blanco, negro, vino], unisex, 90).
zapatilla(hoka_speedgoat, trail, premium, 4.6, [amortiguacion_maxima, agarre, resistencia], [negro, amarillo, azul], unisex, 185).
zapatilla(salomon_sense_ride, trail, medio, 4.3, [proteccion, confort, traccion], [rojo, gris, azul], unisex, 135).
zapatilla(nike_pegasus, running, medio, 4.5, [ligereza, respuesta, durabilidad], [azul, blanco, verde], unisex, 130).
zapatilla(adidas_pureboost, running, medio, 4.2, [confort, boost, amortiguacion], [negro, gris, azul], unisex, 125).
zapatilla(puma_future_rider, urbano, medio, 4.1, [estilo_retro, comodidad, colorido], [azul, rojo, blanco], unisex, 95).
zapatilla(reebok_zig, entrenamiento, medio, 4.2, [respuesta, soporte, estilo_futurista], [negro, rojo, blanco], unisex, 115).
zapatilla(new_balance_fuelcell, running, premium, 4.5, [velocidad, respuesta, confort], [amarillo, negro, azul], unisex, 160).
zapatilla(asics_noosa_triad, triatlon, premium, 4.4, [ligereza, drenaje_agua, estilo_vibrante], [multicolor, azul, naranja], unisex, 175).
zapatilla(vans_ultrarange, skate, medio, 4.3, [confort, estilo_moderno, ligereza], [negro, gris, azul], unisex, 100).
zapatilla(converse_lugged, casual, medio, 4.2, [plataforma, estilo_urbano, agarre], [negro, beige, blanco], mujer, 105).
zapatilla(hoka_bondi, running, premium, 4.7, [amortiguacion_maxima, soporte, confort], [negro, blanco, azul], unisex, 190).
zapatilla(salomon_wings, trail, medio, 4.4, [traccion, proteccion, durabilidad], [gris, rojo, azul], unisex, 150).
zapatilla(nike_air_zoom, entrenamiento, medio, 4.3, [estabilidad, amortiguacion, respuesta], [negro, blanco, azul], unisex, 135).
zapatilla(adidas_forum_low, urbano, medio, 4.1, [estilo_ochentero, soporte, suela_goma], [blanco, azul, negro], unisex, 110).
zapatilla(puma_smash, casual, economico, 4.0, [estilo_simple, comodidad, durabilidad], [negro, blanco, azul], unisex, 65).
zapatilla(reebok_flexagon, entrenamiento, economico, 4.1, [flexibilidad, ligereza, soporte], [gris, azul, negro], unisex, 70).
zapatilla(new_balance_997, urbano, medio, 4.3, [estilo_moderno, confort, durabilidad], [gris, blanco, azul], unisex, 115).
zapatilla(asics_gel_ds_trainer, running, medio, 4.4, [ligereza, soporte, velocidad], [negro, verde, azul], unisex, 140).
zapatilla(vans_classic_slipon, skate, economico, 4.0, [comodidad, estilo_urbano, suela_goma], [blanco, negro, cuadros], unisex, 60).
zapatilla(converse_platform_lift, casual, medio, 4.2, [altura, estilo_moderno, confort], [blanco, negro, rosa], mujer, 95).
zapatilla(hoka_mach, running, premium, 4.5, [ligereza, respuesta, amortiguacion], [gris, azul, verde], unisex, 170).
zapatilla(salomon_x_ultra, trail, premium, 4.6, [soporte, traccion, impermeabilidad], [negro, marron, verde], unisex, 180).

% Productos complementarios
complemento(running, medias_tecnicas).
complemento(running, short_compresion).
complemento(crossfit, rodilleras).
complemento(crossfit, cinturon_lumbar).
complemento(trail, bastones_trekking).
complemento(skate, rodilleras_proteccion).
complemento(entrenamiento, guantes_gimnasio).

% Rangos de precios
rango_precio(economico, 0, 80).
rango_precio(medio, 81, 150).
rango_precio(premium, 151, 300).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% PERSISTENCIA %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cargar los datos desde el archivo
cargar_datos :- 
    % Elimina los hechos previos para evitar duplicados
    % retractall(oferta(_, _)),
    % retractall(complemento(_, _)),
    retractall(feedback(_, _, _)),
    retractall(usuario(_, _, _, _)),

    % Carga los nuevos datos desde el archivo
    (   exists_file('datos_usuarios.dat')
    ->  consult('datos_usuarios.dat'),  % Carga el archivo .dat si existe
        writeln('Datos cargados exitosamente.')
    ;   writeln('El archivo no existe, se procederá a crear uno vacío.')
    ).

% Ofertas especiales
oferta(nike_air_max, 15). % Oferta: 15% de descuento en Nike Air Max
oferta(adidas_ultraboost, 10). % Oferta: 10% de descuento en Adidas Ultraboost
oferta(reebok_nano, 20). % Oferta: 20% de descuento en Reebok Nano

% Guardar los datos en el archivo
guardar_datos :- 
    open('datos_usuarios.dat', write, Stream),  % Abre el archivo 'datos_usuarios.dat' para escritura
    with_output_to(Stream, (
        forall(usuario(U, P, H, F), format(Stream, 'usuario(~q, ~q, ~q, ~q).~n', [U, P, H, F])),
        forall(feedback(U, Z, C), format(Stream, 'feedback(~q, ~q, ~q).~n', [U, Z, C]))
        % forall(oferta(Z, D), format(Stream, 'oferta(~q, ~q).~n', [Z, D])),
        % forall(complemento(A, C), format(Stream, 'complemento(~q, ~q).~n', [A, C]))
    )),
    close(Stream),  % Cierra el archivo después de guardar los datos
    writeln('Datos guardados correctamente.').

% Predicado principal que se ejecuta al iniciar el programa
main :- 
    cargar_datos, % Carga los datos del archivo
    writeln('Programa iniciado.'). % Muestra un mensaje de inicio del programa

% Registrar un nuevo usuario
registrar_usuario(Id) :- 
    (   usuario(Id, _, _, _) ->  % Verifica si el usuario ya existe
        writeln('Usuario ya existe.') % Si el usuario ya está registrado, muestra un mensaje
    ;   assertz(usuario(Id, [], [], [])), % Si el usuario no existe, lo registra con datos vacíos
        writeln('Usuario registrado exitosamente.') % Muestra mensaje de éxito
    ).

% Actualizar los datos de un usuario existente
actualizar_usuario(Id, Prefs, Hist, Feed) :- 
    retract(usuario(Id, _, _, _)), % Elimina el hecho actual del usuario
    assertz(usuario(Id, Prefs, Hist, Feed)). % Añade el hecho actualizado con las nuevas preferencias, historial y feedback

%%%%%%%%%%%%%%%%%%%%%%
%%%%%% INTERFAZ %%%%%%
%%%%%%%%%%%%%%%%%%%%%%

% Predicado para iniciar el sistema
iniciar :- 
    writeln('=================================================='),  % Muestra un encabezado con formato
    writeln('  SISTEMA EXPERTO DE RECOMENDACIÓN DE ZAPATILLAS  '),  % Título del sistema
    writeln('=================================================='),  % Fin del encabezado
    nl,  % Nueva línea para separar la salida
    cargar_datos,  % Carga los datos desde el archivo
    autenticar_usuario.  % Llama al predicado de autenticación del usuario

% Predicado para autenticar al usuario, verificando si es registrado o no
autenticar_usuario :- 
    writeln('\n¿Eres usuario registrado? (si/no):'),  % Pregunta si el usuario está registrado
    leer_linea(Respuesta),  % Lee la respuesta del usuario
    (   Respuesta == 'si' ->  % Si la respuesta es "si", continúa con la autenticación
        writeln('Ingresa tu ID:'),  % Solicita el ID del usuario
        leer_linea(Id),  % Lee el ID del usuario
        (   es_entero(Id) ->  % Verifica que el ID sea un número entero
            (   usuario(Id, _, _, _) ->  % Si el ID existe en la base de datos
                menu_principal(Id)  % Llama al menú principal
            ;   writeln('Usuario no encontrado.'),  % Si no se encuentra el usuario
                autenticar_usuario  % Vuelve a intentar la autenticación
            )
        ;   writeln('ID inválido. Por favor ingresa un número entero.'),  % Si el ID no es un número
            autenticar_usuario  % Vuelve a intentar la autenticación
        )
    ;   Respuesta == 'no' ->  % Si la respuesta es "no", realiza el registro rápido
        writeln('Registro rápido: ingresa un ID:'),  % Solicita el ID para registro
        leer_linea(NuevoId),  % Lee el nuevo ID
        (   es_entero(NuevoId) ->  % Verifica que el nuevo ID sea un número entero
            (   usuario(NuevoId, _, _, _) ->  % Si el ID ya está registrado
                writeln('Este ID ya está registrado. Por favor, ingresa otro ID.'),  % Informa que el ID ya existe
                autenticar_usuario  % Vuelve a intentar la autenticación
            ;   registrar_usuario(NuevoId),  % Si el ID no está registrado, lo registra
                menu_principal(NuevoId)  % Llama al menú principal con el nuevo ID
            )
        ;   writeln('ID inválido. Por favor ingresa un número entero.'),  % Si el ID no es un número
            autenticar_usuario  % Vuelve a intentar la autenticación
        )
    ;   writeln('Respuesta inválida. Por favor, responde con "si" o "no".'),  % Si la respuesta no es "si" ni "no"
        autenticar_usuario  % Vuelve a intentar la autenticación
    ).

% Verificar si la entrada es un número entero
es_entero(X) :- 
    catch(atom_number(X, _), _, fail).  % Si la conversión falla, no es un entero

% Menú principal con opciones repetitivas para el usuario
menu_principal(Usuario) :- 
    repeat,  % Repite el menú hasta que el usuario elija salir
    writeln('\nMENÚ PRINCIPAL:'),  % Muestra el menú de opciones
    writeln('1. Recomendación personalizada'),  % Opción 1: Obtener recomendaciones personalizadas
    writeln('2. Ver historial y preferencias'),  % Opción 2: Ver historial y preferencias del usuario
    writeln('3. Ofertas especiales'),  % Opción 3: Mostrar ofertas especiales
    writeln('4. Dar feedback'),  % Opción 4: Dejar comentarios o feedback
    writeln('5. Salir'),  % Opción 5: Salir del sistema
    leer_linea(Opcion),  % Lee la opción elegida por el usuario
    (   Opcion == '1' -> obtener_preferencias(Usuario), !, menu_principal(Usuario)  % Si se elige la opción 1
    ;   Opcion == '2' -> mostrar_historial(Usuario), !, menu_principal(Usuario)  % Si se elige la opción 2
    ;   Opcion == '3' -> mostrar_ofertas, !, menu_principal(Usuario)  % Si se elige la opción 3
    ;   Opcion == '4' -> recibir_feedback(Usuario), !, menu_principal(Usuario)  % Si se elige la opción 4
    ;   Opcion == '5' -> guardar_datos, writeln('¡Hasta pronto!'), !  % Si se elige la opción 5, guarda los datos y sale
    ;   writeln('Opción inválida'), fail  % Si la opción es inválida, muestra el mensaje de error y repite
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% OBTENER PREFERENCIAS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicado principal que guía al usuario a través de varias preguntas para capturar sus preferencias
obtener_preferencias(Usuario) :-
    writeln('\n=== PREFERENCIAS ==='),  % Muestra título
    obtener_actividad(Usuario),          % Pide actividad
    obtener_presupuesto(Usuario),        % Pide presupuesto
    obtener_genero(Usuario),             % Pide género
    obtener_color(Usuario),              % Pide color
    recomendar_zapatilla(Usuario).       % Llama al motor de recomendación

% Pregunta por la actividad principal de uso de las zapatillas
obtener_actividad(Usuario) :-
    repeat,  % Permite repetir en caso de error
        writeln('\n¿Para qué actividad principal las necesitas?'),
        writeln('(running/crossfit/urbano/casual/trail/skate/entrenamiento):'),
        leer_linea(Actividad),  % Lee la respuesta del usuario
        (actividad_valida(Actividad) ->  % Verifica que la actividad sea válida
            (
                usuario(Usuario, Prefs, Hist, Feed),  % Recupera la información del usuario
                actualizar_usuario(Usuario, [actividad(Actividad)|Prefs], Hist, Feed)  % Actualiza preferencias
            ),
            !  % Sale del repeat
        ;
            writeln('Opción inválida, por favor inténtalo de nuevo.'), fail  % Vuelve a pedir si es inválido
        ).

% Pregunta por el rango de presupuesto del usuario
obtener_presupuesto(Usuario) :-
    repeat,
        writeln('\n¿Cuál es tu rango de presupuesto?'),
        writeln('(economico/medio/premium):'),
        leer_linea(Presupuesto),
        (presupuesto_valido(Presupuesto) ->  % Verifica si es un presupuesto válido
            (
                usuario(Usuario, Prefs, Hist, Feed),
                actualizar_usuario(Usuario, [presupuesto(Presupuesto)|Prefs], Hist, Feed)
            ),
            !
        ;
            writeln('Opción inválida, por favor inténtalo de nuevo.'), fail
        ).

% Pregunta por el género de las zapatillas que prefiere el usuario
obtener_genero(Usuario) :-
    repeat,
        writeln('\n¿Prefieres zapatillas para (hombre/mujer/unisex)?:'),
        leer_linea(Genero),
        (genero_valido(Genero) ->  % Verifica si el género ingresado es válido
            (
                usuario(Usuario, Prefs, Hist, Feed),
                actualizar_usuario(Usuario, [genero(Genero)|Prefs], Hist, Feed)
            ),
            !
        ;
            writeln('Opción inválida, por favor inténtalo de nuevo.'), fail
        ).

% Pregunta por el color preferido de las zapatillas
obtener_color(Usuario) :-
    repeat,
        writeln('\n¿Qué color prefieres?'),
        writeln('(negro/blanco/rojo/azul/gris/marron/verde/naranja/amarillo/dorado/plateado):'),
        leer_linea(Color),
        (color_valido(Color) ->  % Verifica si el color es válido
            (
                usuario(Usuario, Prefs, Hist, Feed),
                actualizar_usuario(Usuario, [color(Color)|Prefs], Hist, Feed)
            ),
            !
        ;
            writeln('Opción inválida, por favor inténtalo de nuevo.'), fail
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% LÓGICA DE RECOMENDACIÓN %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Punto de entrada para recomendar zapatillas según preferencias del usuario
recomendar_zapatilla(Usuario) :-
    usuario(Usuario, Prefs, _, _),  % Obtiene las preferencias del usuario
    findall(Zap-Precio, recomendar_zapatilla_aux(Prefs, Zap, Precio), Zapatillas),
    (Zapatillas = [] -> 
        % Si no hay coincidencias exactas, se buscan alternativas
        writeln('\nNo hay coincidencias exactas. Mostrando alternativas:'),
        encontrar_alternativas(Prefs)
    ;
        % Si hay coincidencias, se ordenan y muestran
        ordenar_recomendaciones(Zapatillas, Ordenadas),
        mostrar_recomendaciones(Ordenadas, [])  % Lista vacía como historial de mostrados
    ),
    % Se sugieren complementos para la zapatilla principal recomendada
    sugerir_complementos(Ordenadas, Usuario).

% Verifica que una zapatilla coincida exactamente con las preferencias
recomendar_zapatilla_aux(Prefs, Nombre, Precio) :-
    member(actividad(Act), Prefs),
    member(presupuesto(Pre), Prefs),
    member(genero(Gen), Prefs),
    member(color(Col), Prefs),
    zapatilla(Nombre, Act, Pre, _, _, Colores, Gen, PrecioBase),
    member(Col, Colores),  % Color debe estar en la lista de colores disponibles
    (oferta(Nombre, Desc) -> Precio is PrecioBase * (1 - Desc/100) ; Precio = PrecioBase).

% Ordena zapatillas recomendadas por valoración
ordenar_recomendaciones(Zapatillas, Ordenadas) :-
    predsort(comparar_recomendaciones, Zapatillas, Ordenadas).

% Predicado de comparación personalizada por valoración
comparar_recomendaciones(>, A, B) :-
    zapatilla(A, _, _, ValA, _, _, _, _),
    zapatilla(B, _, _, ValB, _, _, _, _),
    ValA > ValB.
comparar_recomendaciones(<, _, _) :- !.

% Muestra una lista de recomendaciones al usuario
mostrar_recomendaciones([], _).
mostrar_recomendaciones([Nombre-Precio|Resto], Historial) :-
    \+ member(Nombre, Historial),  % Evita mostrar duplicados
    zapatilla(Nombre, Act, Pre, Val, Caract, Colores, Gen, _),
    format('\n=== ~w ===\n', [Nombre]),
    format('Actividad: ~w\n', [Act]),
    format('Precio: $~w (~w)\n', [Precio, Pre]),
    format('Valoración: ~1f/5.0\n', [Val]),
    format('Género: ~w\n', [Gen]),
    format('Colores: ~w\n', [Colores]),
    format('Características: ~w\n', [Caract]),
    (oferta(Nombre, Desc) -> 
        format(' OFERTA ESPECIAL: ~w% de descuento\n', [Desc]) ; true),
    mostrar_recomendaciones(Resto, [Nombre|Historial]).  % Agrega a historial y continúa

% Sugiere productos complementarios según actividad
sugerir_complementos(_, Usuario) :-  % Cambiamos [Nombre|_] a _
    usuario(Usuario, Prefs, _, _),
    member(actividad(Actividad), Prefs),  % Usa la actividad QUE EL USUARIO SELECCIONÓ
    findall(Comp, complemento(Actividad, Comp), Complementos),
    (   Complementos = [] ->
        writeln('\nNo hay complementos para esta actividad.')
    ;   writeln('\nProductos complementarios recomendados:'),
        forall(member(C, Complementos), format('- ~w\n', [C])),
        pedir_respuesta_historial(Usuario, Complementos)
    ).

% Pregunta al usuario si desea añadir un complemento al historial
pedir_respuesta_historial(Usuario, Complementos) :-
    writeln('\n¿Deseas añadir alguno a tu historial? (si/no):'),
    leer_linea(Resp),
    (   Resp == 'si' ->
        writeln('Ingresa el producto a añadir:'),
        leer_linea(Producto),
        (   member(Producto, Complementos) ->
            usuario(Usuario, Prefs, Hist, Feed),
            actualizar_usuario(Usuario, Prefs, [Producto|Hist], Feed),
            writeln('Producto añadido a tu historial.')
        ;   writeln('Producto no válido. Por favor, elige uno de los productos recomendados.'),
            pedir_respuesta_historial(Usuario, Complementos)
        )
    ;   Resp == 'no' ->
        writeln('No se añadió ningún producto a tu historial.')
    ;   writeln('Respuesta inválida. Por favor, ingresa "si" o "no".'),
        pedir_respuesta_historial(Usuario, Complementos)
    ).

% Busca zapatillas similares si no hay coincidencias exactas
encontrar_alternativas(Prefs) :-
    findall(Puntaje-Nombre-Precio, puntuar_alternativas(Prefs, Nombre, Puntaje, Precio), Puntuadas),
    sort(1, @>=, Puntuadas, Ordenadas),  % Ordenar por mayor puntaje
    tomar_primeras(3, Ordenadas, Mejores),
    mostrar_recomendaciones_alternativas(Mejores).

% Asigna puntajes a alternativas según cercanía con las preferencias
puntuar_alternativas(Prefs, Nombre, Puntaje, Precio) :-
    zapatilla(Nombre, Act, Pre, Val, _, Colores, Gen, PrecioBase),
    (member(actividad(ActPref), Prefs) -> (Act == ActPref -> P1 = 3 ; P1 = 0) ; P1 = 0),
    (member(presupuesto(PrePref), Prefs) -> (Pre == PrePref -> P2 = 2 ; P2 = 0) ; P2 = 0),
    (member(genero(GenPref), Prefs) -> (Gen == GenPref -> P3 = 1 ; P3 = 0) ; P3 = 0),
    (member(color(ColPref), Prefs) -> (member(ColPref, Colores) -> P4 = 1 ; P4 = 0) ; P4 = 0),
    (oferta(Nombre, Desc) -> 
        Precio is PrecioBase * (1 - Desc/100), P5 = 1 ; Precio = PrecioBase, P5 = 0),
    Puntaje is P1 + P2 + P3 + P4 + P5 + Val/2.

% Extrae los primeros N elementos de la lista
tomar_primeras(N, Lista, Primeras) :-
    length(Primeras, N),
    append(Primeras, _, Lista).

% Muestra recomendaciones alternativas
mostrar_recomendaciones_alternativas([]).
mostrar_recomendaciones_alternativas([_-Nombre-Precio|Resto]) :-
    zapatilla(Nombre, Act, Pre, Val, Caract, Colores, Gen, _),
    format('\n=== ~w ===\n', [Nombre]),
    format('Actividad: ~w\n', [Act]),
    format('Precio: $~w (~w)\n', [Precio, Pre]),
    format('Valoración: ~1f/5.0\n', [Val]),
    format('Género: ~w\n', [Gen]),
    format('Colores: ~w\n', [Colores]),
    format('Características: ~w\n', [Caract]),
    (oferta(Nombre, Desc) -> 
        format('OFERTA ESPECIAL: ~w% de descuento\n', [Desc]) ; true),
    mostrar_recomendaciones_alternativas(Resto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% HISTORIAL Y OFERTAS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Muestra la información del usuario: preferencias, historial y feedback
mostrar_historial(Usuario) :-
    usuario(Usuario, Prefs, Hist, Feed),  % Recupera los datos del usuario
    writeln('\n=== TUS PREFERENCIAS ==='),
    mostrar_lista(Prefs),                % Muestra las preferencias del usuario
    writeln('\n=== TU HISTORIAL ==='),
    mostrar_lista(Hist),                 % Muestra el historial de productos añadidos
    writeln('\n=== TU FEEDBACK ==='),
    mostrar_feedback(Feed).             % Muestra las valoraciones dadas por el usuario

% Muestra una lista elemento por elemento con viñetas
mostrar_lista([]).
mostrar_lista([H|T]) :-
    format('- ~w\n', [H]),               % Imprime cada elemento de la lista con formato
    mostrar_lista(T).                    % Recurre con el resto de la lista

% Muestra la lista de feedback del usuario con el producto y su puntuación
mostrar_feedback(Feed) :-
    forall(
        member(feedback(Prod, Punt), Feed),
        format('- ~w: ~w/5\n', [Prod, Punt])  % Imprime cada feedback con su puntuación
    ).

% Muestra todas las zapatillas que están en oferta con el descuento aplicado
mostrar_ofertas :-
    findall(
        Prod-Desc-Precio, 
        (
            oferta(Prod, Desc),                                     % Verifica que el producto esté en oferta
            zapatilla(Prod, _, _, _, _, _, _, PrecioBase),         % Obtiene el precio base
            Precio is PrecioBase * (1 - Desc/100)                  % Calcula el nuevo precio con el descuento
        ), 
        Ofertas
    ),
    (
        Ofertas \= [] ->
            writeln('\n OFERTAS ESPECIALES:'),                      % Si hay ofertas, las muestra
            forall(
                member(Prod-Desc-Precio, Ofertas),
                format('- ~w: ~w% OFF → $~w\n', [Prod, Desc, Precio])
            )
        ; 
            writeln('\nNo hay ofertas disponibles actualmente.')    % Si no hay, lo indica
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% FEEDBACK Y APRENDIZAJE %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Inicia el proceso para que el usuario brinde feedback sobre una zapatilla
recibir_feedback(Usuario) :-
    catch(
        (
            writeln('\nIngrese el nombre de la zapatilla:'),
            leer_linea(Producto),
            (zapatilla(Producto, _, _, _, _, _, _, _) ->
                pedir_calificacion(Producto, Usuario)      % Si la zapatilla existe, pide calificación
            ;
                writeln('Zapatilla no encontrada. Inténtalo de nuevo.'),
                recibir_feedback(Usuario)                  % Si no se encuentra, vuelve a intentar
            )
        ),
        _,  % Captura cualquier excepción
        (writeln('Error al leer la entrada. Intenta nuevamente.'), recibir_feedback(Usuario))
    ).

% Solicita una calificación del producto al usuario y actualiza su feedback
pedir_calificacion(Producto, Usuario) :-
    writeln('Calificación (1-5):'),
    (   leer_linea_num(Calificacion),
        between(1, 5, Calificacion)                         % Verifica que esté en el rango válido
    ->  usuario(Usuario, Prefs, Hist, Feed),
        (   member(feedback(Producto, _), Feed) ->
            writeln('Ya has dado feedback para esta zapatilla.')  % Evita feedback duplicado
        ;
            retract(usuario(Usuario, Prefs, Hist, Feed)),         % Elimina el registro anterior
            assertz(usuario(Usuario, Prefs, Hist, [feedback(Producto, Calificacion)|Feed])),  % Agrega el nuevo feedback
            writeln('Gracias por tu feedback!')
        )
    ;   writeln('Calificación inválida. Intenta nuevamente.'),
        pedir_calificacion(Producto, Usuario)              % Si el número no es válido, reintenta
    ).

% Actualiza las preferencias del usuario si la calificación fue alta
actualizar_preferencias(Producto, Calificacion, Prefs) :-
    Calificacion >= 4,                                     % Solo se actualiza si la calificación es 4 o 5
    zapatilla(Producto, Act, _, _, _, Colores, Gen, _),    % Extrae atributos del producto
    % Verifica y añade la actividad si no está ya en preferencias
    (member(actividad(_), Prefs) -> true ; 
        usuario(Usuario, Prefs, Hist, Feed),
        actualizar_usuario(Usuario, [actividad(Act)|Prefs], Hist, Feed)),
    % Verifica y añade el género si no está ya en preferencias
    (member(genero(_), Prefs) -> true ; 
        usuario(Usuario, Prefs, Hist, Feed),
        actualizar_usuario(Usuario, [genero(Gen)|Prefs], Hist, Feed)),
    % Verifica y añade colores si no están ya en preferencias
    (member(color(_), Prefs) -> true ; 
        member(Color, Colores),  % Toma uno de los colores disponibles
        usuario(Usuario, Prefs, Hist, Feed),
        actualizar_usuario(Usuario, [color(Color)|Prefs], Hist, Feed)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% VALIDACIONES %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Verifica si una actividad ingresada es válida
actividad_valida(Act) :- 
    member(Act, [running, crossfit, urbano, casual, trail, skate, entrenamiento]).

% Verifica si un nivel de presupuesto es válido
presupuesto_valido(Pre) :- 
    member(Pre, [economico, medio, premium]).

% Verifica si un género ingresado es válido
genero_valido(Gen) :- 
    member(Gen, [hombre, mujer, unisex]).

% Verifica si un color ingresado es válido
color_valido(Col) :- 
    member(Col, [negro, blanco, rojo, azul, gris, marron, verde, naranja, amarillo, dorado, plateado]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% UTILIDADES %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%

% Lee una línea del usuario como texto (conversión a minúsculas)
leer_linea(Valor) :-
    catch(
        (
            read_line_to_codes(user_input, Codes),
            ( Codes == end_of_file ->
                writeln('Entrada finalizada inesperadamente. Abortando...'),
                halt
            ;
                string_to_atom(Codes, Atom),
                downcase_atom(Atom, Valor)
            )
        ),
        _,
        (writeln('Error leyendo entrada. Abortando...'), halt)
    ).

% Lee una línea del usuario como número
leer_linea_num(Num) :-
    catch(
        (
            read_line_to_codes(user_input, Codes),
            number_codes(Num, Codes),
            number(Num)
        ),
        _,
        fail  % No detenemos el programa, simplemente fallamos para permitir reintento
    ).

:- discontiguous iniciar/0.

% Iniciar sistema
iniciar :-
    menu_principal(_).  % Usamos _ para indicar que no nos importa el valor