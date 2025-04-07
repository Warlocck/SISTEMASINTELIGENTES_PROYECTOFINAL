% BASE DEL CONOCIMIENTO

% Zapatillas
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

% Ofertas especiales
oferta(nike_air_max, 15). % 15% de descuento
oferta(adidas_ultraboost, 10).
oferta(reebok_nano, 20).

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