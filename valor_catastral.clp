;#############################################################################################################################
;###    PRACTICA 1: FUZZYCLIPS                                                                                             ###
;###                                                                                                                       ###
;###    AUTORES: Álvaro Rodríguez Sánchez                                                                                  ###
;###             Vladyslav Mazurkevich                                                                                     ###
;#############################################################################################################################

;####################################################################################
;##                             FUZZIFY FUNCTION                                   ##
;####################################################################################
(deffunction fuzzify (?fztemplate ?value ?delta)
    (bind ?low (get-u-from ?fztemplate))
    (bind ?hi  (get-u-to   ?fztemplate))

    (if (<= ?value ?low) then
        (assert-string
        (format nil "(%s (%g 1.0) (%g 0.0))" ?fztemplate ?low ?delta))
    else (if (>= ?value ?hi) then
        (assert-string
            (format nil "(%s (%g 0.0) (%g 1.0))" 
                ?fztemplate (- ?hi ?delta) ?hi)
            )
        else
            (assert-string
                (format nil "(%s (%g 0.0) (%g 1.0) (%g 0.0))"
                    ?fztemplate (max ?low (- ?value ?delta))
                    ?value (min ?hi (+ ?value ?delta))
                )
            )
        )
    )
)

;####################################################################################
;##                                  TEMPLATES                                     ##
;####################################################################################
;DEFINITION OF casa CLASS
(deftemplate casa
    (slot nombre (type SYMBOL))
    (slot Categoria-Vivienda (type INTEGER))
    (slot Edad_Aparente (type INTEGER))
    (slot vue_momentum (type FLOAT))
    (slot vue_maximum (type FLOAT))
    (slot n_ventanas (type INTEGER))

)

;DECLARATION OF FUZZY INPUT VARIABLE Categoria-Vivienda
(deftemplate Categoria-Vivienda
    0 150 puntos
    (
        (Economica (40 1) (70 0))
        (Estandar (40 0) (70 1) (100 0))
        (Intermedia (70 0) (100 1) (130 0))
        (Alta (100 0) (130 1))
    )
)

;DECLARATION OF FUZZY INPUT VARIABLE Edad_Aparente
(deftemplate Edad_Aparente
    0 100 anyos
    (
        (Reciente (0 1) (12 0))
        (Nuevo (0 0) (12 1) (24 0))
        (Medio (24 0) (36 1) (48 0))
        (Viejo (48 0) (60 1))
    )
)

;DECLARATION OF FUZZY OUTPUT VARIABLE VUE-DIF
(deftemplate VUE-DIF 
    0 10000 unidad_x_superficie
    (
        (Bajisimo (500 1) (1500 0))
        (Bajo (500 0) (1500 1) (2500 0))
        (Medio (2500 0) (3500 1) (4500 0))
        (Alto (4500 0) (5500 1) (6500 0))
        (Altisimo (5500 0) (6500 1))
    )
)

;####################################################################################
;##                                    ASSERTS                                     ##
;####################################################################################
(deffunction leerconsola ()
    (reset)

    (printout t "Introduzca el nombre de la vivienda (Ej. V1): " crlf)
    (bind ?Rnombre (read))
    (assert(nombre ?Rnombre))

    (printout t "Introduzca puntuacion: " crlf)
    (bind ?Rpuntuacion (read))
    (fuzzify Categoria-Vivienda ?Rpuntuacion 0)

    (printout t "Introduzca tipo edad: " crlf)
    (bind ?Redad (read))
    (fuzzify Edad_Aparente ?Redad 0)

    (printout t "Introduzca numero ventanas: " crlf)
    (bind ?Rventanas (read))
    (assert(n_ventanas ?Rventanas))
    
    ;;(run)
    
)

;####################################################################################
;##                                    RULES                                       ##
;####################################################################################
;; REGLA 1
(defrule cat-alta_eda-reciente
    (Categoria-Vivienda Alta)
    (Edad_Aparente Reciente)
    =>
    (assert (VUE-DIF Altisimo))
)

;; REGLA 2
(defrule cat-alta_eda-niMedio_niViejo
    (Categoria-Vivienda Alta)
    (Edad_Aparente not [ Medio or Viejo ])
    =>
    (assert (VUE-DIF Alto))
)

;; REGLA 3
(defrule cat-alta_eda-niReciente_niNuevo
    (Categoria-Vivienda Alta)
    (Edad_Aparente not [ Reciente or Nuevo ])
    =>
    (assert (VUE-DIF Medio))
)

;; REGLA 4
(defrule cat-intermedio_eda-nuevo
    (Categoria-Vivienda Intermedia)
    (Edad_Aparente Nuevo)
    =>
    (assert (VUE-DIF not [ Medio or Alto ]))
)

;;REGLA 5
(defrule cat-intermedio_eda-niMedio_niViejo
    (Categoria-Vivienda Intermedia)
    (Edad_Aparente not [ Medio or Viejo ])
    =>
    (assert (VUE-DIF Bajo))
)

;;REGLA 6 
(defrule cat-estandar_eda-nue
    (Categoria-Vivienda Estandar)
    (Edad_Aparente Nuevo)
    =>
    (assert (VUE-DIF Medio))
)

;;REGLA 7
(defrule cat-estandar_eda-viejo
    (Categoria-Vivienda Estandar)
    (Edad_Aparente Viejo)
    =>
    (assert (VUE-DIF Bajisimo))
)

;;REGLA 8
(defrule cat-economica_eda-nue
    (Categoria-Vivienda Economica)
    (Edad_Aparente Nuevo)
    =>
    (assert (VUE-DIF not [ Bajo or Medio ]))
)

;;REGLA 9
(defrule cat-economica_eda-niRecienteniNueva
    (Categoria-Vivienda Economica)
    (Edad_Aparente not [ Reciente or Nuevo ])
    =>
    (assert (VUE-DIF Bajisimo))
)

;;REGLA VENTANAS
(defrule menos_tres_ventanas
    (n_ventanas ?vent)
    (test(< ?vent 3))
    =>
    (assert (VUE-DIF more-or-less Bajo))
)
    
(defrule mas_cinco_ventanas
    (n_ventanas ?vent)
    (test(> ?vent 5))
    =>
    (assert (VUE-DIF very Alto))    
)

;;REGLA DEFUSIFICACION
(defrule defuzzificar
    (declare (salience -1))
    (VUE-DIF ?val)
    =>
    (assert(crisp vue_maximum (maximum-defuzzify ?val)))
    (assert(crisp vue_momentum (moment-defuzzify ?val)))

    (printout t "(Moment) El valor por la unidad de superficie es " (moment-defuzzify ?val) crlf)
    (printout t "(Maximum) El valor por la unidad de superficie es " (maximum-defuzzify ?val) crlf)
    (halt)
)