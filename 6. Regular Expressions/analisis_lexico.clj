(ns analisis-lexico)

;(seq (slurp "entrada.txt"))
;
;(spit "salida.txt" "Una\nprueba")

(def er #"(?xi)

    ( -? \d+ \. \d* (?: e -? \d+)? )  # Grupo 1: Flotante
  | ( [a-z] \w* )                     # Grupo 2: Variable
  | ( // .* )                         # Grupo 3: Comentario
  | ( \d+ )                           # Grupo 4: Entero
  | ( [=] )                           # Grupo 5: Asignación
  | ( [+] )                           # Grupo 6: Suma
  | ( [-] )                           # Grupo 7: Resta
  | ( [*] )                           # Grupo 8: Multiplicación
  | ( [/] )                           # Grupo 9: División
  | ( [\^] )                          # Grupo 10: Potencia
  | ( [(]  )                          # Grupo 11: Paréntesis que abre
  | ( [)]  )                          # Grupo 12: Paréntesis que cierra
  | ( \s )                            # Grupo 13: Espacios
  | (.)                               # Grupo 14: Carácter inválido

")


;(def entrada (slurp "entrada.txt"))
;
;(re-seq er entrada)

(defn tokenize
  [input]
  (map (fn [token]
         (cond
           (token 1) [:flotante (token 0)]
           (token 2) [:variable (token 0)]
           (token 3) [:comentario (token 0)]
           (token 4) [:entero (token 0)]
           (token 5) [:asignacion (token 0)]
           (token 6) [:suma (token 0)]
           (token 7) [:resta (token 0)]
           (token 8) [:multiplicacion (token 0)]
           (token 9) [:division (token 0)]
           (token 10) [:potencia (token 0)]
           (token 11) [:par-izq (token 0)]
           (token 12) [:par-der (token 0)]
           (token 14) [:error (token 0)]))
       (remove (fn [v] (v 13)) (re-seq er input))))

(defn tokenize-file
  [file-name]
  (tokenize (slurp file-name)))

(defn separador
  []
  (println (apply str (repeat 56 \=))))

(def termino {:flotante       "Floatente"
              :variable       "Variable"
              :comentario     "Comentario"
              :entero         "Entero"
              :asignacion     "Asignación"
              :suma           "Suma"
              :resta          "Resta"
              :multiplicacion "Multiplicación"
              :division       "Division"
              :potencia       "Potencia"
              :par-izq        "Paréntesis que abre"
              :par-der        "Peréntesis que cierra"
              :error          "Token inválido"})

(defn print-table
  [file-name]
  (separador)
  (println (format "%-32sTipo" "Token"))
  (separador)
  (doseq [token (tokenize-file file-name)]
    (println (format "%-32s%s"
                     (token 1)
                     (termino (token 0)))))
  (separador))

; (spit "salida.txt" (with-out-str (print-table "entrada.txt")))

(def html-template (slurp "template.html"))

(defn htmlize
  [lst]
  (map (fn [[t v]]
         (format "<tr>
                     <td class=\"%s\">%s</td>
                     <td>%s</td>
                  </tr>
                  "
                 (symbol t)
                 v
                 (termino t)))
       lst))

(defn txt->html
  [in-name out-name]
  (let [file-content (slurp in-name)]
    (spit out-name
          (format html-template
                  file-content
                  (apply str (htmlize (tokenize file-content)))))))

(txt->html "entrada.txt" "resultado.html")
