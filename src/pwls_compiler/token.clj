(ns pwls-compiler.token
  (:require 
   [clojure.set :as set]
   [clojure.string :as str]))

(defn gera-sequencia
  "Gera sequência de caracteres, ex. [a d] => a b c d"
  [de ate]
  (set (map char (range (int de) (inc (int ate))))))

(defn inicia-funcao? [c] (#{\(} c))

(defn finaliza-funcao? [c] (#{\)} c))

(defn numero? [c] ((gera-sequencia \0 \9) c))

(defn operador? [c] (#{\+ \- \\ \*} c))

(defn funcao-ou-id? 
  [c]
  ((set/union
    (gera-sequencia \A \Z)
    (gera-sequencia \a \z))
   c))

(defn texto? [c] (#{\"} c))

(defn funcao-ou-id 
  [palavra]
  (let [funcoes (seq '("se" "escreve" "leia" "declara"))]
    (if (some #{palavra} funcoes) 1 2)))

(defn prox-nao-valido
  [texto]
  (let [prox (.indexOf texto \space)]
    (if (= prox -1)
      (.indexOf texto \))
      prox)))

(comment (defn prox-nao-numerico
           [texto]
           (let [prox (min 
                       (filter #(> % 0) 
                               (map #(.indexOf texto %)
                                    (seq (set/union (gera-sequencia \A \Z) 
                                                    (gera-sequencia \a \z)
                                                    #{\space}
                                                    #{\)})))))]
             prox)))

(defn prox-quote
  [texto]
  (let [pos (.indexOf (rest texto) \")]
    (if (= pos -1)
      (throw (Exception. "Erro lexico, não foi encontrado o final do bloco de texto"))
      (+ 2 pos))))
