(ns pwls-compiler.lexical
  (:require 
   [pwls-compiler.token :as token]
   [clojure.string :as str]))

;; 0 -> Inicio da função
;; 1 -> Funcao
;; 2 -> Identificador
;; 3 -> Texto
;; 4 -> Número
;; 9 -> Fim da função

(defn file->seq!
  [file]
  (for [item (slurp file)] item))

(defn scan-inicio
  [codigo]
  (let [[c & cs] (:source codigo)
        tokens (:tokens codigo)]
    (assoc codigo 
           :source cs
           :tokens (conj tokens {:token 0 :content c}))))

(defn scan-fim
  [codigo]
  (let [[c & cs] (:source codigo)
        tokens (:tokens codigo)]
    (assoc codigo
           :source cs
           :tokens (conj tokens {:token 9 :content c}))))

(defn scan-operador
  [codigo]
  (let [[c & cs] (:source codigo)
        tokens (:tokens codigo)]
    (assoc codigo
           :source cs
           :tokens (conj tokens {:token 5 :content c}))))

(defn scan-funcao-ou-id
  [codigo]
  (let [source (:source codigo)
        tokens (:tokens codigo)
        stop-at (token/prox-nao-valido source)
        palavra (str/join (take stop-at source))]
    (assoc codigo
           :source (drop (count palavra) source)
                                        ;:source (rest source)
           :tokens (conj tokens {:token (token/funcao-ou-id palavra) :content palavra}))))

(defn scan-texto
  [codigo]
  (let [source (:source codigo)
        tokens (:tokens codigo)
        stop-at (token/prox-quote source)
        texto (str/join (take stop-at source))]
    (assoc codigo
           :source (drop (count texto) source)
           :tokens (conj tokens {:token 3 :content texto}))))

(defn scan-numero
  [codigo]
  (let [source (:source codigo)
        tokens (:tokens codigo)
        stop-at (token/prox-nao-valido source)
        numero  (str/join (take stop-at source))]
    (assoc codigo
           :source (drop (count numero) source)
           :tokens (conj tokens {:token 4 :content numero}))))

;codigo é o mapa composto pelo fonte carregado e os tokens {:source fonte :tokens []}
;tokens é um vetor de mapas da forma [{:token 0 :content \(} {:token 2 :content \a}]
(defn escaneia!
  [codigo]
  (let [[c & cs] (:source codigo)
        tokens (:tokens codigo)]
    (cond
      (str/blank? (str c)) (assoc codigo :source cs)
      (token/inicia-funcao? c) (scan-inicio codigo)
      (token/finaliza-funcao? c) (scan-fim codigo)
      (token/funcao-ou-id? c) (scan-funcao-ou-id codigo)
      (token/numero? c) (scan-numero codigo)
      (token/operador? c) (scan-operador codigo)
      (token/texto? c) (scan-texto codigo)
      :else (assoc codigo :source cs)
      )))

(defn tokenize!
  "Cria um mapa com todos os tokens (array vazio) e o código fonte sequenciado
  a cada iteração os código fonte é diminuido e transformado em token."
  [source]
  (loop [codigo {:tokens [] :source source}]
    (if (empty? (:source codigo))
      (:tokens codigo)
      (recur (escaneia! codigo)))))
