(ns pwls-compiler.sintax
  (:require [pwls-compiler.code :as code]))

(def contador-parenteses (atom 0))

(defn inc-parenteses! 
  []
  (swap! contador-parenteses inc))

(defn dec-parenteses!
  []
  (if (< @contador-parenteses 0)
    (throw (Exception. "Final de função não esperado"))
    (swap! contador-parenteses dec)))

(defn valida-parenteses
  [npt]
  (println @contador-parenteses)
  (if (= 0 (:token npt))
    (inc-parenteses!))
  (if (= 9 (:token npt))
    (dec-parenteses!)))

(defn syntax-inicia-programa
  [npt npts pt pts]
  (code/gera-arquivo true)
  (if (= 0 (:token npt))
    {:np-tokens (vec npts) :p-tokens (conj [] npt)}
    (throw (Exception. "Erro de sintaxe. Esperado início do programa"))))

(defn syntax-finaliza-programa
  [npt npts pt pts]
  (code/finaliza-arquivo)
  (if (and (= 9 (:token npt)) (= 0 @contador-parenteses))
    {:np-tokens npts :p-tokens (conj pts pt npt)}
    (throw
     (Exception.
      (str "Erro de sintaxe. Esperado fim de programa, recebido: " (:content npt))))))

(defn syntax-inicia-funcao
  [npt npts pt pts]
  (if (= 0 (:token npt))
    {:np-tokens (vec npts) :p-tokens (conj pts pt npt)}
    (throw (Exception. "Erro de sintaxe. Esperado início de função"))))

(defn syntax-funcao
  [npt npts pt pts]
  (if (= 1 (:token npt))
    {:np-tokens (vec npts) :p-tokens (conj pts pt npt)}
    (throw
     (Exception. 
      (str "Erro de sintaxe. Esperado função, recebido: "  (:content npt))))))

(defn syntax-identificador-ou-funcao
  [npt npts pt pts]
  (if (or (= 2 (:token npt))
          (= 1 (:token npt))
          (= 0 (:token npt))
          (= 9 (:token npt))
          (= 3 (:token npt))
          (= 4 (:token npt)))
    {:np-tokens (vec npts) :p-tokens (conj pts pt npt)}
    (throw
     (Exception.
      (str "Erro de sintaxe. Esperado id ou função, recebido: " (:content npt))))))

(defn syntax-continua-programa
  [npt npts pt pts]
  (if (= 0 (:token pt))
    (syntax-funcao npt npts pt pts)
    (if (= 1 (:token pt))
      (syntax-identificador-ou-funcao npt npts pt pts)
      (if (= 9 (:token pt))
        (syntax-inicia-funcao npt npts pt pts)))))

(defn verifica-sintaxe
  "pt é o último token parseado, pts é a lista de todos os tokens que já foram parseados
  npt é o token sendo parseado, npts é a lista de tokens que ainda não foram parseados"
  [all-tokens]
  (let [[npt & npts] (:np-tokens all-tokens)
        [pt & pts] (:p-tokens all-tokens)]
    ;; Se pt esta vazio é porque o programa vai começar agora
    (valida-parenteses npt)
    (if (empty? pt)
      ;; Primeiro token é de início de função
      (syntax-inicia-programa npt npts pt pts)
      (if (empty? npts)
        (syntax-finaliza-programa npt npts pt pts)
        (syntax-continua-programa npt npts pt pts)))))

;; all-tokens é um mapa {:np-tokens [] :p-tokens []}
(defn analisa
  [all-tokens]
  (loop [tokens all-tokens]
    (println tokens)
    (if (empty? (:np-tokens tokens))
      (if (= tokens nil) 
        (throw (Exception. "Erro de sintaxe."))
        (println "Sintaxe OK"))
      (recur (verifica-sintaxe tokens)))))
