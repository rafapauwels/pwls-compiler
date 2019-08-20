(ns pwls-compiler.code)

(def path "/home/pauwels/Desktop/out.c")

(defn gera-arquivo
  [usa-stdio]
  (spit path (if usa-stdio "#include <stdio.h>\n" ""))
  (spit path "void main() {\n" :append true))

(defn finaliza-arquivo
  []
  (spit path "}" :append true))

(defn grava-se
  []
  (spit path "if (" :append true))

(defn grava-declara
  []
  (spit path "double"))

(defn gera-funcao
  [conteudo]
  (cond
    (= conteudo "se") (grava-se)
    (= conteudo "declara") (grava-declara)))

(defn gera-codigo
  [npt]
  (cond
    (= 0 (:token npt)) nil
    (= 1 (:token npt)) (gera-funcao (:content npt))))
