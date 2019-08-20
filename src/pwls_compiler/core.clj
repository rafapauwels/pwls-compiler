(ns pwls-compiler.core
  (:gen-class)
  (:require 
   [pwls-compiler.lexical :as lexical]
   [pwls-compiler.sintax :as sintax]))

(defn -main
  [& args]
  (let [arquivo "/home/pauwels/development/pwls-compiler/sample.pwls"
        arquivo-sequenciado (lexical/file->seq! arquivo)
        tokens (lexical/tokenize! arquivo-sequenciado)
        all-tokens {:np-tokens tokens :p-tokens []}]
    (sintax/analisa all-tokens)))
