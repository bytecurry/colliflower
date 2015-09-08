(:docstring-markup-format :scriba
  :systems (:colliflower/ext
            ;; liter:
            :liter/base
            :liter/generate
            :liter/tools
            :liter/iter-object
            :liter/file
            ;; garten:
            :garten/grower
            :garten/base
            :garten/list
            ;; silo:
            :silo/protocol
            :silo/base)
  :documents ((:title "Colliflower"
               :authors ("Thayne McCombs")
               :output-format (:type :multi-html
                               :template :minima)
               :sources ("overview.scr"
                         "liter.scr"
                         "garten.scr"
                         "silo.scr"))))
