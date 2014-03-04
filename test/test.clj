(defn test []
  (let [x 100]
    (println)
    (let [y 200]
      (let [x 300]
        (let [x 4]
          (println :thing)
          (println :another)
          (println :test)
          (let [x 40]
            (let [x 30]
              (println :another)
              (println :test)
              (println :moo)
              )
            )
          )
        )
      )
    )
  )
