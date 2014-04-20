(ns dsl.core
  (:require [clojure.walk :as walk]))

(def cal (java.util.Calendar/getInstance))
(def today (java.util.Date.))
(def yesterday (do (.add cal java.util.Calendar/DATE -1) (.getTime cal)))
(def tomorrow (do (.add cal java.util.Calendar/DATE 2) (.getTime cal)))
(defn one [] 1)

(comment
  (defn one [] 1)
  
  ;; Примеры вызова
  (with-datetime
    (if (> today tomorrow) (println "Time goes wrong"))
    (if (<= yesterday today) (println "Correct"))
    (let [six (+ 1 2 3)
          d1 (today - 2 days)
          d2 (today + 1 week)
          d3 (today + six months)
          d4 (today + (one) year)]
      (if (and (< d1 d2)
               (< d2 d3)
               (< d3 d4))
        (println "DSL works correctly")))))


;; Поддерживаемые операции:
;; > >= < <=
;; Функция принимает на вход три аргумента. Она должна определить,
;; являются ли второй и третий аргумент датами. Если являются,
;; то из дат необходимо взять date.getTime и сравнить их по этому числу.
;; Если получены не даты, то выполнить операцию op в обычном порядке:
;; (op d1 d2).
(defn d-op [op d1 d2]
  (let [v1 (if (instance? java.util.Date d1) (.getTime d1) d1)
        v2 (if (instance? java.util.Date d2) (.getTime d2) d2)]
    (op v1 v2)))

(defn is-date-comp? [args]
  (and (= (count args) 3)
       (contains? #{'> '< '>= '<=} (first args))))
;; Пример вызова:
;; (d-add today '+ 1 'day)
;; Функция должна на основе своих параметров создать новую дату.
;; Дата создается при помощи календаря, например так:
;; (def cal (java.util.Calendar/getInstance))
;; (.add cal java.util.Calendar/DATE 2)
;; (.getTime cal)
;; Во-первых, необходимо на основе 'op' и 'num' определить количество, на
;; которое будем изменять дату. 'Op' может принимать + и -, соответственно
;; нужно будет не изменять либо изменить знак числа 'num'.
;; Во-вторых, необходимо узнать период, на который будем изменять дату.
;; Например, если получили 'day, то в вызове (.add cal ...) будем использовать
;; java.util.Calendar/DATE. Если получили 'months, то java.util.Calendar/MONTH.
;; И так далее.
;; Результат работы функции - новая дата, получаемая из календаря так: (.getTime cal)
(def period-mapping
  {'day java.util.Calendar/DAY_OF_MONTH
   'days java.util.Calendar/DATE
   'week java.util.Calendar/WEEK_OF_MONTH
   'weeks java.util.Calendar/WEEK_OF_MONTH
   'month java.util.Calendar/MONTH
   'months java.util.Calendar/MONTH
   'year java.util.Calendar/YEAR
   'years java.util.Calendar/YEAR
   'hour java.util.Calendar/HOUR
   'hours java.util.Calendar/HOUR
   'minute java.util.Calendar/MINUTE
   'minutes java.util.Calendar/MINUTE
   'second java.util.Calendar/SECOND
   'seconds java.util.Calendar/SECOND})

(defn d-add [date op num period]
  (let [cal (java.util.Calendar/getInstance)
        val (if (= op '-) (- 0 num) num)]
    (do (.setTime cal date)
        (.add cal (get period-mapping period) val)
        (.getTime cal))))

;; Можете использовать эту функцию для того, чтобы определить,
;; является ли список из 4-х элементов тем самым списком, который создает новую дату,
;; и который нужно обработать функцией d-add.
(defn is-date-op? [code]
  (let [op (second code)
        period (last code)]
    (and (= (count code) 4)
         (or (= '+ op)
             (= '- op))
         (contains? #{'day 'days 'week 'weeks 'month 'months 'year 'years
                      'hour 'hours 'minute 'minutes 'second 'seconds} period ))))

;; В code содержится код-как-данные. Т.е. сам code -- коллекция, но его содержимое --
;; нормальный код на языке Clojure.
;; Нам необходимо пройтись по каждому элементу этого кода, найти все списки из 3-х элементов,
;; в которых выполняется сравнение, и подставить вместо этого кода вызов d-op;
;; а для списков из четырех элементов, в которых создаются даты, подставить функцию d-add.
(defmacro magic-d-add [date op num period]
  `(d-add ~date '~op ~num '~period))

(defmacro with-datetime [& code]
  (let [template
        (walk/postwalk
         (fn [exp]
             (if (seq? exp)
               (cond
                (is-date-op? exp) `(magic-d-add ~@exp)
                (is-date-comp? exp) `(d-op ~@exp)
                :else exp)
               exp))
         code)]
  `(do ~@template)))


(defn -main [& args]
  (with-datetime
    (if (> today tomorrow)
      (println "Time goes wrong"))
    (if (<= yesterday today)
      (println "Correct"))
    (let [six (+ 1 2 3)
          d1 (today - 2 days)
          d2 (today + 1 week)
          d3 (today + six months)
          d4 (today + (one) year)]
      (if (and (< d1 d2)
               (< d2 d3)
               (< d3 d4))
        (println "DSL works correctly")
        (println "DSL fail")))))
