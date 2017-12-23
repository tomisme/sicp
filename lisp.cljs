(ns sicp.lisp
  (:require
   [devcards.core :refer-macros [defcard]]))


(defn self-evaluating?
  [exp]
  (cond
    (number? exp) true
    (string? exp) true
    :else false))


(defn variable?
  [exp]
  (symbol? exp))


(defn enclosing-environment
  [env]
  (rest env))


(defn first-frame
  [env]
  (first env))


(def the-empty-environment
  '())


(defn make-frame
  [variables values]
  (zipmap variables values))


(defn frame-variables
  [frame]
  (keys frame))


(defn frame-values
  [frame]
  (vals frame))


(defn add-binding-to-frame!
  [var val frame])


(defn extend-environment
  [vars vals base-env]
  (if (= (count vars) (count vals))
    (conj (make-frame vars vals) base-env)
    (throw "Couldn't extend environment")))


(defn lookup-variable-value
  [var env]
  (loop [curr-env env]
    (if (= curr-env the-empty-environment)
      (throw "Can't lookup unbound variable")
      (let [frame (first-frame env)]
        (if (contains? frame var)
          (get frame var)
          (recur (enclosing-environment curr-env)))))))


(defn define-variable!
  [var value env])


(defn set-variable-value!
  [var value env]
  (loop [curr-env env]
    (if (= curr-env the-empty-environment)
      (throw "Can't set unbound variable")
      (let [frame (first-frame env)]
        (if (contains? frame var)
          :?
          (recur (enclosing-environment curr-env)))))))


(defn apply-procedure
  [procedure arguments]
  (cond
    ; (primitive-procedure? procedure)
    ; (apply-primitive-procedure procedure arguments)

    ; (compound-procedure? procedure)
    ; (eval-sequence
    ;    (procedure-body procedure)
    ;    (extend-environment
    ;       (procedure-parameters procedure)
    ;       arguments
    ;       (procedure-environment procedure)))

    :else :error/unknown-procedure-type))


(defn eval
  [exp env]
  (cond
    (self-evaluating? exp)
    exp

    (variable? exp)
    (lookup-variable-value exp env)

    ; (quoted? exp)
    ; (text-of-quotation exp)

    ; (assignment? exp)
    ; (eval-assignment exp env)

    ; (definition? exp)
    ; (eval-definition exp env)

    ; (if? exp)
    ; (eval-if exp env)

    ; (lambda? exp)
    ; (make-procedure (lambda-parameters expr)
    ;                 (lambda-body exp)
    ;                 env)

    ; (begin? exp)
    ; (eval-sequence (begin-actions exp) env)

    ; (cond? exp)
    ; (eval (eval (cond->if exp) env))

    ; (application? exp)
    ; (apply-procedure (eval (operator exp) env)
    ;                  (list-of-values (operands exp) env))

    :else :error/unknown-expression-type))


(def test-environment
  {})


(def examples
  '("a"
    'a
    ''a
    :a))


(defcard eval
  (map #(vector % (eval % test-environment)) examples))
