(ns clj-xml-validation.core
  (:require [clojure.java.io :as io]
            [clojure.set :as set])
  (:import [javax.xml XMLConstants]
           [org.xml.sax SAXException ErrorHandler SAXParseException]
           [javax.xml.validation SchemaFactory]
           [javax.xml XMLConstants]
           [javax.xml.transform.stream StreamSource]
           [java.io InputStream Reader File]
           [java.net URL]))

(defprotocol StreamSourcable
  (stream-source [this]))

(extend-protocol StreamSourcable
  java.io.InputStream
  (stream-source [is] (StreamSource. is))
  java.io.Reader
  (stream-source [rdr] (StreamSource. rdr))
  java.io.File
  (stream-source [file] (StreamSource. file))
  String
  (stream-source [s] (StreamSource. (io/input-stream (.getBytes s))))
  java.net.URL
  (stream-source [url] (StreamSource. (str url))))

(defn create-error-handler
  [errs]
  (proxy [ErrorHandler] []
    (error [exc] (swap! errs conj exc))
    (warning [exc] (swap! errs conj exc))
    (fatalError [exc] (swap! errs conj exc))))

(defn- validator-from-schemas
  [sources]
  (let [validator (-> (SchemaFactory/newInstance XMLConstants/W3C_XML_SCHEMA_NS_URI)
                      (.newSchema sources)
                      (.newValidator))]
    (doto validator
      (.setProperty XMLConstants/ACCESS_EXTERNAL_DTD "")
      (.setProperty XMLConstants/ACCESS_EXTERNAL_SCHEMA ""))))

(defn- sax-parse-exception->m
  [source exc]
  (->
    (bean exc)
    (set/rename-keys {:lineNumber :line-number
                      :columnNumber :column-number})
    (select-keys [:message :line-number :column-number])
    (assoc :source source)))

(defprotocol ValidationResult
  (valid? [this])
  (errors [this]))

(defrecord ValidResult []
  ValidationResult
  (valid? [_] true)
  (errors [_] nil))

(defrecord ValidationFailureResult [errors]
  ValidationResult
  (valid? [_] false)
  (errors [_] (vec errors)))

(defn create-validation-fn
  "Create a function that when called will validate an XML stream source"
  [& schemas]
  {:pre [(every? (partial satisfies? StreamSourcable) schemas)]}
  (let [sources (into-array StreamSource (map stream-source schemas))
        validator (validator-from-schemas sources) ;; do this here to ensure schemas are valid
        ]

    (fn [xmldoc]
      {:pre [(satisfies? StreamSourcable xmldoc)]}
      (try
        (let [errs (atom [])
              _ (.setErrorHandler validator (create-error-handler errs))]

          (.validate validator (stream-source xmldoc))

          (if (empty? @errs)
            (map->ValidResult {:source xmldoc})
            (map->ValidationFailureResult {:errors (mapv (partial sax-parse-exception->m xmldoc) @errs)})))
        (catch SAXException e
          (throw (ex-info "Exception while validating xml." {:source xmldoc :schemas sources} e)))))))
