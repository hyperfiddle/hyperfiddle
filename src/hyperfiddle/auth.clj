;; Meant to be AOTed, do not ship source.
(ns hyperfiddle.auth
  (:require
   [hyperfiddle.jwt :as jwt]))

(def PUBKEY (jwt/->pub-key "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAmARaIb1h5BKIurcDMHiSIXl5HsmeelvKQ4mtgBsg5E/NSSWTFUfsW9VI9bYY1lDRPugWldFiBQ6E5CI/2wJOt9vv8uU9fdPts6EOLL39J0PFb4A1dadI3qhhXypm3BurV8iS49gxCPkrc0XexMCnAlo+j04quX/DpkcysjWqsSuB2Kf2hKcd7kz81MebhN9ke5ZxeYLHpeq6vP1/zBEwRLaQeV7sMuVDzWShdcC4BbfjcjEELyTE8xi4rXKo0wLTp7utO9KhkLa1tCPl/Fflzl1jdgRbrOxxnRxWYAPWdBfHFDmLAcnN4waUQHNxVo6fpla9hM03oFD8bdo3jQkC9QIDAQAB"))
