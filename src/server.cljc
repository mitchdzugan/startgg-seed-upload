; import { GoogleSpreadsheet } from 'google-spreadsheet';
; import { JWT } from 'google-auth-library';

(ns server
  (:require ["express" :as express]
            ["fs" :as fs]
            ["request-curl" :as curl]
            ["google-auth-library" :as importGoogleAuthLibrary]
            ["google-spreadsheet" :as importGoogleSpreadsheet]
            [allpa.core :as a]
            [cljs.reader :as reader]
            [router :as r]
            [globals :as g]
            [ui.entry]
            [mayu.dom :as dom]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.async :refer [go-loop <!]]))

(def JWT (aget importGoogleAuthLibrary "JWT"))
(def Sheet (aget importGoogleSpreadsheet "GoogleSpreadsheet"))
(def auth (JWT. #js {:email "sheets-processor@gg-seed-uploader.iam.gserviceaccount.com"
                     :key js/process.env.GOOGLE_SERVICE_KEY
                     :scopes #js ["https://www.googleapis.com/auth/spreadsheets"]}))


(defn fetch [url ^js res]
  (.then (curl #js {:url url})
         (fn [^js curld]
           (case (.-statusCode curld)
             200 (.json res #js{:csv (.-body curld)})
             307 (fetch (.. curld -headers -Location) res)
             404 (.json res #js{:error "Sheet does not exist"})
             (.json res #js{:error "Sheet is not public"})))))


(defn pfetch [url]
  (.then (curl #js {:url url})
         (fn [^js curld]
           (case (.-statusCode curld)
             200 (.-body curld)
             307 (pfetch (.. curld -headers -Location))
             nil))))

(defn get-sheet-data [id ^js sheet]
  (let [sheet-id (.-sheetId sheet)
        sheet-title (.-title sheet)
        url (str "https://docs.google.com/spreadsheets/d/"
                  id
                  "/export?format=csv&gid="
                  sheet-id)]
    (.then (pfetch url)
      (fn [csv]
        [sheet-title csv]))))

(defn dl-sheet [id ^js res]
  (let [url (str "https://docs.google.com/spreadsheets/d/"
                  id
                  "/export?format=csv")
        ^js doc (Sheet. id auth)]
    (.then (.loadInfo doc)
      (fn [data]
        (.then 
          (js/Promise.all (.map (js/Object.values (.-sheetsByIndex doc))
                                (fn [^js sheet] (get-sheet-data id sheet))))
          (fn [tabs]
            (let [tabdata #js {}]
              (doseq [[name csv] tabs]
                (aset tabdata name csv))
              (js/console.log tabdata)
              (.json res tabdata))))))))

(def dev?
  (let [debug? (do ^boolean js/goog.DEBUG)]
    (if debug?
      (cond
        (exists? js/window) true
        (exists? js/process) (not= "true" js/process.env.release)
        :else true)
      false)))

(def assets
  (if dev?
    {:css-name "site.css"
     :output-name "client.js"}
    (-> "./dist/assets.edn"
        (fs/readFileSync "utf8")
        reader/read-string
        first)))

(def css-name (:css-name assets))

(def output-name (:output-name assets))

(def page-pre (str"
<!DOCTYPE html>
<html>
   <head>
      <meta charset=\"utf-8\">
      <meta content=\"width=device-width, initial-scale=1\" name=\"viewport\">
      <link href=\"/" css-name "\" rel=\"stylesheet\" type=\"text/css\">
      <title>ggseed</title>
      <link
          href=\"https://kit-free.fontawesome.com/releases/latest/css/free-v4-shims.min.css\"
          media=\"all\"
          rel=\"stylesheet\"
      >
      <link
          href=\"https://kit-free.fontawesome.com/releases/latest/css/free-v4-font-face.min.css\"
          media=\"all\"
          rel=\"stylesheet\"
      >
      <link
          href=\"https://kit-free.fontawesome.com/releases/latest/css/free.min.css\"
          media=\"all\"
          rel=\"stylesheet\"
      >
      <script
          src=\"https://kit.fontawesome.com/80071b01f7.js\"
          crossorigin=\"anonymous\"
      ></script>
   </head>
   <body>
      <div id=\"app\">
"))

(def page-post (str"
      </div>
      <script src=\"/" output-name "\" type=\"text/javascript\"></script>
   </body>
</html>
"))

(defn stream-route [res route]
  (.type res "html")
  (.write res page-pre)
  (let [{:keys [signal off]}
        (s/build (s/from route e/never))
        markup-channel (dom/render-to-string {:gets #(-> nil)
                                              :sets #(-> nil)
                                              :subs #(-> nil)
                                              :ssr? true
                                              ::r/s-route signal} ui.entry/root)]
    (go-loop []
      (let [markup (<! markup-channel)]
        (if (nil? markup)
          (do (.write res page-post)
              (.end res))
          (do (.write res markup)
              (recur)))))))

(def app (express))

(def client-secret (.. js/process -env -CLIENT_SECRET))

(defn token-auth [code ^js res]
  (.then (curl #js {:url "https://api.start.gg/oauth/access_token"
                    :json true
                    :method "POST"
                    :body #js {:grant_type "authorization_code"
                               :client_secret client-secret
                               :code code
                               :scope g/scope
                               :client_id g/client-id
                               :redirect_uri g/redirect-uri}})
         (fn [^js curld]
           (if (not= js/String (type (.-body curld)))
             (.json res (.-body curld))
             (-> res (.status 400) (.send "Invalid code"))))))


(defn main! []
  (.get app #"/api_oauth"
        (fn [req res]
          (let [code (.. req -query -code)]
            (token-auth code res))))
  (.get app #"/dl-sheet"
        (fn [req res]
          (let [id (.. req -query -sheet)
                url (str "https://docs.google.com/spreadsheets/d/"
                         id
                         "/export?format=csv")]
            (dl-sheet id res))))
  (.get app #".*"
        (fn [req ^js res next]
          (let [url (.-url req)
                route (r/match-by-path url)]
            (if route
              (stream-route res route)
              (next)))))
  (.use app (.static express (if dev? "target" "dist")))
  (let [port (if (some? js/process.env.PORT)
               (js/parseInt js/process.env.PORT)
               3010)]
    (.listen app port #(println (str "App listening on port " port)))))

(defn reload! []
  (println "Code updated."))
