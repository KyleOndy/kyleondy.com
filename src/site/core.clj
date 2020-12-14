(ns site.core)
 (use 'hiccup.core)

;; defining data for the menu
(def menu-items
  ["Crab Rangoon"
   "Pot Stickers"
   "Miso Soup"])

;; creating a function to generate a list elment from the provided `menu-item`
(defn create-menu-item [menu-item]
  [:li.menu-item menu-item])

;; generating an unordered list by mapping over the `menu-items` data
(def menu
  [:ul
   (map create-menu-item menu-items)])
