;Katherine Chung, kchung13
;I tried, Clojure is just not my friend and doesn't want to work for me


(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:house {:desc "You are currently standing inside your house, in front of the front door.
  You hear your dad humming in the kitchen, preparing food for the big football game. It smells really good, and you're tempted to go see if you can eat some of the food before the game starts. "
           :title "in your house"
           :dir {:north :playground, :west :apple-orchard, :east :garden, :south :diner}
           :contents #{:hot-dogs, :hamburgers, :caesar-salad}}
   :school {:desc "Your old high school seems to have gotten renovated since the last time you were here. The lockers are no longer mostly dented, and the walls are freshly painted. "
              :title "in your old high school"
              :dir {:west :playground, :east :football-stadium, :north :aquarium, :south :garden}
              :contents #{}}
  :apple-orchard {:desc "Looks like you've found the local apple orchard. Some trees still seem to have some fruit on it. At any rate, it looks like
  a nice little place. "
             :title "in the apple orchard"
             :dir {:east :house, :north :carnival, :south :hospital}
             :contents #{:apple}}
  :football-stadium {
              :state "dark"
          ;    (if (= :state "dark")
              :desc "The stadium is completely dark. There are a bunch of people crowded around a circuit box, trying to figure out how to turn on the lights before the game starts"
          ;    (:desc "The lights are all lit up, ready for the game. "))
             :title "at the football stadium"
             :dir {:east :university, :west :school, :south :bank, :north :pet-store}
             :contents #{}}
  :farmhouse {:desc "You stand in the doorway of the decrepit farmhouse. It is clearly abandoned, and hasn't been fixed up in a while. You notice, however, the hay has been disturbed, so you aren't the only one that's been here recently. 4 "
             :title "in the farmhouse"
             :dir {:south :power-plant, :west :diner, :north :garden}
             :contents #{}}
  :playground {:desc "You see tiny children half your size scrambling around on the playground equipment. The swings are a little squeaky, and so every time someone decides to become a human pendulum, the screech of metal against metal fills the air, and part of your soul dies. "
             :title "at the playground"
             :dir {:east :school, :south :house, :west :carnival}
             :contents #{}}
  :garden {:desc "There are plants. And weeds. More weeds than plants at this point. "
             :title "at the garden"
             :dir {:west :house, :east :bank, :south :farmhouse, :north :school}
             :contents #{:rotten-tomatoes, :dandelions, :carrots, :peas, :overripe-blueberries}}
  :carnival {:desc "The rides are overpriced, and so is the food, and the place is crawling with little kids, but nothing beats carnival cotton candy and funnel cake. "
             :title "at the carnival"
             :dir {:east :playground, :north :beach, :south :apple-orchard}
             :contents #{:cotton-candy, :funnel-cake}}
  :diner {:desc "The smell of cooking burger patties makes your mouth water. The place definitely shows off the classic American diner decor, with plenty of booths and red-checkered tablecloths. 5"
             :title "in the diner"
             :dir {:east :farmhouse, :north :house, :west :hospital, :south :observatory}
             :contents #{:burger, :fries, :soda}}
  :bank {:desc "The bank tellers are at the counter, waiting for customers to make their money transactions. You feel a little odd standing in here without any money to deposit. "
             :title "at the bank"
             :dir {:east :concert-hall, :west :garden, :north :football-stadium}
             :contents #{:lollipop}}
  :hospital {:desc "You've really only been in the hospital twice. Once when you were born, and once when you fell off your bike and got a concussion. The place is clean, but not that cheery. "
             :title "at hospital"
             :dir {:north :apple-orchard, :east :diner}
             :contents #{:apple-sauce}}
  :observatory {:desc "The place is long abandoned, and nature has partially claimed the building, but the structure is still standing. Inside, the high domed ceiling is painted with the galaxies. pfdc"
             :title "at the observatory"
             :dir {:east :power-plant, :north :diner, :south :chocolate-factory}
             :contents #{}}
  :aquarium {:desc "There's so many colorful fish and sea creatures. You wonder if any of them have ever successfully escaped."
             :title "at the aquarium"
             :dir {:south :school, :east :pet-store}
             :contents #{:chips}}
  :power-plant {
              :state "locked"
            ;  (if (= :state "locked")
              :desc "The power-plant's gate is locked shut, and the high fence around the perimeter discourages you from entering. 3"
          ;    (:desc "The power-plant is unlocked. You go inside and turn on the power. Yay, game is saved."))
             :title "at the power-plant"
             :dir {:north :farmhouse, :west :observatory}
             :contents #{}}
  :beach {:desc "There's water, and there's sand. Yay."
             :title "at the beach"
             :dir {:south :carnival}
             :contents #{:seashell, :broken-bottles}}
  :concert-hall {:desc "The concert-hall is big and grand, and the acoustics inside the auditorium are amazing. THe walls are colorful mosiacs and classic roman columns. Yay for classical music. "
             :title "in the concert hall"
             :dir {:north :university, :west :bank}
             :contents #{:chocolate-bar, :truffle}}
  :chocolate-factory {:desc "When you were younger, you thought it was like Willy Wonka's chocolate factory. Now that you're older, it's just a regular old factory, with a lot more chocolate wrappers that there normally is. 2"
             :title "at the chocolate factory"
             :dir {:north :observatory}
             :contents #{:chocolate-bar}}
  :university {:desc "A college student's life consists of lack of sleep, too much stress, and death from finals. "
             :title "at the university"
             :dir {:south :concert-hall, :west :football-stadium}
             :contents #{:homework}}
  :pet-store {:desc "Aren't the puppies and kittens so cute! It does smell like kitty litter....and pet food....and occasionally like wet dog, but the animals are cute. "
             :title "at the pet store"
             :dir {:south :football-stadium, :west :aquarium}
             :contents #{:dog, :cat, :fish, :gecko, :mouse, :tarantula, :crickets}}
  })



(def adventurer
  {:location :house
   :inventory #{}
   :energy 5
   :tick 0
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

;go to different location
(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in player [:location] dest
        update-in player [:energy] dec)))) ;decrease energy by 1 when go to a different location

; lists out items that are in current location
(defn inspect [player]
  (let [location (player :location)
      things (->> the-map location :contents)]
      (print "In the room: ")
      (println things)
      ))

;lists out items that have been collected
(defn check-inventory [player]
  (let [things (player :inventory)]
  (if (nil? things)
    (do (println "Your bag is currently empty")
    player)
    (println things))))
;returns the amount of energy
(defn check-health [player]
  (let [health (player :energy)]
  (println health)))

;eat object/foods to get energy (that is expended when move to different locations)
(defn eat-food [player]
  (println "enter in the food you want to eat: ")
    (let [things (player :inventory)
     food (read-line)
      ]
  (if (contains? things food) ;checks if the food you want to eat is in your inventory
  (println "You have eaten " food
  update-in player [:energy] inc)
  (println "You do not have this food item in your inventory"))
  ))

;see what location is in the stated direction from the player
(defn check-map [dir player]
  (let [location(player :location)
      dest (->> the-map location :dir dir)]
      (if (nil? dest) ;if you can't go in that direction
      (do (println "There is nothing in that direction.")
          player)
          (println dest) ;prints out location in that direction
          )))

;regain 5 health at the hospital or afte resting in home
(defn heal [player]
  (let [location(player :location)]
    (if (= location :hospital)
    (update-in player [:energy] inc
      update-in player [:energy] inc
      update-in player [:energy] inc
      update-in player [:energy] inc
      update-in player [:energy] inc )
      (if (= location :house)
      (update-in player [:energy] inc
        update-in player [:energy] inc
        update-in player [:energy] inc
        update-in player [:energy] inc
        update-in player [:energy] inc
        )))))

;enter code to enter power plant
(defn enter-code [player]
  (println "enter in the code: ")
  (let [location(player :location)
  ;  state (->> the-map location :state)
  ;  text (->> the-map location :desc)
  ;  footballstate (->> the-map :football-stadium :state)
  ;  footballtext (->> the-map :football-stadium :text)
     code (read-line)] ;enter in the code
     (if (= location :power-plant)
     (if (= code "3452")
    (println "Correct. The power-plant is unlocked. You go inside and turn on the power. Yay, football game is saved. Use ctrl-C to exit the game. "
    update-in the-map :power-plant [:desc] "The power-plant is unlocked. You walk inside, and the hum of electricity tells you that everything is working fine. "
    update-in the-map :power-plant [:state] "unlocked"
    update-in the-map :football-stadium [:state] "lights"
    update-in the-map :football-stadium [:desc] "The lights are all lit up, ready for the game. " ;updatiing the map states/descriptions
    )
    (println "Wrong"))
    )))

;see surrounding locations around current location
(defn surroundings[player]
    (let [location (player :location)
      dest-north (->> the-map location :dir :north)
      dest-south (->> the-map location :dir :south)
      dest-east (->> the-map location :dir :east)
      dest-west (->> the-map location :dir :west)]
      (if-not (nil? dest-north)
      (println "north: " dest-north))
      (if-not (nil? dest-south)
      (println "south: " dest-south))
      (if-not (nil? dest-east)
      (println "east: " dest-east))
      (if-not (nil? dest-west)
      (println "west: " dest-west))
      ))

;takes first item that is in the room
(defn take-item [player]
  (let [location(player :location)
      things (->> the-map location :contents)]
  (if (nil? things)
  (do (println "There is nothing here")
    player)
  (assoc-in player [:inventory] #(conj % (first things) )
  (remove (first things) things)))))

(defn tock [player]
  (update-in player [:tick] inc))

(defn respond [player command room]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         (:or [:n] [:north] ) (go :north player)
         [:south] (go :south player)
         [:east] (go :east player)
         [:west] (go :west player)

         [:stuff] (take-item player)
         [:inspect] (inspect player)
         [:map] (surroundings player)
         [:look-east] (check-map :east player)
         [:look-west] (check-map :west player)
         [:look-north] (check-map :north player)
         [:look-south] (check-map :south player)
         [:bag] (check-inventory player)
         [:enter-code] (enter-code player)
         [:eat] (eat-food player)
         [:rest] (heal player)
         [:energy] (check-health player)

         _ (do (println "I don't understand you.")
               player)

         ))
;This will compile but does not work, and I have no clue how to make it work, and I am so tired. Please take pity on me.

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))
