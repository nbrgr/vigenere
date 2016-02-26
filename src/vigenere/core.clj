(ns vigenere.core)

(def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def indices (range 0 26))

(def alpha-i (map list alphabet indices))

(def encrypted "IYMECGOBDOJBSNTVAQLNBIEAOYIOHVXZYZYLEEVIPWOBBOEIVZHWUDEAQALLKROCUWSWRYSIUYBMAEIRDEFYYLKODKOGIKPHPRDEJIPWLLWPHRKYMBMAKNGMRELYDPHRNPZHBYJDPMMWBXEYOZJMYXNYJDQWYMEOGPYBCXSXXYHLBELLEPRDEGWXLEPMNOCMRTGQQOUPPEDPSLZOJAEYWNMKRFBLPGIMQAYTSHMRCKTUMVSTVDBOEUEEVRGJGGPIATDRARABLPGIMQDBCFWXDFAWUWPPMRGJGNOETGDMCIIMEXTBEENBNICKYPWNQBLPGIMQOELCIMRCLACMV")


;;Simple factorial function
(defn fac [n]
  (reduce *' (drop 1 (range (inc n)))))

;;Combination function
(defn choose [n k]
	(if (< n k) 
		0  
		(/ (fac n) (* (fac (- n k)) (fac k)))))

;;Alternative combination function used for maps
(defn choose' [k n]
  (choose n k))	

;;Returns a map containing the number of occurances of each letter
(defn occur [string] 
	(reduce (fn [m x] (assoc m x (inc (m x 0)))) {} string))

;;Returns the ioc for a given string
(defn ioc [string]
  (/ 
  	(reduce + (map (partial choose' 2) (vals (occur string)))) 
  	(choose (count string) 2)))

;;Given a vector of iocs, returns the average. Really just computes the average of 
;; anything I guess
(defn avioc [iocv]
	(/ (reduce + iocv) (count iocv)))

;;takes every nth item of a collection, each time offset by a different amount 0-n
(defn period-nth [string period]
	(map 
		(partial take-nth period) 
		(map (fn [x]
			(drop x string))
			(range 0 period))))

;;finds the average ioc for a string with key length n
(defn iocn [string n]
	(if (= 1 n)
		(ioc string)
		(avioc (map ioc (period-nth string n))))) 

;;finds the maximum of the ioc function up to a possible max n
(defn maxioc [string maxn]
	(apply (partial max-key first)
		(map 
			(fn [x] 
				(list (float (iocn string (first x))) (second x))) 
			(map list (range 1 maxn) (range 1 maxn)))))

;;turns frequency map into frequency list
(defn map-list [omap]
	(map list (keys omap) (vals omap)))

;;finds the index of a letter
(defn getindex [letter]
	(second (first
		(filter (fn [x]
		(= (first x) letter))
		alpha-i))))

;;given a index, finds the letter
(defn getletter [index]
	(first (first
		(filter (fn [x]
			(= (second x) index))
		alpha-i))))

;;returns the most frequent letter in a frequency list
(defn getmostfreq [freqlist]
	(apply max-key second freqlist))

;;find shift amount from *e
(defn eshift [freqlist]
	(mod 
		(- (getindex (first (getmostfreq freqlist)))
		(getindex \E)) 
	26))

(defn shiftfrom [freqlist letter]
	(mod 
		(- (getindex (first (getmostfreq freqlist)))
		(getindex letter)) 
	26))

;;converts a list of letters to a list of their indices
(defn indexlist [letterlist]
	(map getindex letterlist))

;;converts a list of indeces to a list of letters
(defn letterlist [indexlist]
	(map getletter indexlist))

;;shifts letters by converting to their indexlist and then back
(defn shift [string shift]
	(map getletter
		(map (fn [x]
			(mod (+ (getindex x) shift) 26))
		string)))

;;finds the relative shift by comparing their e-shifts
(defn relative-shift [string1 string2]
	(mod (- (eshift (occur string2)) (eshift (occur string1))) 26))

;;gets the relative shift between each substring in the list
;;takes the list from period-nth
(defn shiftlist [substrings]
	(map (partial apply relative-shift)
		(map 
			(fn [x] (take 2 (drop x substrings)))
		(range (- (count substrings) 1)))))

