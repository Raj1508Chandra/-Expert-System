;----------------------------------------------------------------------------
; CLASSES
;----------------------------------------------------------------------------
(defclass PERSON
	(is-a USER)
	(role concrete)
	(slot company)
	(slot devicetype))

(defclass DEVICENAME
	(is-a USER)
	(slot company)
	(slot price)
	(slot suggested_device))

;----------------------------------------------------------------------------
; DEFAULT INSTANCES
;----------------------------------------------------------------------------

(definstances PERSON-INSTANCES
	(client of PERSON))

(definstances DEVICE-INSTANCES
	(which_device of DEVICENAME))

;----------------------------------------------------------------------------
; INITIAL USER INPUTS AND VALIDATIONS
;----------------------------------------------------------------------------

(deffunction user-input-validation (?question $?valid-input)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?valid-input)) do
      (printout t "Please enter a valid input as mentioned in the question!" crlf)
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)
   
; RULE TO GET THE USER INPUT
(defrule GetCompanion(declare (salience 10))
    =>
    (printout t crlf)
    (printout t "--------------------------------------------------------------------------------------------------------" crlf)
    (printout t "------------------------ WELCOME TO THE SMARTPHONES / TABLETS / PHABLETS EXPERT ------------------------" crlf)
    (printout t "--------------------------------------------------------------------------------------------------------" crlf)
    (printout t crlf)    
    (send [client] put-devicetype
    (user-input-validation "What do you want to buy? (mobile/tablet/phablet):  "mobile tablet phablet)))
   
;----------------------------------------------------------------------------
; RULES OF THE EXPERT SYSTEM TO SELECT THE DEVICE
;----------------------------------------------------------------------------

; RULE TO SELECT PERFECT MOBILE PHONE
(defrule buy_mobile
	?ins <- (object (is-a PERSON) (devicetype mobile))
	=> 
	(printout t crlf)
	(printout t "Let me select a Mobile suitable to buy in your budget..." crlf crlf)
   	(send [which_device] put-price
    (user-input-validation "Enter your preferred Price Range (under10k/ 10k-15k / 15k-25k / above25k):  "
    		under10k 10k-15k 15k-25k above25k)))
   	 
; RULE TO SELECT PERFECT TABLET
(defrule buy_tablet
	?ins <- (object (is-a PERSON) (devicetype tablet))
	=> 
	(printout t crlf)
	(printout t "Let me select a Tablet suitable to buy in your budget..." crlf crlf)
    (send [which_device] put-price
  	(user-input-validation "Enter your preferred Price Range (under20k / above20k): " 
  		under20k above20k)))
RULE TO SELECT CEMRA
(defrule buy_cemra
	?ins <- (object (is-a PERSON) (devicetype cemra))
	=> 
	(printout t crlf)
	(printout t "Let me select a cemra suitable to buy in your budget..." crlf crlf)
    (send [which_device] put-price
  	(user-input-validation "Enter your preferred Price Range (under20k / above20k): " 
  		under20k above20k)))
   	 
; RULE TO SELECT PERFECT HEADPHONE
(defrule buy_headphone
	?ins <- (object (is-a PERSON) (devicetype headphone))
	=> 
	(printout t crlf)
	(printout t "Let me select a Headphone suitable to buy in your budget..." crlf crlf)
	(send [which_device] put-price
    (user-input-validation "Enter your preferred Price Range (under1k / above1k): "
         under5k above5k)))
   	 
; RULE TO HEADPHONE ABOVE 1K
(defrule tab_above 1k
	(and ?ins <- (object (is-a DEVICENAME) (price above 1k))
	(object (is-a PERSON)(devicetype headphone)))
	=> 
	(printout t crlf)
	(printout t "Let me select a Tablet above 1k..." crlf crlf)
	(send [which_device] put-company
    (user-input-validation "Enter your preferred company (boAt/aroma/realme):  "
        boAt aroma  realme)))

; RULE TO HEADPHONE UNDER 1K
(defrule tab_under 1k
	(and ?ins <- (object (is-a DEVICENAME) (price under10k))
	(object (is-a PERSON)(devicetype headphone)))
	=> 
	(printout t crlf)
	(printout t "Let me select a HEADPHONE under 10k..." crlf crlf)
	(send [which_device] put-company
    (user-input-validation "Enter your preferred company (aroma):  "
        aroma)))

   	
; RULE TO CEMRA ABOVE 20K
(defrule phab_above15k
	(and ?ins <- (object (is-a DEVICENAME) (price above 20k))
	(object (is-a PERSON)(devicetype cemra)))
	=> 
	(printout t crlf)
	(printout t "Let me select a Cemra above 15K..." crlf crlf)
   	(send [which_device] put-company
   	(user-input-validation "Enter your preferred company (canon/ fujifilm):  "
         canon fujifilm)))

    ; RULE TO CEMRA UNDER 15K
(defrule phab_under 20k
	(and ?ins <- (object (is-a DEVICENAME) (price under 20k))
	(object (is-a PERSON)(devicetype cemra)))
	=> 
	(printout t crlf)
	(printout t "Let me select a Cemra under 15K..." crlf crlf)
   	(send [which_device] put-company
   	(user-input-validation "Enter your preferred company (samsung):  "
         samsung)))

; RULE TO MOBILE UNDER 10K
(defrule mob_under10k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (price under10k)))
	=> 
	(printout t crlf)
	(printout t "Let me select a Mobile Phone Under 10K..." crlf crlf)
   	(send [client] put-company
   	(user-input-validation "Select your preferred company(poco/realme/redmi): "
         poco realme redmi)))

; RULE TO MOBILE 10K - 15K
(defrule mob_10k15k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (price 10k-15k)))
	=> 
	(printout t crlf)
	(printout t "Let me select a Mobile Phone in range 10K - 15K..." crlf crlf)
   	(send [client] put-company
   	(user-input-validation "What is your preferred comapny? (sumsung/vivo/oppo):  "
         sumsung vivo oppo)))
   	 
; RULE TO MOBILE 15K - 25K
(defrule mob_15k25k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (price 15k-25k)))
	=> 
	(printout t crlf)
	(printout t "Let me select a Mobile Phone in range 15K - 25K..." crlf crlf)   	(send [client] put-company
   	(user-input-validation "What is your preferred company? (oppo /motorola/oneplus):  "
         sumsung vivo oppo)))
 
; RULE TO MOBILE ABOVE 25K
(defrule mob_above25k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (price above25k)))
	=> 
	(printout t crlf)
	(printout t "Let me select a Mobile Phone above 25k..." crlf crlf) 
   	(send [client] put-company
   	(user-input-validation "What is your preferred company? (oneplus/samsung/apple):  "
         oneplus samsung apple)))
 
; RULE TO BUY MOBILE POCO UNDER 10K
(defrule mob_poco_under10k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile) (company poco))
	(object (is-a DEVICENAME) (price under10k)))
	=> 
	(send [which_device] put-suggested_device "POCO C31  (Rs.9999/-)"))

; RULE TO BUY MOBILE samsung UNDER 10K
(defrule mob_samsung_under10k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile) (company samsung))
	(object (is-a DEVICENAME) (price under10k)))
	=> 
	(send [which_device] put-suggested_device "Samsung Galaxy A03(Rs.9500/-)"))

; RULE TO BUY MOBILE OPPO UNDER 10K
(defrule mob_oppo_under10k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile) (company oppo))
	(object (is-a DEVICENAME) (price under10k)))
	=> 
	(send [which_device] put-suggested_device "OPPO A11K Vibe K5 (Rs.8990/-)"))

; RULE TO BUY MOBILE OPPO 10K-15K
(defrule mob_oppo_10k15k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile) (company oppo))
	(object (is-a DEVICENAME) (price 10k-15k)))
	=> 
	(send [which_device] put-suggested_device "OPPO A57  (Rs.13490/-)"))
	
; RULE TO BUY MOBILE VIVO 10K-15K
(defrule mob_vivo_10k15k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile) (company vivo))
	(object (is-a DEVICENAME) (price 10k-15k)))
	=> 
	(send [which_device] put-suggested_device "VIVO T1 44W (Rs. 14499/-)"))
	
; RULE TO BUY MOBILE SAMSUNG 10K-15K
(defrule mob_samsung_10k15k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile) (company samsung))
	(object (is-a DEVICENAME) (price 10k-15k)))
	=> 
	(send [which_device] put-suggested_device "Samsung Galaxy J7 (Rs. 14990/-)"))

; RULE TO BUY MOBILE ONE PLUS 15K - 25K
(defrule mob_oneplus_15k25k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile) (company oneplus))
	(object (is-a DEVICENAME) (price 15k-25k)))
	=> 
	(send [which_device] put-suggested_device "One Plus 2 (Rs. 19999/-)"))
	
; RULE TO BUY MOBILE SAMSUNG 15K - 25K
(defrule mob_samsung_15k25k 
	(and ?ins <- (object (is-a PERSON) (devicetype mobile) (company samsung))
	(object (is-a DEVICENAME) (price 15k-25k)))
	=> 
	(send [which_device] put-suggested_device "Samsung Galaxy J7 Prime (Rs. 18499/-)"))
	
; RULE TO BUY MOBILE  VIVO 15K - 25K
(defrule mob_xiaomi_15k25k  
	(and ?ins <- (object (is-a PERSON) (devicetype mobile) (company vivo))
	(object (is-a DEVICENAME) (price 15k-25k)))
	=> 
	(send [which_device] put-suggested_device "ViVo (Rs. 22999/-)"))

; RULE TO BUY MOBILE ONE PLUS ABOVE 25K
(defrule mob_oneplus_above25k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile) (company oneplus))
	(object (is-a DEVICENAME) (price above25k)))
	=> 
	(send [which_device] put-suggested_device "One Plus 3 (Rs. 27999/-)"))
	
; RULE TO BUY MOBILE SAMSUNG ABOVE 25K
(defrule mob_samsung_above25k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile) (company samsung))
	(object (is-a DEVICENAME) (price above25k)))
	=> 
	(send [which_device] put-suggested_device "Samsung Galaxy S7 Edge (Rs. 50899/-)"))
	
; RULE TO BUY MOBILE APPLE ABOVE 25K
(defrule mob_lenovo_above25k
	(and ?ins <- (object (is-a PERSON) (devicetype mobile) (company apple))
	(object (is-a DEVICENAME) (price above25k)))
	=> 
	(send [which_device] put-suggested_device "Apple Iphone 6s (Rs. 41000/-)  OR  Apple Iphone 7 (Rs. 69999/-)"))
	
; RULE TO BUY TABLET UNDER 20K
(defrule tab_ realme_under20k
	(and ?ins <- (object (is-a DEVICENAME) (price under20k) (company realme))
	(object(is-a PERSON)(devicetype tablet)))
	=> 
	(send ?ins put-suggested_device "Realme Pad  (Rs. 17999/-)") )

; RULE TO BUY Realme TABLET ABOVE 20K
(defrule tab_ realme_above20k
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company realme)(price above20k)))
	=> 
	(send [which_device] put-suggested_device " realme pad  4G (Rs. 38999/-)  OR   realme pad 6G (Rs. 69999/-)"))
	
; RULE TO BUY SAMSUNG TABLET ABOVE 20K
(defrule tab_samsung_above20k
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company samsung)(price above20k)))
	=> 
	(send [which_device] put-suggested_device "Samsung Galaxy S7 (Rs. 51990/-) OR Samsung Galaxy Tab S6(Rs. 26999)"))
	
; RULE TO BUY LENOVO TABLET ABOVE 20K
(defrule tab_lenovo_above20k
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(price above20k)))
	=> 
	(send [which_device] put-suggested_device "Lenovo M10 FHD Plus 3 (Rs. 21999/-)"))
	
; RULE TO BUY  bOAT HEADPHONE ABOVE 1K
(defrule head__boat_above1k
	(and ?ins <- (object (is-a DEVICENAME) (price above1k)(company boat))
	(object (is-a PERSON) (devicetype headphone)))
	=> 
	(send ?ins put-suggested_device "boAt Airdopes (Rs. 1299/-)"))
	
; RULE TO BUY REALME HEADPHONE ABOVE 1K
(defrule head_realme_above1k
	(and ?ins <- (object (is-a DEVICENAME) (price above1k)(company realme))
	(object (is-a PERSON) (devicetype headphone)))
	=> 
	(send ?ins put-suggested_device "Realme Buds(Rs. 1499/-)"))
	
; RULE TO BUY AROMA HEADPHONE ABOVE 1K
(defrule head_aroma_above1k
	(and ?ins <- (object (is-a DEVICENAME) (price above1k)(company aroma))
	(object (is-a PERSON) (devicetype phablet)	=> 
	(send ?ins put-suggested_device "Aroma NB17A 8 (Rs. 4999/-)"))
	
; RULE TO BUY Headphone UNDER 1K
(defrule head_under1k_wings phantom 
	(and ?ins <- (object (is-a DEVICENAME) (price under15k)(company wings phantom ))
	(object (is-a PERSON) (devicetype headphone)))
	=> 
	(send ?ins put-suggested_device "wings phantom 250 (Rs. 769/-)")
	(printout t crlf)
	(printout t "Let me select a Headphone Under 1K..." crlf)) 
	
;----------------------------------------------------------------------------
; PRINTS THE FINAL SUGGESSION	
;----------------------------------------------------------------------------

; RULE TO PRINT THE SUGGESTED DEVICE
(defrule choose_device (declare (salience -1))
	(object (is-a DEVICENAME) (suggested_device ?mov))
	=>
	(printout t crlf)
	(printout t "-------------------------------------------------------------------------------------------------------------------------------" crlf)
    (printout t "The recommended Device which best suits your needs is: " ?mov crlf)
    (printout t "-------------------------------------------------------------------------------------------------------------------------------" crlf))