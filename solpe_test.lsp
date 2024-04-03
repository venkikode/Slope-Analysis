(defun solpe_layers()
  (command "layer" "m" "0-0.5" "c" "225" "" "lw" "0.2" "" "")
  (command "layer" "m" "0.5-1.5" "c" "41" "" "lw" "0.2" "" "")
  (command "layer" "m" "1.5-2" "c" "210" "" "lw" "0.2" "" "")
  (command "layer" "m" "2-5" "c" "110" "" "lw" "0.2" "" "")
  (command "layer" "m" "5-8" "c" "80" "" "lw" "0.2" "" "")
  (command "layer" "m" "8-10" "c" "74" "" "lw" "0.2" "" "")
  (command "layer" "m" "10-15" "c" "86" "" "lw" "0.2" "" "")
  (command "layer" "m" "15-20" "c" "140" "" "lw" "0.2" "" "")
  (command "layer" "m" "20-30" "c" "150" "" "lw" "0.2" "" "")
  (command "layer" "m" "30-40" "c" "160" "" "lw" "0.2" "" "")
  (command "layer" "m" "40-50" "c" "170" "" "lw" "0.2" "" "")
  (command "layer" "m" "50-60" "c" "50" "" "lw" "0.2" "" "")
  (command "layer" "m" "60-70" "c" "40" "" "lw" "0.2" "" "")
  (command "layer" "m" "70-80" "c" "30" "" "lw" "0.2" "" "")  
  (command "layer" "m" "80-90" "c" "20" "" "lw" "0.2" "" "")
  (command "layer" "m" "90-100" "c" "10" "" "lw" "0.2" "" "")

  )

(defun c:slopearrows ()
  (solpe_layers)
  (setq highest nil lowest nil)
  (setq ss (ssget '((0 . "TEXT"))))
  (setq highest nil)
  (setq lowest nil)
  (setq count 0)
  (repeat (sslength ss)
    (setq ent (ssname ss count))
    (setq text (cdr (assoc 1 (entget ent))))
    (setq val (atof text))
    (if (or (not highest) (> val highest))
        (setq highest val))
    (if (or (not lowest) (< val lowest))
        (setq lowest val))
    (setq count (1+ count))
  )
  (prompt (strcat "\nHighest value: " (rtos highest 2 3)))
  (prompt (strcat "\nLowest value: " (rtos lowest 2 3)))
  (prompt (strcat "\nDiffrence between intervels: " (rtos (- highest lowest) 2 3)))
  (setq ibom (getpoint "\nPick the point to generate table"))
  
  (solpe_table)
  
  (setq val1 0 val2 0 val3 0 val4 0 val5 0 val6 0 val7 0 val8 0 val9 0 val10 0 val11 0 val12 0 val13 0 val14 0 val15 0 val16 0)
  
  (setq count 0)
  (repeat (sslength ss)
    (setq obj (entget (ssname ss count)))
    (setq txt (atof (cdr (assoc 1 obj))))
    (setq clay (assoc 8 obj))
      (cond ((and (>= txt lowest) (<= txt bv1)) (setq nlay (cons (car clay) "0-0.5")) (setq val1 (1+ val1)))
            ((and (> txt bv1) (<= txt bv2)) (setq nlay (cons (car clay) "0.5-1.5")) (setq val2 (1+ val2)))
            ((and (> txt bv2) (<= txt bv3)) (setq nlay (cons (car clay) "1.5-2")) (setq val3 (1+ val3)))
            ((and (> txt bv3) (<= txt bv4)) (setq nlay (cons (car clay) "2-5")) (setq val4 (1+ val4)))
            ((and (> txt bv4) (<= txt bv5)) (setq nlay (cons (car clay) "5-8")) (setq val5 (1+ val5)))
            ((and (> txt bv5) (<= txt bv6)) (setq nlay (cons (car clay) "8-10")) (setq val6 (1+ val6)))
            ((and (> txt bv6) (<= txt bv7)) (setq nlay (cons (car clay) "10-15")) (setq val7 (1+ val7)))
            ((and (> txt bv7) (<= txt bv8)) (setq nlay (cons (car clay) "15-20")) (setq val8 (1+ val8)))
            ((and (> txt bv8) (<= txt bv9)) (setq nlay (cons (car clay) "20-30")) (setq val9 (1+ val9)))
            ((and (> txt bv9) (<= txt bv10)) (setq nlay (cons (car clay) "30-40")) (setq val10 (1+ val10)))
            ((and (> txt bv10) (<= txt bv11)) (setq nlay (cons (car clay) "40-50")) (setq val11 (1+ val11)))
            ((and (> txt bv11) (<= txt bv12)) (setq nlay (cons (car clay) "50-60")) (setq val12 (1+ val12)))
            ((and (> txt bv12) (<= txt bv13)) (setq nlay (cons (car clay) "60-70")) (setq val13 (1+ val13)))
            ((and (> txt bv13) (<= txt bv14)) (setq nlay (cons (car clay) "70-80")) (setq val14 (1+ val14)))
            ((and (> txt bv14) (<= txt bv15)) (setq nlay (cons (car clay) "80-90")) (setq val15 (1+ val15)))
            ((and (> txt bv15) (<= txt highest)) (setq nlay (cons (car clay) "90-100")) (setq val16 (1+ val16)))
      )
    
    (setq newobj (subst nlay clay obj))
    (entmod newobj)
    (setq count (1+ count))
  )
  
        
)

(defun solpe_table()
(setq bom1t (polar ibom (DTR 0.0) 24.66)) (setq bom1 (polar bom1t (DTR 270.0) 11)) ;low
(setq bom2t (polar ibom (DTR 0.0) 73.97)) (setq bom2 (polar bom2t (DTR 270.0) 11)) ;high
  
(setq bom3t (polar ibom (DTR 0.0) 74.39)) (setq bom3 (polar bom3t (DTR 270.0) 32.73)) ;0.5
(setq bom4t (polar ibom (DTR 0.0) 101.4)) (setq bom4 (polar bom4t (DTR 270.0) 32.73)) ;0.5
  
(setq bom5t (polar ibom (DTR 0.0) 74.39)) (setq bom5 (polar bom5t (DTR 270.0) 44.12)) ;1.5
(setq bom6t (polar ibom (DTR 0.0) 101.16)) (setq bom6 (polar bom6t (DTR 270.0) 44.12)) ;1.5
  
(setq bom7t (polar ibom (DTR 0.0) 74.16)) (setq bom7 (polar bom7t (DTR 270.0) 55.51)) ;2
(setq bom8t (polar ibom (DTR 0.0) 101.44)) (setq bom8 (polar bom8t (DTR 270.0) 55.51)) ;2
  
(setq bom9t (polar ibom (DTR 0.0) 74.43)) (setq bom9 (polar bom9t (DTR 270.0) 66.91)) ;5
(setq bom10t (polar ibom (DTR 0.0) 101.44)) (setq bom10 (polar bom10t (DTR 270.0) 66.91)) ;5
  
(setq bom11t (polar ibom (DTR 0.0) 74.39)) (setq bom11 (polar bom11t (DTR 270.0) 78.3)) ;8
(setq bom12t (polar ibom (DTR 0.0) 101.16)) (setq bom12 (polar bom12t (DTR 270.0) 78.3)) ;8
  
(setq bom13t (polar ibom (DTR 0.0) 74.16)) (setq bom13 (polar bom13t (DTR 270.0) 89.69)) ;10
(setq bom14t (polar ibom (DTR 0.0) 101.44)) (setq bom14 (polar bom14t (DTR 270.0) 89.69)) ;10
  
(setq bom15t (polar ibom (DTR 0.0) 74.43)) (setq bom15 (polar bom15t (DTR 270.0) 101.08));15
(setq bom16t (polar ibom (DTR 0.0) 101.44)) (setq bom16 (polar bom16t (DTR 270.0) 101.08)) ;15
  
(setq bom17t (polar ibom (DTR 0.0) 74.39)) (setq bom17 (polar bom17t (DTR 270.0) 112.47))  ;20
(setq bom18t (polar ibom (DTR 0.0) 101.16)) (setq bom18 (polar bom18t (DTR 270.0) 112.47))  ;20
  
(setq bom19t (polar ibom (DTR 0.0) 74.16)) (setq bom19 (polar bom19t (DTR 270.0) 123.86)) ;30
(setq bom20t (polar ibom (DTR 0.0) 101.44)) (setq bom20 (polar bom20t (DTR 270.0) 123.86)) ;30
  
(setq bom21t (polar ibom (DTR 0.0) 74.43)) (setq bom21 (polar bom21t (DTR 270.0) 135.25)) ;40
(setq bom22t (polar ibom (DTR 0.0) 101.44)) (setq bom22 (polar bom22t (DTR 270.0) 135.25)) ;40
  
(setq bom23t (polar ibom (DTR 0.0) 74.39)) (setq bom23 (polar bom23t (DTR 270.0) 146.64)) ;50
(setq bom24t (polar ibom (DTR 0.0) 101.16)) (setq bom24 (polar bom24t (DTR 270.0) 146.64)) ;50
  
(setq bom25t (polar ibom (DTR 0.0) 74.16)) (setq bom25 (polar bom25t (DTR 270.0) 158.03)) ;60
(setq bom26t (polar ibom (DTR 0.0) 101.44)) (setq bom26 (polar bom26t (DTR 270.0) 158.03)) ;60
  
(setq bom27t (polar ibom (DTR 0.0) 74.43)) (setq bom27 (polar bom27t (DTR 270.0) 169.42)) ;70
(setq bom28t (polar ibom (DTR 0.0) 101.44)) (setq bom28 (polar bom28t (DTR 270.0) 169.42)) ;70
  
(setq bom29t (polar ibom (DTR 0.0) 74.39)) (setq bom29 (polar bom29t (DTR 270.0) 180.81)) ;80
(setq bom30t (polar ibom (DTR 0.0) 101.16)) (setq bom30 (polar bom30t (DTR 270.0) 180.81)) ;80
  
(setq bom31t (polar ibom (DTR 0.0) 74.16)) (setq bom31 (polar bom31t (DTR 270.0) 192.2));90
(setq bom32t (polar ibom (DTR 0.0) 101.44)) (setq bom32 (polar bom32t (DTR 270.0) 192.2));90
  
(setq bom33t (polar ibom (DTR 0.0) 74.43)) (setq bom33 (polar bom33t (DTR 270.0) 203.59));100
(setq bom34t (polar ibom (DTR 0.0) 101.44)) (setq bom34 (polar bom34t (DTR 270.0) 203.59));100
  
   ;; Slope intervels
    (setq bv1 (+ lowest (* (- highest lowest) 0.005)))
    (setq bv2 (+ lowest (* (- highest lowest) 0.015)))
    (setq bv3 (+ lowest (* (- highest lowest) 0.02)))
    (setq bv4 (+ lowest (* (- highest lowest) 0.05)))
    (setq bv5 (+ lowest (* (- highest lowest) 0.08)))
    (setq bv6 (+ lowest (* (- highest lowest) 0.1)))
    (setq bv7 (+ lowest (* (- highest lowest) 0.15)))
    (setq bv8 (+ lowest (* (- highest lowest) 0.20)))
    (setq bv9 (+ lowest (* (- highest lowest) 0.30)))
    (setq bv10 (+ lowest (* (- highest lowest) 0.40)))
    (setq bv11 (+ lowest (* (- highest lowest) 0.50)))
    (setq bv12 (+ lowest (* (- highest lowest) 0.60)))
    (setq bv13 (+ lowest (* (- highest lowest) 0.70)))
    (setq bv14 (+ lowest (* (- highest lowest) 0.80)))
    (setq bv15 (+ lowest (* (- highest lowest) 0.90)))
  

  
  (command "insert" "C:/mymenu/slope_table.dwg" ibom "" "" "")
  (command "textstyle" "Standard")
  (command "_text" bom1 "2.5" "" (rtos lowest 2 3))
  (command "_text" bom2 "2.5" "" (rtos highest 2 3))
  (command "_text" bom3 "2.5" "" (rtos lowest 2 3))
  (command "_text" bom4 "2.5" "" (rtos bv1 2 3))
  (command "_text" bom5 "2.5" "" (rtos bv1 2 3))
  (command "_text" bom6 "2.5" "" (rtos bv2 2 3))
  (command "_text" bom7 "2.5" "" (rtos bv2 2 3))
  (command "_text" bom8 "2.5" "" (rtos bv3 2 3))
  (command "_text" bom9 "2.5" "" (rtos bv3 2 3))
  (command "_text" bom10 "2.5" "" (rtos bv4 2 3))
  (command "_text" bom11 "2.5" "" (rtos bv4 2 3))
  (command "_text" bom12 "2.5" "" (rtos bv5 2 3))
  (command "_text" bom13 "2.5" "" (rtos bv5 2 3))
  (command "_text" bom14 "2.5" "" (rtos bv6 2 3))
  (command "_text" bom15 "2.5" "" (rtos bv6 2 3))
  (command "_text" bom16 "2.5" "" (rtos bv7 2 3))
  (command "_text" bom17 "2.5" "" (rtos bv7 2 3))
  (command "_text" bom18 "2.5" "" (rtos bv8 2 3))
  (command "_text" bom19 "2.5" "" (rtos bv8 2 3))
  (command "_text" bom20 "2.5" "" (rtos bv9 2 3))
  (command "_text" bom21 "2.5" "" (rtos bv9 2 3))
  (command "_text" bom22 "2.5" "" (rtos bv10 2 3))
  (command "_text" bom23 "2.5" "" (rtos bv10 2 3))
  (command "_text" bom24 "2.5" "" (rtos bv11 2 3))
  (command "_text" bom25 "2.5" "" (rtos bv11 2 3))
  (command "_text" bom26 "2.5" "" (rtos bv12 2 3))
  (command "_text" bom27 "2.5" "" (rtos bv12 2 3))
  (command "_text" bom28 "2.5" "" (rtos bv13 2 3))
  (command "_text" bom29 "2.5" "" (rtos bv13 2 3))
  (command "_text" bom30 "2.5" "" (rtos bv14 2 3))
  (command "_text" bom31 "2.5" "" (rtos bv14 2 3))
  (command "_text" bom32 "2.5" "" (rtos bv15 2 3))
  (command "_text" bom33 "2.5" "" (rtos bv15 2 3))
  (command "_text" bom34 "2.5" "" (rtos highest 2 3))
  
)


(defun temp()
  
    ;temp points 
    (setq tm2 (polar bom4 (dtr 0.0) 50))
    (setq tm3 (polar bom6 (dtr 0.0) 50))
    (setq tm4 (polar bom8 (dtr 0.0) 50))
    (setq tm5 (polar bom10 (dtr 0.0) 50))
    (setq tm6 (polar bom12 (dtr 0.0) 50))
    (setq tm7 (polar bom14 (dtr 0.0) 50))
    (setq tm8 (polar bom16 (dtr 0.0) 50))
    (setq tm9 (polar bom18 (dtr 0.0) 50))
    (setq tm10 (polar bom20 (dtr 0.0) 50))
    (setq tm11 (polar bom22 (dtr 0.0) 50))
    (setq tm12 (polar bom24 (dtr 0.0) 50))
    (setq tm13 (polar bom26 (dtr 0.0) 50))
    (setq tm14 (polar bom28 (dtr 0.0) 50))
    (setq tm15 (polar bom30 (dtr 0.0) 50))
    (setq tm16 (polar bom32 (dtr 0.0) 50))
    (setq tm17 (polar bom34 (dtr 0.0) 50))
    
    (command "_text" tm2 "2.5" "" (rtos val1 2 3))
    (command "_text" tm3 "2.5" "" (rtos val2 2 3))
    (command "_text" tm4 "2.5" "" (rtos val3 2 3))
    (command "_text" tm5 "2.5" "" (rtos val4 2 3))
    (command "_text" tm6 "2.5" "" (rtos val5 2 3))
    (command "_text" tm7 "2.5" "" (rtos val6 2 3))
    (command "_text" tm8 "2.5" "" (rtos val7 2 3))
    (command "_text" tm9 "2.5" "" (rtos val8 2 3))
    (command "_text" tm10 "2.5" "" (rtos val9 2 3))
    (command "_text" tm11 "2.5" "" (rtos val10 2 3))
    (command "_text" tm12 "2.5" "" (rtos val11 2 3))
    (command "_text" tm13 "2.5" "" (rtos val12 2 3))
    (command "_text" tm14 "2.5" "" (rtos val13 2 3))
    (command "_text" tm15 "2.5" "" (rtos val14 2 3))
    (command "_text" tm16 "2.5" "" (rtos val15 2 3))
    (command "_text" tm17 "2.5" "" (rtos val16 2 3))
  
  )

(defun dir_arrow(a b / a b dir_ang p1 p2 p4 p3 p5)
  (setq dir_ang (angle a b))
  
  (setq p1 (polar a dir_ang 2.3))
  (setq p2 (polar p1 (+ dir_ang (dtr 90)) 0.625))
  (setq p3 (polar p1 (+ dir_ang (dtr 270)) 0.625))
  (setq p4 (polar p1 dir_ang 1.65))
  (command "_pline" a p1 p2 p4 p3 p1 "c")
  (command "hatch" "solid" "l" "")
)

;| (setq a (getpoint))
(setq b (getpoint)) |;
