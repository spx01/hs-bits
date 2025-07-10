#lang racket

(require 2htdp/image)
(require colors)

(define (iapp l f) (apply f l))
(define floor-exact (compose inexact->exact floor))
(define v+ (curry map +))
(define v- (curry map -))
(define (vdot u v) (apply + (map * u v)))
(define (relu x)
  (cond
    [(< x 0) 0]
    [(> x 1) 1]
    [else x]))
(define (mix a b f)
  (+ (* a (- 1 f)) (* b f)))
(define (poly lst t)
  (foldr (match-λ** [((cons a e) acc) (+ acc (* a (expt t e)))]) 0 lst))
(define (flmod x m)
  (- x (* (floor (/ x m)) m)))
(define (put-pixel p color img)
  (define pixel (square 1 "solid" color))
  (place-image pixel (first p) (second p) img))
(define trns (color 255 255 255 0))
(define (mkgrid w h)
  (for*/list ([x (in-range w)]
              [y (in-range h)])
    (list (+ 0.5 x) (+ 0.5 y))))
(define (color2img c)
  (let ([r (send c red)]
        [g (send c green)]
        [b (send c blue)])
    (color r g b)))

(define ptable '(151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74 165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41 55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89 18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185 112 104 218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51 145 235 249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176 115 121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156 180))
(define grads2d (build-list 8 (λ (i) (map (curry iapp `(,(* i (/ pi 4)))) `(,cos ,sin)))))
(define perm (list->vector (append ptable ptable)))

(define hash
  (curry foldr (λ (x acc) (vector-ref perm (bitwise-and (+ x acc) 255))) 0))

(define (bilinear p ys
                  #:scale [scale *]
                  #:+ [+ +])
  (match-let* ([(list x y) p]
               [(list y1 y2 y3 y4) ys]
               [m1 (mix y1 y2 x)]
               [m2 (mix y3 y4 x)])
    (mix m1 m2 y)))

(define (pgrad2d p)
  (list-ref grads2d (bitwise-and (hash p) 7)))

(define (perlin2d
         #:scale [scale 1]
         #:interp [interp bilinear]
         #:fade [fade (curry poly '((6 . 5) (-15 . 4) (10 . 3)))])
  (λ (p) (match-let* ([(list x y) p]
                      [p0 (map floor-exact p)]
                      [dirs '((0 0) (1 0) (0 1) (1 1))]
                      [qs (map (curry v+ p0) dirs)]
                      [vals (map (λ (q) (vdot (v- p q) (pgrad2d q))) qs)]
                      [p- (map fade (v- p p0))]
                      [raw (interp p- vals)])
           (relu (/ (+ 1 (* scale raw)) 2)))))

(define (f2c-grey f)
  (let ([l (floor-exact (* f 255))])
    (color l l l 255)))

(define (f2c-hue f)
  (color2img (hsl->color (hsl (flmod f 1) 1 0.5))))

(define (plot-perlin2d w h
                       #:scale [scale 1]
                       #:sample [sample (perlin2d #:scale 1.4)])
  (let* ([blank (rectangle w h "solid" trns)]
         [points (mkgrid w h)])
    (foldr (λ (p acc) (put-pixel p (f2c-hue (sample (map (curry * scale) p))) acc)) blank points)))

(define (perlin2d-fbm
         sample
         octaves
         lacunarity
         gain
         #:scale [scale 1])
  (λ (p) (for/fold ([total 0]
                    [max_val 0]
                    [freq 1]
                    [amp 1]
                    #:result (/ total max_val))
                   ([oct (in-range octaves)])
           (values
            (+ total (* amp (sample (map (curry * freq) p))))
            (+ max_val amp)
            (* freq lacunarity)
            (* amp gain)))))

(define plot1 (plot-perlin2d 200 200 #:scale 0.02))
(save-image plot1 "out.png")
plot1

(define plot2
  (let ([sample (perlin2d #:scale 1.4)])
    (plot-perlin2d 200 200 #:scale 0.02 #:sample (perlin2d-fbm sample 4 2 0.3))))
(save-image plot2 "out_octaves.png")
plot2
