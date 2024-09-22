;; Just because 3d-matrices:m* produces different result  

(in-package :raymath)

(defcstruct (%matrix :class matrix-type)
  "Matrix type (OpenGL style 4x4)"
  (m0 :float) (m4 :float) (m8 :float) (m12 :float)
  (m1 :float) (m5 :float) (m9 :float) (m13 :float)
  (m2 :float) (m6 :float) (m10 :float) (m14 :float)
  (m3 :float) (m7 :float) (m11 :float) (m15 :float))

(define-conversion-into-foreign-memory ((object 3d-matrices:mat4) (type matrix-type) pointer)
    (with-foreign-slots ((m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15) pointer (:struct %matrix))
      (setf
     ;; row 1
       m0  (3d-matrices:miref4 object 0)
       m4  (3d-matrices:miref4 object 1)
       m8  (3d-matrices:miref4 object 2)
       m12 (3d-matrices:miref4 object 3)
       ;; row 2
       m1  (3d-matrices:miref4 object 4)
       m5  (3d-matrices:miref4 object 5)
       m9  (3d-matrices:miref4 object 6)
       m13 (3d-matrices:miref4 object 7)
       ;; row 3
       m2  (3d-matrices:miref4 object 8)
       m6  (3d-matrices:miref4 object 9)
       m10 (3d-matrices:miref4 object 10)
       m14 (3d-matrices:miref4 object 11)
       ;; row 4
       m3  (3d-matrices:miref4 object 12)
       m7  (3d-matrices:miref4 object 13)
       m11 (3d-matrices:miref4 object 14)
       m15 (3d-matrices:miref4 object 15))))

(define-conversion-from-foreign (pointer (type matrix-type))
    (with-foreign-slots ((m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15) pointer (:struct %matrix))
      (3d-matrices:mat m0 m4 m8 m12
                       m1 m5 m9 m13
                       m2 m6 m10 m14
                       m3 m7 m11 m15)))

; RMAPI Matrix MatrixMultiply(Matrix left, Matrix right)
(defcfun ("MatrixMultiply" matrix-multiply) (:struct %matrix)
  "Get two matrix multiplication"
  (left (:struct %matrix))
  (right (:struct %matrix)))
