(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defun main ()
  (let* ((screen-width 800)
         (screen-height 450)
         (res-path (uiop:native-namestring (asdf:system-relative-pathname 'cl-raylib  "examples/resources/models/")))
         (camera-pos (vec 5.0 4.0 5.0))
         (camera-target (vec 0.0 2.0 0.0))
         (camera-up (vec 0.0 1.0 0.0))
         (camera (make-camera3d :position camera-pos
                                :target camera-target
                                :up camera-up
                                :fovy 90
                                :projection :camera-perspective)))

    (with-window (screen-width screen-height "Window")
      (set-target-fps 60)
      (let* ((animation-count (cffi:foreign-alloc :int))
             (animation-ptr (load-model-animations (concatenate 'string res-path "guyanim.iqm") animation-count))
             (model (load-model (concatenate 'string res-path "guy.iqm")))
             (texture (load-texture (concatenate 'string res-path "guytex.png")))
             (material-ptr (model-materials model))
             (anim-frame 0))

        (with-material (material material-ptr)
          (let ((mat-map-ptr (material-maps material)))
            (set-material-map-texture mat-map-ptr texture)))
        
        (loop until (window-should-close)
              do
                (update-camera camera :camera-orbital)
                (incf anim-frame)
                (let ((anim (model-animation-deref animation-ptr)))
                             (if (>= anim-frame (model-animation-frame-count anim)) (setf anim-frame 0))
                             (update-model-animation model anim anim-frame))
                (with-drawing
                  (clear-background :raywhite)
                  (with-mode-3d (camera)
                    (draw-grid 10 1.0)
                    (draw-model model (vec 0.0 0.0 0.0) 1.0 :white))))
        (unload-model model)
        (unload-texture texture)
        (unload-model-animations animation-ptr (cffi:mem-ref animation-count :int))))))

(main)