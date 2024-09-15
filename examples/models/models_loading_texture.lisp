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

    (with-window (screen-width screen-height "raylib [model] example - loading texture")
      (set-target-fps 60)
      (let* ((castle (load-model (concatenate 'string res-path "castle.obj")))
             (texture (load-texture (concatenate 'string res-path "castle_diffuse.png")))
             (material-ptr (model-materials castle)))

        (with-material (material material-ptr)
          (let ((mat-map-ptr (material-maps material)))
            (set-material-map-texture mat-map-ptr texture)))

        (loop until (window-should-close)
              do
                (update-camera camera :camera-orbital)
                (with-drawing
                  (clear-background :raywhite)
                  (with-mode-3d (camera)
                    (draw-grid 10 1.0)
                    (draw-model castle (vec 0.0 0.0 0.0) 0.25 :white))))
        (unload-model castle)
        (unload-texture texture)))))

(main)