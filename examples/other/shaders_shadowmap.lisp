(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
(defparameter *camera-position* (vec 10.0 10.0 10.0))

(defmacro asset_path (file-name)
  `(concatenate 'string res-path ,file-name))

(defun draw-frame (cube)
  (draw-model-ex cube (vec 0.0 0.0 0.0) (vec 0.0 1.0 0.0) 0.0 (vec 10.0 1.0 10.0) :blue)
  (draw-model-ex cube (vec 1.5 1.0 -1.5) (vec 0.0 1.0 0.0) 0.0 (vec 1.0 1.0 1.0) :white)
  (draw-model-ex cube (vec 0.5 1.5 0.5) (vec 0.0 1.0 0.0) 0.0 (vec 2.0 2.0 2.0) :white))

(defun load-shadowmap-render-texture (width height)
  (let ((target-id (rlgl:load-framebuffer)))
    (if (< target-id 1)
        (format t "FBO: Framebuffer object can not be created")
        (let ((target (make-render-texture
                        :id target-id
                        :texture (make-texture
                                   :width width
                                   :height height
                                   :id 0
                                   :mipmaps 0
                                   :format 0)
                        :depth (make-texture
                                 :width width
                                 :height height
                                 :format 19
                                 :mipmaps 1
                                 :id (rlgl:load-texture-depth width height nil)))))
          (rlgl:enable-framebuffer target-id)
          (rlgl:framebuffer-attach target-id (texture-id (render-texture-depth target)) 100 100 0)
          (if (rlgl:framebuffer-complete target-id)
              (format t "FBO: [ID ~a] Framebuffer object created successfully" target-id))
          (rlgl:disable-framebuffer)
          target))))

(defun main ()
  (let* ((res-path (uiop:native-namestring (asdf:system-relative-pathname 'cl-raylib "examples/other/resources/")))
         (camera-target (vec 0.0 0.0 0.0))
         (camera-up (vec 0.0 1.0 0.0))
         (camera (make-camera3d :position *camera-position*
                                :target camera-target
                                :up camera-up
                                :fovy 45
                                :projection :camera-perspective)))

    (with-window (*screen-width* *screen-height* "Window")
      (set-target-fps 60)
      (let* ((shadow-shader (load-shader (asset_path "shadowmap.vs") (asset_path "shadowmap.fs")))
             (shader-loc (get-shader-location shadow-shader "viewPos"))
             (light-dir (vec 0.35 -1.0 -0.35))
             (light-color :white)
             (light-color-norm (color-normalize light-color))
             (light-dir-loc (get-shader-location shadow-shader "lightDir"))
             (light-col-loc (get-shader-location shadow-shader "lightColor"))
             (ambient-loc (get-shader-location shadow-shader "ambient"))
             (light-vp-loc (get-shader-location shadow-shader "lightVP"))
             (shadow-map-loc (get-shader-location shadow-shader "shadowMap"))
             (cube (load-model-from-mesh (gen-mesh-cube 1.0 1.0 1.0)))
             (shadow-map (load-shadowmap-render-texture 1024 1024))
             (light-cam (make-camera3d :position (v* light-dir -15.0)
                                       :target (vec 0.0 0.0 0.0)
                                       :up (vec 0.0 1.0 0.0)
                                       :fovy 20
                                       :projection :camera-orthographic))
             (slot 10)
             (slot-ptr (cffi:foreign-alloc :int :count 1)))

        (set-material-shader (model-materials cube) shadow-shader)

        (setf (cffi:mem-aref slot-ptr :int 0) slot)
        (setf (cffi:mem-ref (car (cdr shadow-shader)) :int 11) shader-loc)

        (let ((ptr (cffi:foreign-alloc :float :count 3)))
          (setf (cffi:mem-aref ptr :float 0) (vx light-dir))
          (setf (cffi:mem-aref ptr :float 1) (vy light-dir))
          (setf (cffi:mem-aref ptr :float 2) (vz light-dir))
          (set-shader-value shadow-shader light-dir-loc ptr :shader-uniform-vec3))

        (let ((ptr (cffi:foreign-alloc :float :count 4)))
          (setf (cffi:mem-aref ptr :float 0) (vx light-color-norm))
          (setf (cffi:mem-aref ptr :float 1) (vy light-color-norm))
          (setf (cffi:mem-aref ptr :float 2) (vz light-color-norm))
          (setf (cffi:mem-aref ptr :float 3) (vw light-color-norm))
          (set-shader-value shadow-shader light-col-loc ptr :shader-uniform-vec4))

        (let ((ptr (cffi:foreign-alloc :float :count 4)))
          (setf (cffi:mem-aref ptr :float 0) 0.1)
          (setf (cffi:mem-aref ptr :float 1) 0.1)
          (setf (cffi:mem-aref ptr :float 2) 0.1)
          (setf (cffi:mem-aref ptr :float 3) 0.1)
          (set-shader-value shadow-shader ambient-loc ptr :shader-uniform-vec4))

        (let ((ptr (cffi:foreign-alloc :int)))
          (setf (cffi:mem-aref ptr :int) 1024)
          (set-shader-value shadow-shader (get-shader-location shadow-shader "shadowMapResolution") ptr :shader-uniform-int))

        (loop until (window-should-close)
              do
                (let ((ptr (cffi:foreign-alloc :float :count 3))
                      (light-view (3d-matrices:mat4))
                      (light-proj (3d-matrices:mat4)))
                  (setf (cffi:mem-aref ptr :float 0) (vx (camera3d-position camera)))
                  (setf (cffi:mem-aref ptr :float 1) (vy (camera3d-position camera)))
                  (setf (cffi:mem-aref ptr :float 2) (vz (camera3d-position camera)))
                  (set-shader-value shadow-shader
                                    (cffi:mem-ref (car (cdr shadow-shader)) :int 11) ptr :shader-uniform-vec3)
                  (update-camera camera :camera-orbital)

                  ;; render all objects into the shadowmap
                  (with-drawing
                    (with-texture-mode (shadow-map)
                      (clear-background :white)
                      (with-mode-3d (light-cam)
                        (setf light-view (rlgl:get-matrix-modelview))
                        (setf light-proj (rlgl:get-matrix-projection))
                        (draw-frame cube)))

                    (let ((light-view-proj (raymath:matrix-multiply light-view light-proj)))
                      (clear-background :raywhite)
                      (set-shader-value-matrix shadow-shader light-vp-loc light-view-proj))

                    (rlgl:active-texture-slot slot)
                    (rlgl:enable-texture (texture-id (render-texture-depth shadow-map)))
                    (rlgl:set-uniform shadow-map-loc slot-ptr 4 1)

                    (with-mode-3d (camera)
                      (draw-frame cube)))

                  (cffi:foreign-free ptr)))

        (unload-shader shadow-shader)
        (unload-model cube)
        (rlgl:unload-framebuffer (render-texture-id shadow-map))))))

(main)