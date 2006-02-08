;; theme file, written Tue Oct  8 01:07:30 2002
;; created by sawfish-themer -- DO NOT EDIT!

(require 'make-theme)

(let
    ((patterns-alist
      '(("border_left"
         (inactive
          "border_left.png")
         (focused
          "border_left.png"))
        ("border_right"
         (inactive
          "border_right.png")
         (focused
          "border_right.png"))
        ("bottom_bar"
         (inactive
          "bottom_bar.png")
         (focused
          "bottom_bar.png"))
        ("bottom_left"
         (inactive
          "bottom_left-dim.png")
         (focused
          "bottom_left.png"))
        ("bottom_left-top"
         (inactive
          "bottom_left-top-dim.png")
         (focused
          "bottom_left-top.png"))
        ("bottom_right"
         (inactive
          "bottom_right-dim.png")
         (focused
          "bottom_right.png"))
        ("bottom_right-top"
         (inactive
          "bottom_right-top-dim.png")
         (focused
          "bottom_right-top.png"))
        ("close"
         (inactive
          "close-dim.png")
         (focused
          "close.png")
         (highlighted
          "close-hilight.png")
         (clicked
          "close-clicked.png"))
        ("iconify"
         (inactive
          "iconify-dim.png")
         (focused
          "iconify.png")
         (highlighted
          "iconify-hilight.png")
         (clicked
          "iconify-clicked.png"))
        ("maximize"
         (inactive
          "maximize-dim.png")
         (focused
          "maximize.png")
         (highlighted
          "maximize-hilight.png")
         (clicked
          "maximize-clicked.png"))
        ("menu"
         (inactive
          "menu-dim.png")
         (focused
          "menu.png")
         (highlighted
          "menu-hilight.png")
         (clicked
          "menu-clicked.png"))
        ("title-back"
         (inactive
          "title-back-dim.png"
          (tiled . t))
         (focused
          "title-back.png"
          (tiled . t)))
        ("menu-shaded"
         (inactive
          "menu-shaded-dim.png")
         (focused
          "menu-shaded.png")
         (highlighted
          "menu-shaded-hilight.png")
         (clicked
          "menu-shaded-clicked.png"))
        ("close-shaded"
         (inactive
          "close-shaded-dim.png")
         (focused
          "close-shaded.png")
         (highlighted
          "close-shaded-hilight.png")
         (clicked
          "close-shaded-clicked.png"))
        ("iconify-shaded"
         (inactive
          "iconify-shaded-dim.png")
         (focused
          "iconify-shaded.png")
         (highlighted
          "iconify-shaded-hilight.png")
         (clicked
          "iconify-shaded-clicked.png"))
        ("maximize-shaded"
         (inactive
          "maximize-shaded-dim.png")
         (focused
          "maximize-shaded.png")
         (highlighted
          "maximize-shaded-hilight.png")
         (clicked
          "maximize-shaded-clicked.png"))
        ("title-back-shaded"
         (inactive
          "title-back-shaded-dim.png")
         (focused
          "title-back.png"))))

     (frames-alist
      '(("normal"
         ((right-edge . 14)
          (top-edge . -19)
          (font . "-microsoft-verdana-medium-r-normal-*-*-110-*-*-p-*-koi8-ru")
          (left-edge . 14)
          (below-client . t)
          (x-justify . 6)
          (y-justify . center)
          (text . window-name)
          (foreground . "#000000000000")
          (background . "title-back")
          (class . title))
         ((left-edge . -6)
          (background . "menu")
          (top-edge . -19)
          (class . menu-button))
         ((top-edge . 0)
          (bottom-edge . 16)
          (left-edge . -6)
          (below-client . t)
          (background . "border_left")
          (class . left-border))
         ((bottom-edge . 16)
          (top-edge . 0)
          (right-edge . -6)
          (below-client . t)
          (background . "border_right")
          (class . right-border))
         ((background . "close")
          (right-edge . -6)
          (top-edge . -19)
          (class . close-button))
         ((background . "iconify")
          (right-edge . 33)
          (top-edge . -19)
          (class . iconify-button))
         ((right-edge . 14)
          (background . "maximize")
          (top-edge . -19)
          (class . maximize-button))
         ((background . "bottom_bar")
          (bottom-edge . -6)
          (right-edge . 16)
          (left-edge . 16)
          (class . bottom-border))
         ((bottom-edge . -6)
          (background . "bottom_left")
          (left-edge . -6)
          (class . bottom-left-corner))
         ((bottom-edge . 0)
          (left-edge . -6)
          (background . "bottom_left-top")
          (class . bottom-left-corner))
         ((right-edge . -6)
          (background . "bottom_right")
          (bottom-edge . -6)
          (class . bottom-right-corner))
         ((bottom-edge . 0)
          (right-edge . -6)
          (background . "bottom_right-top")
          (class . bottom-right-corner)))
        ("shaded"
         ((background . "title-back-shaded")
          (right-edge . 14)
          (font . "-microsoft-verdana-medium-r-normal-*-*-110-*-*-p-*-koi8-ru")
          (left-edge . 14)
          (top-edge . -19)
          (below-client . t)
          (x-justify . 6)
          (y-justify . center)
          (text . window-name)
          (foreground . "#000000000000")
          (class . title))
         ((left-edge . -6)
          (background . "menu-shaded")
          (top-edge . -19)
          (class . menu-button))
         ((top-edge . -19)
          (background . "close-shaded")
          (right-edge . -6)
          (class . close-button))
         ((right-edge . 33)
          (background . "iconify-shaded")
          (top-edge . -19)
          (class . iconify-button))
         ((right-edge . 14)
          (background . "maximize-shaded")
          (top-edge . -19)
          (class . maximize-button)))))

     (mapping-alist
      '((default . "normal")
        (transient . "normal")
        (shaped . "shaded")
        (shaped-transient . "shaded")
        (unframed . "nil")))

     (theme-name 'Bluecurve-Prion))

  (add-frame-style
   theme-name (make-theme patterns-alist frames-alist mapping-alist))
  (when (boundp 'mark-frame-style-editable)
    (mark-frame-style-editable theme-name)))
