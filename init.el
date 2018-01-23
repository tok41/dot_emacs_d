;; load-path
(let ((default-directory (expand-file-name "~/.emacs.d/elpa")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path)))

;; setting of package.el
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)
;(package-initialise)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; 日本語入力モード
;;Japanese
;(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; 見た目の設定
;; color thema
(load-theme 'wombat t)

;; 背景色を半透明にする
(if window-system (progn
  (set-frame-parameter nil 'alpha 90)
  ))

;; メニューバーを消す
(menu-bar-mode -1)
;;;;; M-x menu-bar-mode で、メニューバーを表示/非表示


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; 基本動作
;; スタートアップページを表示しない
(setq inhibit-startup-message t)

;; バックアップファイルを作成しない
(setq make-backup-files nil)

;; オートセーブファイルを作成しない
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; 入力補助
;; 現在行をハイライト
(global-hl-line-mode t)

;; 現在行に下線を表示
(setq hl-line-face 'underline)
(global-hl-line-mode)

;; タブ幅の指定
(setq default-tab-width 4)

;;; 対応する括弧を光らせる。
(show-paren-mode 1)

;; 対応する括弧の色の設定
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "grey")
(set-face-foreground 'show-paren-match-face "black")

;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;;; 画面の左側に行番号を表示
(require 'linum)
(global-linum-mode 1)

;;; カーソルの位置が何行目かを表示する
(line-number-mode t)

;; 非アクティブウィンドウの背景色を設定
(require 'hiwin)
(hiwin-activate)
(set-face-background 'hiwin-face "gray30")

;; タイトルにフルパス表示
(setq frame-title-format "%f")
;;current directory 表示
(let ((ls (member 'mode-line-buffer-identification
                  mode-line-format)))
  (setcdr ls
    (cons '(:eval (concat " ("
            (abbreviate-file-name default-directory)
            ")"))
            (cdr ls))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; キーバインド
(define-key global-map "\C-h" 'delete-backward-char) ; 削除
(define-key global-map "\M-?" 'help-for-help)        ; ヘルプ
(define-key global-map "\C-z" 'undo)                 ; undo
(define-key global-map "\C-ci" 'indent-region)       ; インデント
(define-key global-map "\C-c\C-i" 'dabbrev-expand)   ; 補完
(define-key global-map "\C-c;" 'comment-region)      ; コメントアウト
(define-key global-map "\C-c:" 'uncomment-region)    ; コメント解除
(define-key global-map "\C-c " 'other-frame)         ; フレーム移動

;;; redo
(require 'redo+)
(global-set-key (kbd "C-z") 'redo)

;;;;; 分割ウィンドウを矢印キーで移動する
(windmove-default-keybindings) ; 引数なしの場合は Shift
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;;;; 分割Windowのサイズ変更
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))
(global-set-key "\C-c\C-r" 'window-resizer)

;; アクティブウィンドウのサイズを自動調整
(require 'golden-ratio)
(golden-ratio-mode 1)
(add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; マウス操作
;; スクロールは1行ごと
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))

;; bufferの最後でカーソルを動かそうとしても音をならなくする
(defun next-line (arg)
  (interactive "p")
  (condition-case nil
      (line-move arg)
    (end-of-buffer)))

;; エラー音をならなくする
(setq ring-bell-function 'ignore)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; git
;; git-gutter 差分の表示
(require 'git-gutter)
(require 'git-gutter-fringe)
(global-git-gutter-mode 1)

;; ファイル編集時にbufferを再読込
(global-auto-revert-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Coding
;; auto-compete
(require 'popup)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(ac-config-default)

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"       ;; personal snippets
        "~/.emacs.d/elpa/snippets"  ;; official snippet collections
        ))
(yas-global-mode 1)

;; dumb-jump
(require 'dumb-jump)
(setq dumb-jump-mode t)
(setq dumb-jump-use-visible-window nil)
(define-key global-map [(super d)] 'dumb-jump-go) ;; go-to-definition!
(define-key global-map [(super shift d)] 'dumb-jump-back)

;; diff
;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)

;; flymake : コードを動的にチェックしてくれる
(require 'tramp-cmds)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
     ; Make sure it's not a remote buffer or flymake would not work
     (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "pyflakes" (list local-file)))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
;; エラーをミニバッファに表示
(defun flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
	(let ((help (get-char-property (point) 'help-echo)))
	  (if help (message "%s" help)))))
(add-hook 'post-command-hook 'flymake-show-help)


;;;;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;;;; Python
(require 'python-mode)
;; autopep8
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;; pyflakes
(add-hook 'python-mode-hook
          (lambda ()
            (flymake-mode t)))


;;;;; C
;; Cモード
(require 'cc-mode)
(add-hook 'c-mode-common-hook
		  (lambda ()
			(setq c-default-style "k&r") ;; カーニハン・リッチースタイル
			(setq indent-tabs-mode nil)  ;; タブは利用しない
			(setq c-basic-offset 2)      ;; indent は 2 スペース
			))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; CMake-mode
(require 'cmake-mode)
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))



