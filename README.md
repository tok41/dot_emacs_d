# dot emacs d

# config file
- `~.emacs, ~/.emacs.el, ~/.emacs.d/init.el`の順番に検索される
  - [参考](https://qiita.com/tadsan/items/a2018379ffaadf07a418)
  - 最初に見つかった設定ファイルが反映される
- ここではgitでディレクトリ毎管理すると楽なので、`~/.emacs.d/init.el`を作成

# 設定
## load-path
- 指定ディレクトリ以下のサブディレクトリにパスを通す
```
;; load-path
(let ((default-directory (expand-file-name "~/.emacs.d/elpa")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path)))
```

## Color-Thema
- デフォルトでインストールされているテーマを使う
  - テーマは[このページ](http://aoe-tk.hatenablog.com/entry/20130210/1360506829)などを参考に選ぶ
  ```
  (load-theme 'wombat t)
  ```
- デフォルト外のテーマを使う場合は、以下のページを参考にしたらできるかも
  - [ここ](https://github.com/emacs-jp/replace-colorthemes)を参考に

## 日本語対応
- emacs-mozcを使う
  - パッケージのインストールが必要
  ```
  $ sudo apt-get install emacs-mozc emacs-mozc-bin
  
  # init.elに記述
  ;;Japanese
  (require 'mozc)
  (set-language-environment "Japanese")
  (setq default-input-method "japanese-mozc")
  (prefer-coding-system 'utf-8)
  ```

## アクティブウィンドウと非アクティブを色分け
- hiwin.elをインストール
```
M-x package-list-packages
hiwin
```
- init.elに記載
```
(require 'hiwin)
(hiwin-activate)
(set-face-background 'hiwin-face "gray30")
```

## 行番号を左側に表示
- built-inされているlinum.elを使う
  - 無い場合は、`M-x package-list-packages`でインストールできると思う
- M-x linum-mode これで行番号表示のモードになるが、init.elに設定
```
(require 'linum)
(global-linum-mode 1)
```
- `M-x linum-mode`で表示を消せる

## ウィンドウサイズの自動調整（黄金比）
- 分割ウィンドウのアクティブウィンドウのサイズをちょうどよく変更してくれる
```
M-x package-list-packages
golden-ratio
```

## redo
- redo+をインストール
  - キーバインドは`Ctrl+z`とした
```
M-x package-list-packages
redo+

# init.el
(require 'redo+)
(global-set-key (kbd "C-z") 'redo)
```

## git-gutter-fringe
- gitのコミット済みのコードとの差分のある箇所を表示する
  - git-gutterをlinumモードに対応させたもの
  ```
  M-x package-list-packages
  git-gutter-fringe
  ```
  - elファイルは以下の3つのリンクを貼る
    git-gutter.elc
    git-gutter-fringe.elc
    fringe-helper.elc
- init.el
```
;; git-gutter 差分の表示
(require 'git-gutter)
(require 'git-gutter-fringe)
(global-git-gutter-mode 1)

;; ファイル編集時にbufferを再読込
(global-auto-revert-mode 1)
```

## markdown
- markdown-mode
  - txt, markdown, mdの拡張子のファイルにmark-downモードを適用する
  - C-c, C-c, vでブラウザでプレビュー
  - 詳細は[公式](https://jblevins.org/projects/markdown-mode/)参照
  ```
  M-x package-list-packages
  markdown-mode
  # init.el
  (autoload 'markdown-mode "markdown-mode"
	  "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  ```
- markdownコマンドのインストールが必要
    $ brew install markdown
    など。multimarkdownでも良いらしい

## python-mode
- python mode
```
M-x package-list-packages
python-mode

# init.el
(require 'python-mode)
```

## yasnippet
- 短いフレーズで定形文を挿入する
  - snippetファイルを作成して、定型文を登録する
  ```
  M-x package-list-packages
  yasnippet
  ```
  - snippets collectionを入れる
  ```
  M-x package-list-packages
  yasnippet-snippets
  ```
  - init.elに以下の設定
  - elpa以下にsnippetsコレクションが入るので、パスを通す
  ```
  (require 'yasnippet)
  (setq yas-snippet-dirs
	  '("~/.emacs.d/snippets"       ;; personal snippets
		"~/.emacs.d/elpa/snippets"  ;; official snippet collections
        ))
	(yas-global-mode 1)
	```
- emacsから`M-x yas-describe-tables`でsnippetの詳細を調べることができる
- Trouble-Shoot
  - snippets/go-mode/default でエラーが出る場合があり、強制的に削除
- 参考（公式）
  - https://github.com/joaotavora/yasnippet
  - https://github.com/AndreaCrotti/yasnippet-snippets

## autopep8
- pep8コード規準に従ってコードの整形をしてくれる（保存時）
- autopep8をインストール
```
$ pip install autopep8
```
- emacsにautopep8.elを導入
```
M-x package-list-packages
py-autopep8

# init.el
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
```

## auto-complete
- 入力補完
  - popupというパッケージが見つからないというエラーが出るが、インストール済みになっていた。一度deleteして再度インストールし直し、popupを明にrequiredで呼び出すことで一応は解決
  ```
  M-x package-list-packages
  auto-compete
  
  # init.el
  (require 'popup)
  (require 'auto-complete-config)
  (global-auto-complete-mode t)
  (ac-config-default)
  ```

## flymake
- コード上のエラーや警告を自動で評価してくれる
  - [こちら](http://hamukazu.com/2014/12/05/setting-emacs-for-python/)の記述を丸々コピーしただけ
- pyflakesをインストール
```
$ pip install pyflakes
```
- emacsで`flymake, flymake-cursor, flymake-easy, flymake-python-pyflakes`をインストール
```
M-x package-list-packages
flymake
flymake-cursor
flymake-easy
flymake-python-pyflakes

# init.el
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
 
(add-hook 'python-mode-hook
          (lambda ()
            (flymake-mode t)))
```

## dumb-jump
- 関数などの定義元に飛べる
  - [こちら](https://qiita.com/blue0513/items/c0dc35a880170997c3f5#dumb-jump)を丸コピーさせてもらいました
  - super+d -> 定義にジャンプ
  - super+shifr+d -> 元の位置に戻る
```
M-x package-list-packages
dumb-jump

# init.el
(require 'dumb-jump)
(setq dumb-jump-mode t)
(setq dumb-jump-use-visible-window nil)
(define-key global-map [(super d)] 'dumb-jump-go)
(define-key global-map [(super shift d)] 'dumb-jump-back)
```


## cmake-mode
- CMakeLists.txtの編集モード
  - 便利な使い方は後で調べる
  - https://cmake.org/Wiki/CMake/Editors/Emacs
```
M-x package-list-packages
cmake-mode

# init.el
(require 'cmake-mode)
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))
```



# 試したいパッケージ等
- [ここ](http://emacs.rubikitch.com/tag/recommended-packages/)をチェック
- elscreen
  - windowをタブ化してくれるらしい
  - [参考](https://qiita.com/blue0513/items/ff8b5822701aeb2e9aae)
- neotree
  - サイドバーにdirectory構成を表示してくれるらしい
  - [参考](https://qiita.com/blue0513/items/ff8b5822701aeb2e9aae)
- fontの設定
  - font種類
  - サイズ
- 検索、補完機能の強化
  - [参考](https://qiita.com/blue0513/items/c0dc35a880170997c3f5)
- magit
  - gitクライアント
  - [参考](https://qiita.com/maueki/items/70dbf62d8bd2ee348274)
- elpy or jedi
  - 入力補完
  - companyというパッケージもあるらしい
  - auto-completeは入れてあるが、試したい
- elm-mode

