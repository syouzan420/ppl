# ppl

#### Peoplemon をソースコードから實行する
Peoplemon は Alex Stuart氏による ポケモンのパロディとも言ふべき作品である
https://www.reddit.com/r/haskell/comments/c29lks/peoplemon_an_allhaskell_roleplaying_game/
このゲームの製作には FRPのコンセプトに基づいた Yampaといふライブラリが使はれており これを學ぶ目的としても ソースコードからコンパイルしたかったのだが 今まで全然できなかった
しかし 今回 GHC 9.8.4 の環境で `stack` を使ひ `package.yaml` や `stack.yaml` などを編集したあと エラーを修復していくことで 最終的に實行することができたので その過程を紹介する
ちなみに OSは Nix 2.28.5 を使用してゐる

1. ソースコード入手
https://hub.darcs.net/linearity/pplmonad
2. offline といふフォルダ(画像・音樂データなどが入ってゐる)
https://linearity.itch.io/peoplemon
から實行ファイルとともにダウンロード
3. 適當なディレクトリへ移動し
`stack new ppl --resolver=lts-23.28`
を實行
`ppl`は作りたいフォルダ名であり 何でもよい
`resolver=lts-23.28`は コンパイラのヴァージョンを指定しており
自分は`ghc 9.8.4`といふコンパイラを使ってゐるので
それに對應するものを
https://www.stackage.org/ で確認して記述した
4. 作成したフォルダ(ディレクトリ:以下pplフォルダと呼ぶ)に 1. で入手したソースコードの`src`フォルダの中身を すべて pplフォルダにある`src`フォルダにコピーする(もともとある`Lib.hs`は消してもよい)
5. pplフォルダ内の`package.yaml`を編集する
`dependencies:`の項目に
`- base >= 4.7 && < 5` と書かれてゐるが その下に次を追加する
```
- array
- containers
- mtl
- random
- text
- time
- executable-path
- filepath
- simple-affine-space
- automaton
- sdl2
- sdl2-image
- sdl2-mixer
- Yampa
```
さらに
`executables:`の項目の
`source-dirs:`の右に`app`と書かれてゐるが それを消して`src`とする
以上の變更のあと 保存する
6. pplフォルダ内の`stack.yaml`を編集する
まづ `extra-deps:` といふ項目があるので
もし この項目の前に`#`が付いてゐたら消して さらに`:`の後に`[]`と書かれてゐたらそれも消しておき
この行を含めて
```
extra-deps:
- dunai-0.13.0@sha256:460bb9f5157a031746588c544b55f7ef5781ec5978bd75a9b1aad210599d6e79,7288
- sdl2-2.5.5.0@sha256:9d1125181e651518ea960738266e3cef98c0c4fa2d5f9c35e67f4b0f87fd1464,12695
- sdl2-image-2.1.0.0@sha256:777291e9edb09a49628b9b776ebd8e8b25527e46538cec9b2d397638db2a94b7,2098
- sdl2-mixer-1.2.0.0@sha256:6a6b5a46c035c9e77eaf9c244e45de9e4a9b9a110890cfeeb9fa72faa2419cef,4497
```
と追加する
また このファイルは ほとんどがコメントで何も實行されないから どこに書いてもいいのだが なるべく最後の方に まづ
`allow-newer: true`
と書き(左端に寄せる)
その後に 次のくだりを入れる
```
nix:
  enable: true
  packages: [pkgconfig,SDL2,SDL2_image,SDL2_mixer,libtiff,harfbuzz,libwebp,glib,pcre2,libsndfile,libpulseaudio,alsa-lib,jack2,libGLU,lerc,libsysprof-capture]
```
7. pplフォルダ内(ルート)で `stack run` を實行する
8. エラーがたくさん出て來るので ひとつづつ對処する
 - `Tuner.hs` で `Control.Monad.Trans.MStreamF` がない と言はれるのだが これは `Data.Automation.Trans.Reader` で代用する
 - `Tuner.hs` で `(up controls, down controls, left controls, right controls)` の `up` や `down` など `controls` の前にあるものを消すだけで エラーはなくなり 實行できる (その後 本來の擧動が實現できるやうに修正した)
 - 二つのモジュールに 同じ名前の函數があって競合する といふ場合 オリジナルで作成された(ソースコードにある)モジュールからの函數を使ふやうにし まう一つのモジュールには `hiding` をつけて 使用させないやうにした
 - `FRP.Yampa.Geometry` といふモジュールは 現在の`Yampa`には存在しないので `simple-affine-space` 由來の `Data.Point2` や `Data.Vector2` そして `Data.AffineSpace` や `Data.VectorSpace` に置きかへなければならない(それぞれ 使はれてゐる型や函數を調べて それに對應するモジュールをインポートした) (ちなみに これを訂正する箇所が一番多かった)
 - 實行ファイルのダウンロードとともに入手した `offline` フォルダを pplフォルダにあらかじめ コピーしておくか これに關するエラーが出た場合は 指定の箇所に `offline`フォルダをコピーする

このやうにして修正したものは 一應 github にのせておいた (LICENSE は元のソースからコピーした)


