---
title: Haskell/Applicative
categories: ["타입클래스"]
license: CC BY-SA 4.0
---

`Applicative` (적용성 함자, applicative functor)는 하스켈의 타입클래스이다.
상위 클래스로 [Functor](Functor)를 가진다.
대표적인 하위 클래스로 [Monad](Monad)가 있다.

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```
