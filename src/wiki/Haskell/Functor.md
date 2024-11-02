---
title: Haskell/Functor
categories: ["타입클래스"]
license: CC BY-SA 4.0
---

`Functor`는 하스켈의 타입클래스로, List, Maybe등 타입의 "map" 동작을 일반화해 정의한 인터페이스이다.
대표적인 하위 클래스로 [Applicative](Applicative)와 [Monad](Monad)가 있다.

`Functor` 클래스를 정의하는 코드는 다음과 같다.
```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

`fmap` 함수는 다음의 두 조건을 만족하여야 한다.
```
fmap id = id
fmap (f . g) = fmap f . fmap g
```
