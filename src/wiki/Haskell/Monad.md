---
title: Haskell/Monad
categories: ["타입클래스"]
license: CC BY-SA 4.0
---

`Monad`는 하스켈의 타입클래스이다.
상위 클래스로 [Functor](Functor), [Applicative](Applicative)를 가진다.

모나드는 개념적으로 다음과 같이 정의될 수 있다.
```haskell
class Functor m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
```
