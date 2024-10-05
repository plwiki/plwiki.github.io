---
title: Rust
categories: ["프로그래밍 언어", "명령형 프로그래밍 언어"]
license: CC BY-SA 4.0
---

Rust는 [러스트 재단](https://foundation.rust-lang.org/)에 의해 개발 및 관리되고 있는 프로그래밍 언어이다. 

## 특징
소유권(OwnerShip)과 수명(Lifetime) 개념을 도입해 GC없는 메모리 관리를 한다. 

### 소유권
Rust에서 관리되는 모든 값(스택 메모리 상의 공간)은 특정 변수(혹은 구조체의 필드명, 매개변수)에 귀속되며 각 변수와 값은 매 순간 일대일로 대응된다. 만약 특정 값을 소유한 이름에 접근이 불가능해지는 순간 해당 값의 할당은 해제된다. 

```Rust
{
    let s = "hello";
}
// s가 선언된 scope를 벗어나 s에 접근이 불가능하기 때문에 "hello"라는 문자열이 할당된 공간은 해제된다.
```
즉 이를 통해 Rust는 [RAII](RAII) 를 강제하게 된다.

### 참조와 수명
Rust에서 특정 값을 소유권을 가진 변수외에 접근하는 방법으로 참조가 존재한다. 참조는 크게 가변참조(mutable reference)와 불변참조(immutable reference)로 나뉘는데 그에 대한 규칙은 다음과 같다.

* 가변으로 선언된 변수만 가변참조를 가질 수 있다
* 같은 변수에 대한 불변 참조는 각 시점마다 여러개 존재 가능하다
* 같은 변수에 대한 가변 참조는 각 시점마다 단 한개만 존재 가능하다
* 같은 변수에 대한 불변 참조와 가변 참조는 각 시점마다 같이 존재할 수 없다  

이는 다음과 같은 실수를 막기 위한 규칙이다.

```Rust
fn add(a : &mut usize){
    a+=1
}

fn main(){
    let mut a = 3;
    let b = &a; //b를 불변참조로 설정한 시점에서 b가 참조하는 값이 변하지 않기를 기대함을 알 수 있다
    let c = &mut a; //그러나 만약 여기서 불변참조와 같이 존재하는 가변참조를 생성한다면
    add(c);
    print!(b) // b가 참조하는 값이 이미 변해있을 수 있다
}
```
위와 같은 코드는 Rust 컴파일러가 컴파일 시점에 그 오류를 잡아낼 수 있다. 

한편 Rust는 수명이라는 개념을 통해 [Dangling Pointer](DanglingPointer) 문제를 미연에 방지한다. 수명이란 Rust에서 어떤 변수에 접근 가능한 구간이라고 생각할 수 있다. 그리고 모든 변수는 수명과 관련된 다음과 같은 규칙을 따른다.  

* 변수의 수명은 그 변수의 참조의 수명보다 반드시 길어야 한다.  

다음과 같은 예시를 보자.

```Rust
fn return_ref() -> &usize{
    let b = 7;
    &b
}

fn main(){
    let a = return_ref();
    print!(a); // 이 시점에서 7이라는 값은 이미 사라진 상태이다. 하지만 a는 그 값을 참조하고 있기 때문에 오류가 발생한다
}
```

위의 코드에서 발생하는 문제는 b라는 변수를 참조하는 변수인 a의 수명이 b의 것보다 길기 때문이다. 따라서 Rust 컴파일러는 해당 코드를 컴파일 하는 과정에서 오류를 낸다. 



## 외부 링크
* [러스트 재단](https://foundation.rust-lang.org/)
* [The Book](https://doc.rust-lang.org/book/)
* [Rust Playground](https://play.rust-lang.org/?version=stable&mode=debug&edition=2021)
