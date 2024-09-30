---
title: Nix
categories: ["프로그래밍 언어", "함수형 프로그래밍 언어"]
mathMethod: MathJax
license: CC BY-SA 4.0
---

Nix는 선언적 패키지 관리를 위한 소프트웨어 도구이다.

## Nix 패키지 매니저
패키지 빌드 및 설치를 담당하는 패키지 관리 소프트웨어이다.

## Nix 언어
Nix의 패키지의 빌드 과정을 기술하기 위한 순수 함수형 프로그래밍 언어이다.

## NixOS
Nix를 시스템 패키지 관리 프로그램으로 사용하는 리눅스 배포판이다.

## nixpkgs
[Nixpkgs](https://github.com/NixOS/nixpkgs)는 nix 패키지 매니저로 설치될 수 있는 소프트웨어 패키지 모음이다.
100,000개가 넘는 패키지들이 관리되고 있다.

## 설치하기
[공식 설치 안내](https://nixos.org/download/) 사이트를 참조하라.

### flake
Nix flake는 nix 패키지 매니저의 실험적 기능으로, 기본적으로 비활성화 되어 있다.
Flake 기능을 사용하기 위해서는 `~/.config/nix/nix.conf` 혹은 `/etc/nix/nix.conf` 파일에 다음을 추가하라.
<!-- 아래 코드는 nix 언어가 아니다. 수정 필요. -->
```nix
experimental-features = nix-command flakes
```

## 외부 링크
* [공식 웹사이트](https://nixos.org)
* [비공식 위키](https://nixos.wiki/wiki/Main_Page)
