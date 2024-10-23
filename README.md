# PL wiki
<https://plwiki.github.io>

## 환경 설정

### Nix 사용하기 (추천)
Nix 패키지 매니저를 설치하고 flake 기능을 활성시키세요.
`nix develop`을 입력해 nix로 설정된 셸에 들어갈 수 있습니다.
Nix를 사용하는 경우 cabal을 통해 빌드하는 것이 추천됩니다.

## 빌드하기

### cabal
```
cabal run
```

### stack
```
stack run
```

## 서빙하기
[serve](https://www.npmjs.com/package/serve)를 설치하고 다음 명령을 실행해 빌드된 사이트를 브라우저에서 확인할 수 있습니다.
```
serve ./site
```
