# haskell-learn

Study haskell functional programming!

You should go, because left is not right!

## 书籍

`Learn You A Haskell For Great Good`: [`Haskell`趣学指南](http://learnyouahaskell.com/chapters) [中文](https://learnyouahaskell.mno2.org/zh-cn)
`Real Haskell World`: [中文](http://cnhaskell.com/)

## 基本语法

haskell的一大特色是语法特色

1. 惰性求值
2. 模式匹配
3. 强类型系统、多态
4. Monad

> lisp使用模板-宏定义来实现更高级功能

### data|type|newtype 区别

```hs
-- data    关键字 introduce一个代数数据类型。albegraic
-- type    关键字 产生synonym，一个已经existing的类型的别名，可以交换使用。(interchangeably)
-- newtype 关键字 给一个existing的类型一个独特的身份(distinct identity)。新类型和老类型不可交换的(interchangeable)
```
