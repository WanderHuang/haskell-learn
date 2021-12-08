---
title: css样式动态变更
author: wander
date: 2021-12-08
tags: css
---

制作动态的主题样式，无非就是修改css的值，现在有好几个方案可以做，这里记录思路。

## 1. 打包不同的主题包

在发布的时候就打包成无数个内置的主题包，然后默认有一种格式。

```
theme-blue.css
theme-red.css
theme-default.css
```

然后在代码中去修改引用主题的位置。

```javascript
// <link class="theme" href="" rel="stylesheet" />
document.querySelector('link.theme')?.setProperty('href', `xxxxx-${theme}`);
```

这种方式优点

- 好上手，一般由UI定制几个主图，然后用`css`预处理器处理出来就行了
- `js`处理也不复杂

不好的点

- 只能设置预设的值，不能修改更多

## 2. 用css变量

这种方式可以支持任意主题，变量值你还可以缓存到`sessionStorage`里面，扩展性非常高。

```css
:root {
  --theme-color: blue;
}
```

在顶部定义`:root`之后，在任意`css`文件中都可以引用。

```css
.button {
  background: var(--theme-color);
}
```

然后你可以在`js`中修改这个参数

```js
document.documentElement.style.setProperty('--theme-color', 'red');
```

这种方式的优点

- 比第一种还简单，而且不需要发起`http`请求，浏览器也只需要做一个重绘，性能非常好，立时生效
- 现代浏览器支持程度都不错
- 可以定制的范围很大，理论上任意值都可以修改

缺点

- 属于`css3`内容，不是所有浏览器都支持
- 需要在`js`和后台维护一个变量列表（不过也不复杂）

## 3. 使用filter

`css filter`是个骚操作，不过扩展性也不高，不推荐用在生产环境。

## 4. 纯js

纯`js`的方案就广泛了，可以完全用`js`生成文本去替换。


-----

> 总的来说，推荐使用第二个方案，生产使用也很不错，可以快速支持多主题定制包。

