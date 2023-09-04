# JQ-Card 插件介绍手册

# 1、 CSS布局

- [x] 兼容分辨率 1920 * 1080

视图标签 头部高度

![](/Users/rain/Library/Application%20Support/marktext/images/2022-07-29-17-15-55-image.png)

顶部高 84 

![](/Users/rain/Library/Application%20Support/marktext/images/2022-07-29-16-46-37-image.png)

<div>
margin-top:14px;
height: 70px; padding:13px;
border: 1px solid #e1e1e1;
</div>

分页标签栏高度 40 + 15 px

总体高度 134 +40 + 15 = 189px

数据体  100%- 15= 175px；

 calc(100vh - 189px)

# 2、参数

## 2.1 title

 控制card视图中 左侧的数列文字，超出部分影藏，显示title 提示栏 ~~支持Solt自定义。~~

<mark>移除Solt 自定义损毁布局。</mark>

![](/Users/rain/Library/Application%20Support/marktext/images/2022-07-29-11-16-17-image.png)

## 2.2  tags

控制视图中标签区域 ，当不传入tags参数 默认会影藏 tags 标签 并影藏 oparte，新增状态下 没有任何标签

<img title="" src="file:///Users/rain/Library/Application%20Support/marktext/images/2022-07-29-10-48-47-image.png" alt="" data-align="center" width="835">

数据结构，按照对象数组形式传递 ，标签名称和值

```
     this.tags = [{'客户名称': data.companyName}, {'实施类型': data.productTypeNames}, {'合同编号': data.contractNo}, {'签订时间': data.signDate}]
```

## 2.3 row

  card 视图数据 ，视图数据主要提供业务支撑， 若视图数据传输的为空数据，则显示 new 插槽的业务数据。提示用户这个是一个新增的Card标签， 该数据 在打开时接收，关闭时设置为null，并将数据通过关闭事件，向上层组件传递。

## 2.4 show

    控制Card视图显示及关闭的参数。 默认值为false ， 参数接收后card 会显示 并增加动画效果。

# 三、Slots

## 3.1  new Slot



# 四、Event


