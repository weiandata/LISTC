# LISTR 进阶操作指南(非编程人员版)

[English version](advanced-en.md) · [← 快速上手](quickstart-zh.md)

前提:你已经会用配置表跑出第一张表。本指南全部通过**配置表**完成,
仍然不需要写代码;每节末尾附上等价的 YAML 写法,供你把配置交给
同事或 AI 助手复用。

## 1. 一次产出多张表

"统计表"工作表里,**表名不同的行属于不同的表**。例如同时要
"分省概览"和"城乡对比"两张表,就写两组行,各自填行变量、列变量
和统计量。输出的 Excel 里每张表一个工作表。

## 2. 九种统计量速查

| 统计量(填在"统计量"列) | 含义 | 需要额外填 |
| --- | --- | --- |
| st_mean | 加权均值 | 变量 |
| st_sd | 加权标准差 | 变量 |
| st_prop_above | 达到阈值的人数占比 | 变量、阈值 |
| st_level_prop | 各等级人数占比 | 变量、等级分数线 |
| st_quantile | 分位数(默认中位数) | 变量 |
| st_count / st_wcount | 人数 / 加权人数 | — |
| st_pvalue | 题目正答率 | (需在数据角色里声明作答列) |
| st_option_dist | 题目选项分布(含缺失率) | 同上 |

"变量"可以填能力维度名(如 `math`)或数据里的数值列名。

## 3. 硬分类还是概率化("方法"列)

- 留空(硬分类):按分数线一刀切。简单直观,但分数线附近的人
  因测量误差被误分,占比会有偏。
- `prob`(概率化,推荐):利用每个人的能力值 ± 标准误,计算其
  落入各等级的**概率**再汇总。对误分类稳健,是 LISTR 的特色。
- "校正"列:能力值是 **EAP** 类(常见于 flexMIRT/mirt 输出)就
  **留空**;是 **WLE/ML** 类(常见于 Winsteps/ConQuest)就填
  `latent`,否则极端等级占比会被高估。不确定问提供能力值的同事
  "用的是 EAP 还是 WLE"。

## 4. 处理 PISA/TIMSS 类国际测评数据

这类数据用整群抽样,必须用**复制权重**才能算对标准误。在
"数据与角色"工作表:

| 设置项 | 填写值 |
| --- | --- |
| 复制权重列 | `W_FSTR`(前缀,自动展开为 W_FSTR1..80) |
| 复制权重方法 | `fay`(PISA)或 `jk2`(TIMSS) |

如果数据用 **plausible values**(每人多个能力值,如 PV1MATH 到
PV10MATH),在"能力维度"工作表把 **PV 列** 填成 `PV#MATH`
(`#` 代表编号),能力值列和标准误列留空。LISTR 会自动做 Rubin
合并;两者同时声明就是 PISA 官方的完整方差算法。
注意:PV 维度的"方法"列**不要**填 prob(PV 本身已包含测量不
确定性,程序会提醒你)。

## 5. 三种输出各有用途

| 输出 | 填在"输出"工作表 | 适合 |
| --- | --- | --- |
| Excel | Excel输出路径 | 日常交付、继续加工 |
| HTML | HTML输出路径 | 直接转发的成品报告(自带表格样式和解读) |
| JSON | JSON输出路径 | 给数据团队或 AI 系统做二次分析 |

三个都填就同时输出三份。

## 6. 读懂三种标准误

JSON 和长表里每个数字有三个 SE 分量:

- **se_sampling**:抽样误差——只调查了一部分人带来的不确定;
- **se_measurement**:测量/插补误差——考试本身测不准(或 PV
  之间的差异)带来的不确定;
- **se_total**:两者合并,**报告里引用这个**。

Excel 表格里括号中的就是 se_total。

## 7. 常用判断口径

- **显著差异**:|A − B| > 2 × √(SE_A² + SE_B²) 才说"显著高于"。
  "结论"工作表已按此规则自动判断。
- **小样本**:单元格 n < 30 时结果不稳定,"结论"表会自动标警告,
  报告中请谨慎引用。

## 8. 把配置交给 AI 助手

配置表和 YAML 完全等价。让 AI 助手(或同事)按下面模板生成
配置文本,你保存为 `config.yml` 后同样 `lst_run("config.yml")`:

```yaml
data: students.csv
roles:
  id: student_id
  weight: w_final
  group: [region, gender]
  pv: {math: "PV#MATH"}        # 或 theta/theta_se
  rep_weights: W_FSTR
  rep_method: fay
tables:
  - name: 分省概览
    rows: [region]
    margins: true
    values:
      平均能力: {stat: st_mean, var: math}
      优秀率: {stat: st_prop_above, var: math, cutoff: 1.2}
output:
  xlsx: 结果.xlsx
  html: 报告.html
```

包里自带给 AI 看的接口说明(`llms.txt`)和配置校验规则
(JSON Schema),直接把这两个文件发给 AI 助手,它就能替你写配置。
