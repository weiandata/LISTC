# LISTR 设计文档 v1

Large-scale Item-response Statistics Tables in R

状态:草案(2026-07-18) 负责人:Ma Kunxiang

## 1. 定位与创新点

LISTR 是一个把测评/调查样本数据转化为可自由定制的透视统计表的 R 包。
输入是含人口学变量、样本 ID、作答、得分、权重、能力值(theta)、IRT
标准误等列的数据;输出是行、列、单元格内容均可自主指定的统计表,
并且所有统计量附带测量学上正确的标准误。

现有生态没有同时覆盖这两端:

| 包 | 权重/PV/SE | 透视式自由布局 | 备注 |
| --- | --- | --- | --- |
| EdSurvey | 有(replicate weights + PV) | 无 | 按统计量逐个调函数 |
| intsvy | 有 | 无 | 按国际测评项目硬编码 |
| Rrepest | 有 | 弱(扁平表) | OECD,replicate weights |
| BIFIEsurvey | 有 | 无 | 引擎强,无展示层 |
| pivottabler | 无 | 有 | 不懂权重与测量误差 |

LISTR 的三个创新点:

1. 统计引擎 + 透视布局一体:像 Excel 数据透视表一样声明行、列、
   值,但每个单元格是带 SE 的加权统计量。
2. 个体 IRT 标准误的误差传递:利用每个样本自带的 theta SE,把
   测量误差传递进群体统计量(见 §6),这是上述所有包都不做的
   (它们依赖 PV 或 replicate weights)。
3. 概率化等级划分:划等级时不做硬分类,而是用每人的
   theta ± SE 算出落入各等级的概率再聚合,得到对测量误差
   稳健的等级占比(§5.2)。

### 1.1 三类用户与接口分层(2026-07-18 新增)

同一个计算内核,三层入口,三种输出:

| 用户 | 交互入口 | 主要输出 |
| --- | --- | --- |
| 不懂统计/R 的调查人员 | 配置模板 + `lst_run()` 一键运行 | Excel 透视表 + 自动解读文字 |
| 懂 R 的统计人员 | `lst_data() → st_* → lst_table()` 完整函数 API | `listr_table` 对象、tidy 长表、SE 全分量 |
| AI agent | `lst_run(config)`,config 为 JSON/YAML 文本 | JSON 结果 + 计算日志;随包附 llms.txt 与配置 JSON Schema |

关键设计:配置文件是新手模板和 agent 接口的同一套机制。

- 配置内容:数据文件路径、变量角色映射、分数线、要生成的
  表(rows/cols/values/format)、输出目标(xlsx/json)
- 配置载体:YAML/JSON(agent 生成),或按模板填写的 Excel
  配置簿(调查人员填写,结构与 YAML 一一对应)
- `lst_run(config)` = 读配置 → 校验(错误信息用非技术语言,
  中文,指出配置文件的哪一格有问题)→ 执行 → 写出全部输出
- `inst/llms.txt`:面向 LLM 的紧凑 API 文档;
  `inst/schema/config.schema.json`:配置的 JSON Schema,
  agent 可据此自校验
- 自动解读文字:模板化规则生成(非 LLM),如"A 地区均分
  显著高于 B 地区(差异超过 2 倍标准误)",防止读者误读 SE;
  写入 Excel 的"结论"sheet,也进入 JSON 输出的
  `interpretation` 字段

## 2. 核心数据模型

### 2.1 listr_data

`listr_data` = data.frame + 变量角色说明(spec)。角色:

- `id`:样本唯一标识
- `group`:人口学/分组变量(可多个;保留值标签)
- `weight`:抽样权重(缺省为 1)
- `score`:观察得分(可多个,如分量表)
- `theta`:能力值(可多个)
- `theta_se`:与 theta 一一配对的 IRT 标准误
- `resp`:题目作答列(v1 仅存储,不计算)
- 其余列保留为普通变量

```r
d <- lst_data(df,
  id     = student_id,
  group  = c(region, gender, grade),
  weight = w_final,
  theta  = c(math = th_math, read = th_read),
  theta_se = c(math = se_math, read = se_read),
  score  = raw_total)
```

spec 校验:theta 与 theta_se 必须成对;权重非负;id 唯一。

### 2.2 设计原则

- spec 与数据分离,同一份 spec 可复用于多批数据文件。
- 所有中间结果是 tidy tibble,可进入任何 tidyverse 流程。
- 统计函数纯函数化,便于测试与后续并行。

## 3. 导入层

`read_listr(path, ...)` 按扩展名分派:

| 格式 | 后端 |
| --- | --- |
| .csv / .tsv / .txt | utils/readr |
| .xlsx / .xls | readxl |
| .sav / .zsav | haven(保留变量标签、值标签) |
| .dta | haven |
| .sas7bdat | haven |

SPSS/Stata 值标签自动转为带 label 属性的 factor,供分组显示。

IRT 软件人参数文件专用解析器(v1 先实现前两个):

- `read_winsteps_pfile()`:Winsteps PFILE(theta、SE、拟合量)
- `read_conquest_person()`:ConQuest show/plausible 文件
- `read_flexmirt_scores()`:flexMIRT -sco 输出(v1.1)

解析器输出统一列名(theta, theta_se, id),可与主数据按 id 合并:
`lst_join_person(d, pfile)`。

## 4. 派生计算层

在 spec 声明之后、统计之前,对数据做派生:

- `lst_classify(var, breaks, labels)`:按多个分数线切等级,
  生成有序 factor(硬分类)
- `lst_above(var, cutoff)`:是否超过阈值的 0/1 指示变量
- `lst_derive(...)`:任意自定义 mutate 式计算
- 概率化版本(见 §5.2)由统计层直接调用,不落地为列

分数线支持命名向量:`breaks = c(不及格 = -Inf, 及格 = 0.5, 优秀 = 1.2)`。

### 4.1 估计类型校正(2026-07-18 依据数值实验修订)

蒙特卡洛实验(见 test-variance.R)得到与最初设想相反的结论:

- **EAP + 后验 SD**:概率化公式 p = 1 − Φ((c−θ)/se) 本身就是
  后验概率,聚合后对潜在分布的等级占比**已经校准**,不需要
  也不应该做反收缩(实验中反收缩把 0.116 的正确估计推到
  0.185,真值 0.1165)。
- **WLE/ML + 抽样 SE**:观测分布比潜在分布更散,naive 概率化
  会高估极端等级占比。校正方法是经验贝叶斯收缩到正态模型
  后验:ρ = 1 − mean_w(se²)/var_w(θ),θ* = μ + ρ(θ−μ),
  se* = √ρ·se,再套概率化公式(实验验证更接近真值)。

因此 `correction` 参数定义为:

- `"none"`(默认):适用于 EAP + 后验 SD
- `"latent"`:适用于 WLE/ML + 抽样 SE,做上述 EB 收缩;
  ρ 可由数据估计或用户经 `rho` 显式提供

## 5. 统计量

每个统计量返回 estimate + SE(+ 可选 CI)。v1 集合:

| 名称 | 说明 |
| --- | --- |
| `st_mean` | 加权均值(得分、theta 或任意数值列) |
| `st_prop_above` | 超过阈值人数占比 |
| `st_level_prop` | 各等级人数占比(硬分类或概率化) |
| `st_quantile` | 加权分位数 |
| `st_count` / `st_wcount` | 人数 / 加权人数 |
| `st_sd` | 加权标准差 |
| `st_pvalue` | 题目加权正答率/平均得分率(resp 列) |
| `st_option_dist` | 题目选项分布(含缺失率) |

题目层统计复用同一透视引擎:行 = 题目,列 = 人群,
单元格 = 正答率(SE)。resp 列在 spec 中声明计分键
(`key =`)后即可参与统计。

### 5.1 加权公式

加权均值 x̄ = Σwᵢxᵢ / Σwᵢ。抽样方差用带权重的线性化估计:

Var_samp(x̄) = Σwᵢ²(xᵢ − x̄)² / (Σwᵢ)²

占比是 0/1 变量的加权均值,同一公式。

### 5.2 概率化等级占比(创新)

对每个样本,基于 θᵢ 与 seᵢ 的正态近似,其真值落入等级 k
(分数线 c_k 到 c_{k+1})的概率:

p_{ik} = Φ((c_{k+1} − θᵢ)/seᵢ) − Φ((c_k − θᵢ)/seᵢ)

等级 k 的占比 = Σwᵢ·p_{ik} / Σwᵢ。相比硬分类,该估计对
分数线附近样本的误分类不敏感。`st_prop_above` 同理提供
`method = "prob"`:pᵢ = 1 − Φ((c − θᵢ)/seᵢ)。

## 6. 误差传递(SE 引擎)

总方差 = 抽样方差 + 测量方差,两部分独立相加
(与 PISA 合并抽样方差和插补方差的思路同构,但测量方差
由个体 SE 直接传递,不需要 PV)。

均值的测量方差(delta 法,theta 间独立):

Var_meas(θ̄) = Σwᵢ²·seᵢ² / (Σwᵢ)²

概率化占比的测量方差:pᵢ 是 θᵢ 的函数,
∂pᵢ/∂θᵢ = φ((c − θᵢ)/seᵢ)/seᵢ,则

Var_meas(p̄) = Σwᵢ²·(∂pᵢ/∂θᵢ)²·seᵢ² / (Σwᵢ)²

SE = √(Var_samp + Var_meas)。结果中三个分量
(se_sampling, se_measurement, se_total)都保留,便于报告。

扩展点:SE 引擎抽象为 `lst_variance_engine`,v2 增加
replicate weights(BRR/JK)引擎,v3 增加 PV/Rubin 引擎,
统计量代码不变。

### 6.1 replicate weights 引擎(v0.3,2026-07-18 实现)

在 `lst_data()` 声明 `rep_weights`(列名向量或前缀如
`"W_FSTR"`,自动展开为 W_FSTR1..R)与 `rep_method` 后,
抽样方差自动切换为:

Var_samp = c · Σ_r (θ̂⁽ʳ⁾ − θ̂)²

因子 c 按方法:fay = 1/(R(1−k)²)(PISA,k=0.5)、
brr = 1/R、jk1 = (R−1)/R、jk2 = 1(TIMSS)。测量方差分量
(个体 SE 传递)照旧独立相加,两套机制正交。全部 st_*
统计量(含题目正答率、分位数)统一走该路径;结果的
meta 记录 `variance = "replicate:<method>"`。

蒙特卡洛验证(50 群 × 20 人,ICC 型数据,test-rep-weights.R):
经验 SE 0.0722,JK1 引擎 0.0702(比值 0.97),线性化公式
0.0339——整群设计效应约 4 倍,验证了引擎的必要性与正确性。

### 6.2 plausible values / Rubin 引擎(v0.4,2026-07-18 实现)

`lst_data()` 声明 `pv = list(math = "PV#MATH")`(# 为编号占位,
自动展开为 PV1MATH..PVmMATH)后,该维度的所有统计量走
Rubin 合并:

- 点估计:est = mean_m(est_m)
- 插补方差:B = var_m(est_m),计入 se_measurement =
  √((1+1/M)B)
- 抽样方差 U:各 PV 的抽样方差(线性化,或声明了复制权重时
  用 replicate 法)按 `pv_sampling` 口径合并——`"first"`
  (PISA 手册常用,只算第 1 个 PV)或 `"average"`(全 PV 平均)
- 总方差 = U + (1+1/M)B;meta 记录
  `variance = "rubin+replicate:fay"` 等

约束:PV 维度不支持 `method = "prob"`(PV 本身已携带测量
不确定性,硬分类 + Rubin 即为正确做法,报错信息会说明);
pv 与 theta 维度名不得重复。

蒙特卡洛验证(test-pv.R):Rubin 总方差 95% 置信覆盖率 0.99
(略保守,符合 Rubin 方法特性);PV 硬分类的等级占比对潜在
分布校准(真值 0.1125,估计 0.1163);PV × replicate 组合
(PISA 完整方差)分量正交可加。

## 7. 透视层

核心 API,一次调用产出透视表:

```r
tab <- lst_table(d,
  rows   = c(region, gender),
  cols   = grade,
  values = list(
    均值   = st_mean(theta_math),
    优秀率 = st_prop_above(theta_math, cutoff = 1.2, method = "prob"),
    等级   = st_level_prop(theta_math, breaks = lv, method = "prob")),
  format = "est_se",     # est | est_se | est_ci | percent
  digits = 2,
  margins = TRUE)        # 追加"合计"行列
```

实现:先按 rows × cols 的全部组合分组计算 → 得到 tidy 长表
(每行 = 组合 × 统计量 × estimate/se)→ 再 pivot 成宽表。

`listr_table` 对象同时携带两种形态:

- `as_long(tab)`:tidy 长表(二次计算用)
- `as_wide(tab)` / 默认打印:行列布局宽表(阅读用)

单元格显示形式由 format 控制,如 `"512.3 (2.1)"`、
`"51.2% [48.9, 53.5]"`;digits 可按统计量分别指定。

## 8. 输出层

- `print()` / `as_wide()`:控制台与数据框
- `lst_to_excel(tab, path)`:openxlsx 导出,含合并表头、
  数字格式、SE 括号样式;多个表可写入同一工作簿的多个 sheet
- 内置中文默认样式:默认字体等线(fallback 微软雅黑),
  列宽按中文字符实际宽度(全角计 2)自动估算,UTF-8 全程
  保持;西文用户可通过 `style` 参数覆盖字体
- `lst_to_json(tab)`:面向 agent 的机器可读输出——tidy 长表
  数组 + 元数据(每个统计量的方法、公式名、n、权重和、SE
  分量)+ interpretation 字段
- `lst_interpret(tab)`:模板化规则生成自动解读文字(§1.1),
  规则集:组间显著差异(>2SE)、最高/最低组、等级占比要点、
  小样本警告(n < 30 的单元格标注"结果不稳定")
- v2:gt/flextable HTML 渲染、rmarkdown 成品报告模板

## 9. 包结构与依赖

```text
R/
  listr-package.R   包文档
  spec.R            lst_data(), 角色声明与校验
  read.R            read_listr() 及格式分派
  read-irt.R        Winsteps/ConQuest 解析器
  classify.R        lst_classify(), lst_above(), lst_derive()
  stats.R           st_* 统计量
  stats-item.R      题目层统计 st_pvalue(), st_option_dist()
  variance.R        误差传递引擎(含 unshrink)
  table.R           lst_table(), listr_table 对象
  pivot.R           长表↔宽表
  config.R          配置读取/校验(YAML/JSON/Excel 配置簿)
  run.R             lst_run() 一键入口
  interpret.R       lst_interpret() 自动解读文字
  export-excel.R    lst_to_excel()
  export-json.R     lst_to_json()
  utils.R
tests/testthat/
vignettes/
inst/COPYRIGHTS
inst/llms.txt               面向 LLM 的 API 文档
inst/schema/config.schema.json
inst/templates/config-template.xlsx  调查人员配置模板
```

Imports:haven, readxl, openxlsx, rlang, tibble, stats,
jsonlite, yaml, data.table。
核心聚合用 data.table 实现(百万级性能要求,见 §9.1);
不依赖 dplyr。Suggests:testthat, knitr, readr。

### 9.1 性能与内存设计(2026-07-18 新增)

目标:500 万人 × 多维分组的全套统计表,在 16GB 普通办公
电脑上分钟内完成;100 万人常规任务秒级。

- 聚合后端:data.table(`by =` 分组 + 引用语义避免拷贝)。
  所有 st_* 统计量实现为对 (x, w, se) 向量的纯函数,由
  data.table 分组调用,天然向量化(pnorm/dnorm 一次算全列)
- 导入:csv/tsv 用 `data.table::fread`;sav/dta 用 haven
  (100 万行无压力);xlsx 超过 50 万行给出提示建议转 csv
- 按需加载列:`read_listr` 支持 `col_select`,`lst_run` 从
  配置推断实际用到的列,只读这些列(作答列常有数百个,
  不用就不进内存)
- 作答统计分块:st_option_dist 按题分块(每次 melt 一批题),
  峰值内存与题数解耦;100 万人 × 200 题不会出现一次性
  2 亿行的长表
- 一次分组多个统计量共享同一趟扫描;margins 复用
  data.table 的 grouping sets(`cube`/`groupingsets`)
- 内存纪律:不复制原数据框(setDT 就地转换需用户同意,
  否则 shallow copy);中间结果及时释放;文档标注每个
  操作的内存峰值量级
- 扩展点:聚合层封装为内部接口,v3 视需求加 duckdb/arrow
  后端应对千万级以上(与 replicate weights 引擎同期评估)

实测(2026-07-18,Apple Silicon,scripts/benchmark.R):

| 任务(50 作答列) | 100 万人 | 500 万人 |
| --- | --- | --- |
| lst_data 角色+校验 | 0.00s | 0.02s |
| 均值+prob占比+等级 10x2+margins | 2.0s | 10.5s |
| 三维分组 10x2x6 | 0.3s | 1.0s |
| 50 题正答率 x 10 组 | 1.6s | 7.7s |
| latent 校正(含 rho 估计) | 0.3s | 1.1s |

500 万人全套表 10.5s,远低于 5 分钟验收线;§9.1 目标达成。

## 10. 测试计划

- 加权公式对照 `stats::weighted.mean` 与手算值
- SE 对照蒙特卡洛模拟(抽样 + 测量两层模拟)
- 概率化等级占比:se→0 时收敛于硬分类结果
- haven 标签数据往返测试;Winsteps PFILE 固定样例文件
- lst_table 长/宽表一致性;Excel 导出可重新读回
- 性能基准:合成 100 万/500 万人数据集(含 200 作答列),
  bench 记录典型任务耗时与峰值内存,回归门槛进 CI;
  16GB 环境验收:500 万人全套表 < 5 分钟、峰值 < 8GB

## 11. 路线图

- v0.1(2026-07-18 完成):spec + 通用导入 +
  st_mean/st_prop_above/st_level_prop + st_pvalue/st_option_dist +
  误差传递(latent 校正)+ lst_table + 中文样式 Excel 导出 +
  lst_run(YAML/JSON 配置)+ lst_to_json + llms.txt/schema
- v0.2(2026-07-18 实现,待验收):Excel 配置模板簿
  (lst_config_template + xlsx 解析)、Winsteps/ConQuest 解析器、
  分位数 Woodruff SE、lst_interpret 等级/题目规则、
  vignette(LISTR-intro)、性能基准脚本(scripts/benchmark.R)
- v0.3(2026-07-18 实现,待验收):replicate weights 方差引擎
  (fay/brr/jk1/jk2,前缀展开,配置层与 Excel 模板同步);
  option_dist 的 replicate SE 留待 v0.3.x
- v0.4(2026-07-18 实现,待验收):plausible values/Rubin 引擎
  (PV#模板展开、first/average 口径、与 replicate 组合)、
  零依赖 lst_to_html() 报告渲染(中文样式+自动解读+方法脚注)、
  配置层/Schema/Excel 模板同步(能力维度 sheet 增 PV 列,
  输出增 HTML)
- v1.0:CRAN 提交

## 12. 已决问题(2026-07-18)

- 估计类型校正:数值实验证明 EAP+后验 SD 无需校正,
  WLE/ML 需要 `correction = "latent"` EB 收缩(§4.1);
  原"反收缩"设想已被实验推翻并修订。
- 作答统计:st_pvalue / st_option_dist 纳入 v0.1(§5)。
- Excel 中文:内置中文默认样式,style 参数可覆盖(§8)。
- 三类用户:配置模板 + lst_run 服务新手与 agent,完整函数
  API 服务统计人员;agent 另配 llms.txt 与 JSON Schema;
  非专业报告用模板化规则自动解读,不依赖 LLM(§1.1)。
  Shiny 界面与 MCP server 明确排除出本包,留待独立仓库。
- 规模:百万级(至 500 万)为设计目标,16GB 办公电脑可跑;
  聚合后端定为 data.table,千万级以上留给 v3 的
  duckdb/arrow 后端(§9.1)。
