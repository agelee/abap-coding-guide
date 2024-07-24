 要通过从另一个内表复制数据并过滤掉不满足 WHERE 条件的行来创建一个新的内表，可以使用 FILTER 操作符。

- FILTER 操作符根据指定的类型（可以是显式指定的非泛型表类型或在第一个括号前用作操作数类型符号的 # 字符）构建内表。
- 新内表的行是根据 WHERE 子句中指定的条件从现有内表中获取的。请注意，现有内表的表类型必须可以转换为指定的目标类型。
- 条件可以基于单个值或过滤表。
### 示例代码
```abap
" 定义一个结构类型
TYPES: BEGIN OF ty_line,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 10,
       END OF ty_line.

" 定义内表类型
TYPES ty_line_tab TYPE TABLE OF ty_line WITH EMPTY KEY.

" 创建并填充内表
DATA(itab) = VALUE ty_line_tab(
               ( comp1 = 1 comp2 = 'A' )
               ( comp1 = 2 comp2 = 'B' )
               ( comp1 = 3 comp2 = 'C' )
               ( comp1 = 4 comp2 = 'D' )
               ( comp1 = 5 comp2 = 'E' )
             ).

" 使用 FILTER 操作符创建一个新内表，过滤出 comp1 大于 2 的行
DATA(filtered_itab) = FILTER ty_line_tab(
                        itab USING KEY primary_key WHERE comp1 > 2
                      ).

" 显示结果
cl_demo_output=>display( filtered_itab ).
```
### 解释

1. **定义结构类型**：定义一个结构类型 `ty_line`，包含两个字段 `comp1` 和 `comp2`。
2. **定义内表类型**：定义一个表类型 `ty_line_tab`。
3. **创建并填充内表**：使用 `VALUE` 操作符创建并填充内表 `itab`。
4. **使用 **`FILTER`** 操作符**：使用 `FILTER` 操作符从 `itab` 中复制数据到 `filtered_itab`，同时过滤出 `comp1` 大于 2 的行。`USING KEY primary_key` 指定使用主键进行过滤。
5. **显示结果**：使用 `cl_demo_output=>display` 显示过滤后的内表 `filtered_itab` 的内容。
### 结果
运行此代码后，`filtered_itab` 将包含从 `itab` 中过滤出来的行，这些行满足 `comp1 > 2` 的条件。因此，`filtered_itab` 的内容将是：

- `( comp1 = 3, comp2 = 'C' )`
- `( comp1 = 4, comp2 = 'D' )`
- `( comp1 = 5, comp2 = 'E' )`

![image.png](https://cdn.nlark.com/yuque/0/2024/png/488204/1721114134323-a8b3c023-626f-4140-bfff-181a6dc84d42.png#averageHue=%23fbfbfb&clientId=ube21f1f3-c7c2-4&from=paste&height=217&id=u4250095e&originHeight=271&originWidth=452&originalType=binary&ratio=1.25&rotation=0&showTitle=false&size=4197&status=done&style=none&taskId=u1a7c5fd4-7570-4e41-9675-86702ce9731&title=&width=361.6)
ABAP 中的 `FILTER` 操作符非常强大，可以根据特定条件从内部表中选择或排除条目。使用 `USING KEY` 可以指定用于过滤操作的键，`EXCEPT` 允许排除符合条件的条目。
此外，还可以使用过滤表来匹配另一个表中的条目。这为在 ABAP 中操作和查询内部表提供了灵活的方式。 

| **操作符** | **说明** |
| --- | --- |
| `USING KEY` | 指定用于评估 `WHERE` 条件的表键：可以是排序键或哈希键。如果内表没有这些键之一，则必须具有次表键，并且必须在 `USING KEY` 之后指定。 |
| `EXCEPT` | 指定 `EXCEPT` 表示使用现有表中不符合 `WHERE` 子句中指定条件的行。如果未指定 `EXCEPT`，则使用现有表中符合条件的行。 |

### 示例代码
以下是使用 `FILTER` 操作符的示例代码，展示如何使用 `USING KEY` 和 `EXCEPT` 选项进行过滤操作。
```abap
REPORT z_filter_demo.

TYPES: BEGIN OF struc,
         num TYPE i,
         data TYPE string,
       END OF struc.

DATA: itab1 TYPE SORTED TABLE OF struc WITH NON-UNIQUE KEY num,
      itab2 TYPE STANDARD TABLE OF struc WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS num,
      itab3 TYPE HASHED TABLE OF struc WITH UNIQUE KEY num.

DATA: filter_tab TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.

INITIALIZATION.
  APPEND VALUE #( num = 1 data = 'A' ) TO itab1.
  APPEND VALUE #( num = 2 data = 'B' ) TO itab1.
  APPEND VALUE #( num = 3 data = 'C' ) TO itab1.
  APPEND VALUE #( num = 4 data = 'D' ) TO itab1.
  APPEND VALUE #( num = 5 data = 'E' ) TO itab1.

  itab2 = itab1.
  itab3 = itab1.

  APPEND 3 TO filter_tab.
  APPEND 5 TO filter_tab.

DATA(f1) = FILTER #( itab1 WHERE num >= 3 ).
DATA(f2) = FILTER #( itab1 EXCEPT WHERE num >= 3 ).
DATA(f3) = FILTER #( itab2 USING KEY sec_key WHERE num >= 4 ).
DATA(f4) = FILTER #( itab3 WHERE num = 3 ).
DATA(f5) = FILTER #( itab1 IN filter_tab WHERE num = table_line ).

START-OF-SELECTION.

  cl_demo_output=>new(
    )->write( 'itab1:'
    )->write( itab1
    )->write( 'f1: 过滤 num >= 3' )->write( f1
    )->write( 'f2: 排除 num >= 3' )->write( f2
    )->write( 'f3: 使用次键过滤 num >= 4' )->write( f3
    )->write( 'f4: 使用哈希键过滤 num = 3' )->write( f4
    )->write( 'f5: 使用过滤表' )->write( f5
    )->display( ).
```
### 示例代码解释
#### 数据声明
```abap
DATA: itab1 TYPE SORTED TABLE OF struc WITH NON-UNIQUE KEY num,
      itab2 TYPE STANDARD TABLE OF struc WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS num,
      itab3 TYPE HASHED TABLE OF struc WITH UNIQUE KEY num.

DATA: filter_tab TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.
```
#### 填充示例数据
```abap
INITIALIZATION.
  APPEND VALUE #( num = 1 data = 'A' ) TO itab1.
  APPEND VALUE #( num = 2 data = 'B' ) TO itab1.
  APPEND VALUE #( num = 3 data = 'C' ) TO itab1.
  APPEND VALUE #( num = 4 data = 'D' ) TO itab1.
  APPEND VALUE #( num = 5 data = 'E' ) TO itab1.

  itab2 = itab1.
  itab3 = itab1.

  APPEND 3 TO filter_tab.
  APPEND 5 TO filter_tab.
```
#### 过滤示例
```abap
DATA(f1) = FILTER #( itab1 WHERE num >= 3 ).
DATA(f2) = FILTER #( itab1 EXCEPT WHERE num >= 3 ).
DATA(f3) = FILTER #( itab2 USING KEY sec_key WHERE num >= 4 ).
DATA(f4) = FILTER #( itab3 WHERE num = 3 ).
DATA(f5) = FILTER #( itab1 IN filter_tab WHERE num = table_line ).
```

- `f1` 过滤出 `num >= 3` 的记录。
- `f2` 排除 `num >= 3` 的记录。
- `f3` 使用次键过滤出 `num >= 4` 的记录。
- `f4` 过滤出 `num = 3` 的记录（针对哈希表）。
- `f5` 使用过滤表 `filter_tab` 过滤出 `num` 在过滤表中的记录。
#### 输出结果
```abap
START-OF-SELECTION.

  cl_demo_output=>new()
    ->write( 'itab1:' )->write( itab1 )
    ->write( 'f1: 过滤 num >= 3' )->write( f1 )
    ->write( 'f2: 排除 num >= 3' )->write( f2 )
    ->write( 'f3: 使用次键过滤 num >= 4' )->write( f3 )
    ->write( 'f4: 使用哈希键过滤 num = 3' )->write( f4 )
    ->write( 'f5: 使用过滤表' )->write( f5 )
    ->display( ).
```
### 运行结果
运行上述代码后，输出将显示：

- `itab1`: 原始数据集。
- `f1`: 过滤条件 `num >= 3` 后的数据。
- `f2`: 排除条件 `num >= 3` 后的数据。
- `f3`: 使用次键过滤 `num >= 4` 后的数据。
- `f4`: 使用哈希键过滤 `num = 3` 后的数据。
- `f5`: 使用过滤表 `filter_tab` 后的数据。

![image.png](https://cdn.nlark.com/yuque/0/2024/png/488204/1721203494831-c887b56b-9c40-4411-ab25-d9fa254d5e24.png#averageHue=%23fcfcfc&clientId=u2e414c41-71b2-4&from=paste&height=454&id=ub6a97ea4&originHeight=568&originWidth=598&originalType=binary&ratio=1.25&rotation=0&showTitle=false&size=13867&status=done&style=none&taskId=u3cba72ae-2790-4e30-b131-f539babc9fa&title=&width=478.4)
![image.png](https://cdn.nlark.com/yuque/0/2024/png/488204/1721203511867-6e1b5cd0-4d69-4ea1-a547-feadf398bd68.png#averageHue=%23fcfbfb&clientId=u2e414c41-71b2-4&from=paste&height=379&id=ue5d6bff2&originHeight=474&originWidth=547&originalType=binary&ratio=1.25&rotation=0&showTitle=false&size=18680&status=done&style=none&taskId=uad9ace6b-4813-491d-a4b8-140d2dc0cb9&title=&width=437.6)
这个示例展示了如何使用 `FILTER` 操作符在 ABAP 中进行复杂的数据过滤操作。


